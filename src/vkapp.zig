const std = @import("std");
const Allocator = std.mem.Allocator;
const vk = @import("vulkan.zig");
const c = @import("c.zig");
const Dispatch = @import("dispatch.zig");

const max_concurrent_frames = 3;
const init_width = 800;
const init_height = 600;

const required_device_extensions = [_][*:0]const u8{
    vk.extension_info.khr_swapchain.name,
};

const PerFrame = struct {
    present_complete_semaphore: vk.Semaphore,
    render_complete_semaphore: vk.Semaphore,
    wait_fences: vk.Fence,
    command_buffer: vk.CommandBuffer,
};

pub const VkApp = struct {
    app_name: [*:0]const u8 = "Vulkan Zig",
    allocator: Allocator = undefined,
    window: *c.GLFWwindow = undefined,
    dispatch: Dispatch = undefined,
    per_frame: [max_concurrent_frames]PerFrame = undefined,
    frame_buffer_resized: bool = true,
    surface: vk.SurfaceKHR = .null_handle,
    instance: vk.Instance = vk.Instance.null_handle,
    debugMessenger: vk.DebugUtilsMessengerEXT = .null_handle,
    pdev: vk.PhysicalDevice = .null_handle,
    graphics_family: ?u32 = null,
    present_family: ?u32 = null,

    // Dispatch tables
    vkb: Dispatch.BaseDispatch = undefined,
    vki: Dispatch.InstanceDispatch = undefined,
    vkd: Dispatch.DeviceDispatch = undefined,

    fn frameBufferResizeCallback(window: ?*c.GLFWwindow, width: c_int, height: c_int) callconv(.C) void {
        _ = width;
        _ = height;

        const app: *align(@alignOf(*VkApp)) *VkApp = @alignCast(@ptrCast(c.glfwGetWindowUserPointer(window).?));
        app.*.frame_buffer_resized = true;
    }

    fn initWindow(self: *VkApp) !void {
        _ = c.glfwInit();
        c.glfwWindowHint(c.GLFW_CLIENT_API, c.GLFW_NO_API);
        self.*.window = c.glfwCreateWindow(init_width, init_height, "Vulkan", null, null).?;
        c.glfwSetWindowUserPointer(self.window, self);
        // c.glfwSetFramebufferSizeCallback(self.window, @constCast(@ptrCast(&frameBufferResizeCallback)));
        _ = c.glfwSetFramebufferSizeCallback(self.window, &frameBufferResizeCallback);
    }

    fn getRequiredExtensions(self: *VkApp) !std.ArrayList([*c]const u8) {
        var glfw_exts_count: u32 = 0;
        const glfw_exts = c.glfwGetRequiredInstanceExtensions(&glfw_exts_count);
        var extensions = std.ArrayList([*c]const u8).init(self.allocator);
        for (0..glfw_exts_count) |i| {
            try extensions.append(glfw_exts[i]);
        }
        try extensions.append(vk.extension_info.ext_debug_utils.name);
        return extensions;
    }

    fn createInstance(self: *VkApp) !void {
        if (c.glfwVulkanSupported() != c.GLFW_TRUE) {
            std.log.err("GLFW could not find libvulkan", .{});
            return error.NoVulkan;
        }
        self.vkb = try Dispatch.BaseDispatch.load(c.glfwGetInstanceProcAddress);

        const exts = try getRequiredExtensions(self);
        defer exts.deinit();

        const app_info = vk.ApplicationInfo{
            .p_application_name = @as(?[*:0]const u8, @ptrCast(self.app_name)),
            .application_version = vk.makeApiVersion(0, 0, 0, 0),
            .p_engine_name = self.app_name,
            .engine_version = vk.makeApiVersion(0, 0, 0, 0),
            .api_version = vk.API_VERSION_1_2,
        };

        self.instance = try self.vkb.createInstance(&.{
            .p_application_info = &app_info,
            .enabled_extension_count = @intCast(exts.items.len),
            .pp_enabled_extension_names = @as([*]const [*:0]const u8, @ptrCast(exts.items.ptr)),
        }, null);

        self.vki = try Dispatch.InstanceDispatch.load(self.instance, self.vkb.dispatch.vkGetInstanceProcAddr);
    }

    fn debugCallback(severity: vk.DebugUtilsMessageSeverityFlagsEXT, message_type: vk.DebugUtilsMessageTypeFlagsEXT, callback_data: ?*const vk.DebugUtilsMessengerCallbackDataEXT, user_data: ?*anyopaque) callconv(.C) u32 {
        if (callback_data != null) {
            const msg = callback_data.?.p_message;
            std.debug.print("(debugCallback) validation layer: {s}\n", .{msg});
        }
        _ = severity; // autofix
        _ = message_type; // autofix
        _ = user_data; // autofix
        return vk.FALSE;
    }

    fn setupDebugMessenger(self: *VkApp) !void {
        const ci = vk.DebugUtilsMessengerCreateInfoEXT{ .message_severity = .{
            .verbose_bit_ext = true,
            .warning_bit_ext = true,
            .error_bit_ext = true,
        }, .message_type = .{
            .general_bit_ext = true,
            .validation_bit_ext = true,
            .performance_bit_ext = true,
        }, .pfn_user_callback = &debugCallback };

        self.debugMessenger = try self.vki.createDebugUtilsMessengerEXT(self.instance, &ci, null);
    }

    fn createSurface(self: *VkApp) !void {
        if (c.glfwCreateWindowSurface(self.instance, self.window, null, &self.surface) != .success) {
            return error.CreateSurfaceFailed;
        }
    }

    fn pickPhysicalDevice(self: *VkApp) !void {
        var device_count: u32 = undefined;
        _ = try self.vki.enumeratePhysicalDevices(self.instance, &device_count, null);

        const pdevs = try self.allocator.alloc(vk.PhysicalDevice, device_count);
        defer self.allocator.free(pdevs);

        _ = try self.vki.enumeratePhysicalDevices(self.instance, &device_count, pdevs.ptr);

        for (pdevs) |pdev| {
            if (try isDeviceSuitable(self, pdev)) {
                self.pdev = pdev;
                return;
            }
        }

        return error.NoSuitableDevice;
    }

    fn isDeviceSuitable(self: *VkApp, pdev: vk.PhysicalDevice) !bool {
        if (!try checkDeviceExtensionSupport(self, pdev)) {
            return false;
        }

        if (!try checkSurfaceSupport(self, pdev)) {
            return false;
        }

        try findQueueFamilies(self, pdev);
        if ((self.present_family == null) or (self.graphics_family == null)) {
            return false;
        }

        return true;
    }

    fn findQueueFamilies(self: *VkApp, pdev: vk.PhysicalDevice) !void {
        var family_count: u32 = undefined;
        self.vki.getPhysicalDeviceQueueFamilyProperties(
            pdev,
            &family_count,
            null,
        );

        const families = try self.allocator.alloc(vk.QueueFamilyProperties, family_count);
        defer self.allocator.free(families);
        self.vki.getPhysicalDeviceQueueFamilyProperties(
            pdev,
            &family_count,
            families.ptr,
        );

        for (families, 0..) |properties, i| {
            const family: u32 = @intCast(i);

            if (self.graphics_family == null and properties.queue_flags.graphics_bit) {
                self.graphics_family = family;
            }

            if (self.present_family == null and (try self.vki.getPhysicalDeviceSurfaceSupportKHR(
                pdev,
                family,
                self.surface,
            )) == vk.TRUE) {
                self.present_family = family;
            }
        }
    }

    fn checkSurfaceSupport(self: *VkApp, pdev: vk.PhysicalDevice) !bool {
        var format_count: u32 = undefined;
        _ = try self.vki.getPhysicalDeviceSurfaceFormatsKHR(
            pdev,
            self.surface,
            &format_count,
            null,
        );

        var present_mode_count: u32 = undefined;
        _ = try self.vki.getPhysicalDeviceSurfacePresentModesKHR(
            pdev,
            self.surface,
            &present_mode_count,
            null,
        );

        return format_count > 0 and present_mode_count > 0;
    }

    fn checkDeviceExtensionSupport(self: *VkApp, pdev: vk.PhysicalDevice) !bool {
        var count: u32 = undefined;
        _ = try self.vki.enumerateDeviceExtensionProperties(
            pdev,
            null,
            &count,
            null,
        );

        const propsv = try self.allocator.alloc(vk.ExtensionProperties, count);
        defer self.allocator.free(propsv);

        _ = try self.vki.enumerateDeviceExtensionProperties(
            pdev,
            null,
            &count,
            propsv.ptr,
        );

        for (required_device_extensions) |ext| {
            for (propsv) |props| {
                if (std.mem.eql(u8, std.mem.span(ext), std.mem.sliceTo(&props.extension_name, 0))) {
                    break;
                }
            } else {
                return false;
            }
        }

        return true;
    }

    pub fn initVulkan(self: *VkApp) !void {
        try createInstance(self);
        try setupDebugMessenger(self);
        try createSurface(self);
        try pickPhysicalDevice(self);
        // createLogicalDevice();
        // createSwapChain();
        // createImageViews();
        // createRenderPass();
        // createGraphicsPipeline();
        // createFramebuffers();
        // createCommandPool();
        // createCommandBuffers();
        // createSyncObjects();
    }

    fn drawFrame(self: *VkApp) void {
        _ = self;
    }

    fn mainLoop(self: *VkApp) !void {
        while (c.glfwWindowShouldClose(self.window) == 0) {
            c.glfwPollEvents();
            drawFrame(self);
        }
        // _ = try self.vkd.deviceWaitIdle(self.dev);
    }

    fn cleanup(self: *VkApp) void {
        _ = self;
    }

    pub fn run(self: *VkApp, allocator: Allocator) !void {
        self.allocator = allocator;
        try initWindow(self);
        try initVulkan(self);
        try mainLoop(self);
        cleanup(self);
    }
};
