const std = @import("std");
const ArrayList = std.ArrayList;
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
    dev: vk.Device = .null_handle,
    graphics_queue: vk.Queue = undefined,
    present_queue: vk.Queue = undefined,
    swap_chain_images: ArrayList(vk.Image) = undefined,
    swap_chain_image_format: vk.Format = undefined,
    swap_chain_extent: vk.Extent2D = undefined,

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

        const details = try querySwapchainSupport(self, pdev);
        defer details.deinit();

        if (details.formats.items.len == 0 or details.present_modes.items.len == 0) {
            return false;
        }

        const indices = try findQueueFamilies(self, pdev);
        return indices.isComplete();
    }

    const QueueFamilyIndices = struct {
        graphics_family: ?u32,
        present_family: ?u32,

        pub fn isComplete(self: QueueFamilyIndices) bool {
            return self.graphics_family != null and self.present_family != null;
        }
    };

    fn findQueueFamilies(self: *VkApp, pdev: vk.PhysicalDevice) !QueueFamilyIndices {
        var indices: QueueFamilyIndices = undefined;

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
            if (properties.queue_flags.graphics_bit) {
                indices.graphics_family = @as(u32, @intCast(i));
            }

            const present_support = try self.vki.getPhysicalDeviceSurfaceSupportKHR(
                pdev,
                family,
                self.surface,
            );
            if (present_support != 0) {
                indices.present_family = @intCast(i);
            }

            if (indices.isComplete()) {
                break;
            }
        }

        return indices;
    }

    const SwapChainSupportDetails = struct {
        capabilities: vk.SurfaceCapabilitiesKHR,
        formats: ArrayList(vk.SurfaceFormatKHR),
        present_modes: ArrayList(vk.PresentModeKHR),

        pub fn init(allocator: Allocator) !SwapChainSupportDetails {
            return .{
                .capabilities = undefined,
                .formats = ArrayList(vk.SurfaceFormatKHR).init(allocator),
                .present_modes = ArrayList(vk.PresentModeKHR).init(allocator),
            };
        }

        pub fn deinit(self: SwapChainSupportDetails) void {
            self.formats.deinit();
            self.present_modes.deinit();
        }
    };

    fn querySwapchainSupport(self: *VkApp, pdev: vk.PhysicalDevice) !SwapChainSupportDetails {
        var details = try SwapChainSupportDetails.init(self.allocator);
        errdefer details.deinit();

        details.capabilities = try self.vki.getPhysicalDeviceSurfaceCapabilitiesKHR(
            pdev,
            self.surface,
        );

        var format_count: u32 = undefined;
        _ = try self.vki.getPhysicalDeviceSurfaceFormatsKHR(
            pdev,
            self.surface,
            &format_count,
            null,
        );

        if (format_count != 0) {
            _ = try details.formats.resize(format_count);
            _ = try self.vki.getPhysicalDeviceSurfaceFormatsKHR(
                pdev,
                self.surface,
                &format_count,
                details.formats.items.ptr,
            );
        }

        var present_mode_count: u32 = undefined;
        _ = try self.vki.getPhysicalDeviceSurfacePresentModesKHR(
            pdev,
            self.surface,
            &present_mode_count,
            null,
        );
        if (present_mode_count != 0) {
            _ = try details.present_modes.resize(present_mode_count);
            _ = try self.vki.getPhysicalDeviceSurfacePresentModesKHR(
                pdev,
                self.surface,
                &present_mode_count,
                details.present_modes.items.ptr,
            );
        }

        return details;
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

    fn createLogicalDevice(self: *VkApp) !void {
        const indices = try self.findQueueFamilies(self.pdev);

        const priority = [_]f32{1};
        const qci = [_]vk.DeviceQueueCreateInfo{
            .{
                .queue_family_index = 0, //self.graphics_family_index.?,
                .queue_count = 1,
                .p_queue_priorities = &priority,
            },
            .{
                .queue_family_index = 0, //self.present_family_index.?,
                .queue_count = 1,
                .p_queue_priorities = &priority,
            },
        };

        const queue_count: u32 = if (indices.graphics_family.? == indices.present_family.?)
            1
        else
            2;

        self.dev = try self.vki.createDevice(
            self.pdev,
            &.{
                .queue_create_info_count = queue_count,
                .p_queue_create_infos = &qci,
                .enabled_extension_count = required_device_extensions.len,
                .pp_enabled_extension_names = @as(
                    [*]const [*:0]const u8,
                    @ptrCast(&required_device_extensions),
                ),
            },
            null,
        );

        self.vkd = try Dispatch.DeviceDispatch.load(self.dev, self.vki.dispatch.vkGetDeviceProcAddr);

        self.graphics_queue = self.vkd.getDeviceQueue(
            self.dev,
            indices.graphics_family.?,
            0,
        );
        self.present_queue = self.vkd.getDeviceQueue(
            self.dev,
            indices.present_family.?,
            0,
        );
    }

    fn chooseSurfaceFormat(available_formats: []vk.SurfaceFormatKHR) !vk.SurfaceFormatKHR {
        for (available_formats) |format| {
            if (format.format == .b8g8r8_srgb and format.color_space == .srgb_nonlinear_khr) {
                return format;
            }
        }
        return available_formats[0];
    }

    fn chooseSwapPresentMode(available_present_modes: []vk.PresentModeKHR) !vk.PresentModeKHR {
        for (available_present_modes) |mode| {
            if (mode == .mailbox_khr) {
                return mode;
            }
        }
        return .fifo_khr;
    }

    fn chooseSwapExtent(window: *c.GLFWwindow, capabilities: vk.SurfaceCapabilitiesKHR) !vk.Extent2D {
        if (capabilities.current_extent.width != std.math.maxInt(u32)) {
            return capabilities.current_extent;
        }
        var width: c_int = 0;
        var height: c_int = 0;
        c.glfwGetFramebufferSize(window, &width, &height);

        var actual_extent: vk.Extent2D = .{
            .width = @as(u32, @intCast(width)),
            .height = @as(u32, @intCast(height)),
        };
        actual_extent.width = std.math.clamp(
            actual_extent.width,
            capabilities.min_image_extent.width,
            capabilities.max_image_extent.width,
        );
        actual_extent.height = std.math.clamp(
            actual_extent.height,
            capabilities.min_image_extent.height,
            capabilities.max_image_extent.height,
        );
        return actual_extent;
    }

    fn createSwapChain(self: *VkApp) !void {
        const swap_chain_support = try self.querySwapchainSupport(self.pdev);
        defer swap_chain_support.deinit();

        const surface_format = try chooseSurfaceFormat(swap_chain_support.formats.items);
        const present_mode = try chooseSwapPresentMode(swap_chain_support.present_modes.items);
        const extent = try chooseSwapExtent(self.window, swap_chain_support.capabilities);
        var image_count = swap_chain_support.capabilities.min_image_count + 1;
        if (swap_chain_support.capabilities.max_image_count > 0 and image_count > swap_chain_support.capabilities.max_image_count) {
            image_count = swap_chain_support.capabilities.max_image_count;
        }

        var create_info = vk.SwapchainCreateInfoKHR{
            .surface = self.surface,
            .min_image_count = image_count,
            .image_format = surface_format.format,
            .image_color_space = surface_format.color_space,
            .image_extent = extent,
            .image_array_layers = 1,
            .image_usage = .{ .color_attachment_bit = true },
            .image_sharing_mode = undefined,
            .p_queue_family_indices = undefined,
            .pre_transform = swap_chain_support.capabilities.current_transform,
            .composite_alpha = .{ .opaque_bit_khr = true },
            .present_mode = present_mode,
            .clipped = vk.TRUE,
        };

        const indices = try self.findQueueFamilies(self.pdev);
        const queue_family_indices = [_]u32{ indices.graphics_family.?, indices.present_family.? };
        if (indices.graphics_family != indices.present_family) {
            create_info.image_sharing_mode = .concurrent;
            create_info.queue_family_index_count = 2;
            create_info.p_queue_family_indices = &queue_family_indices;
        } else {
            create_info.image_sharing_mode = .exclusive;
        }

        const swap_chain = try self.vkd.createSwapchainKHR(self.dev, &create_info, null);
        _ = try self.vkd.getSwapchainImagesKHR(self.dev, swap_chain, &image_count, null);
        self.swap_chain_images = ArrayList(vk.Image).init(self.allocator);
        _ = try self.swap_chain_images.resize(image_count);
        _ = try self.vkd.getSwapchainImagesKHR(
            self.dev,
            swap_chain,
            &image_count,
            self.swap_chain_images.items.ptr,
        );

        self.swap_chain_image_format = surface_format.format;
        self.swap_chain_extent = extent;
    }

    pub fn initVulkan(self: *VkApp) !void {
        try createInstance(self);
        try setupDebugMessenger(self);
        try createSurface(self);
        try pickPhysicalDevice(self);
        try createLogicalDevice(self);
        try createSwapChain(self);
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
