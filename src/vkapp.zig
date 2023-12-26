const std = @import("std");
const vk = @import("vulkan.zig");
const c = @import("c.zig");
const GraphicsContext = @import("graphics_context.zig").GraphicsContext;
const Dispatch = @import("dispatch.zig");

const MAX_CONCURRENT_FRAMES = 3;
const WIDTH = 800;
const HEIGHT = 600;

const PerFrame = struct {
    present_complete_semaphore: vk.Semaphore,
    render_complete_semaphore: vk.Semaphore,
    wait_fences: vk.Fence,
    command_buffer: vk.CommandBuffer,
};

pub const VkApp = struct {
    app_name: [*:0]const u8 = "Vulkan Zig",
    window: *c.GLFWwindow = undefined,
    gc: GraphicsContext = undefined,
    dispatch: Dispatch = undefined,
    per_frame: [MAX_CONCURRENT_FRAMES]PerFrame = undefined,
    frame_buffer_resized: bool = true,
    surface: vk.SurfaceKHR = .null_handle,
    instance: vk.Instance = vk.Instance.null_handle,
    debugMessenger: vk.DebugUtilsMessengerEXT = .null_handle,

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
        self.*.window = c.glfwCreateWindow(WIDTH, HEIGHT, "Vulkan", null, null).?;
        c.glfwSetWindowUserPointer(self.window, self);
        // c.glfwSetFramebufferSizeCallback(self.window, @constCast(@ptrCast(&frameBufferResizeCallback)));
        _ = c.glfwSetFramebufferSizeCallback(self.window, &frameBufferResizeCallback);
    }

    fn createInstance(self: *VkApp) !void {
        if (c.glfwVulkanSupported() != c.GLFW_TRUE) {
            std.log.err("GLFW could not find libvulkan", .{});
            return error.NoVulkan;
        }
        self.vkb = try Dispatch.BaseDispatch.load(c.glfwGetInstanceProcAddress);

        var glfw_exts_count: u32 = 0;
        const glfw_exts = c.glfwGetRequiredInstanceExtensions(&glfw_exts_count);

        const app_info = vk.ApplicationInfo{
            .p_application_name = @as(?[*:0]const u8, @ptrCast(self.app_name)),
            .application_version = vk.makeApiVersion(0, 0, 0, 0),
            .p_engine_name = self.app_name,
            .engine_version = vk.makeApiVersion(0, 0, 0, 0),
            .api_version = vk.API_VERSION_1_2,
        };

        self.instance = try self.vkb.createInstance(&.{
            .p_application_info = &app_info,
            .enabled_extension_count = glfw_exts_count,
            .pp_enabled_extension_names = @as([*]const [*:0]const u8, @ptrCast(glfw_exts)),
        }, null);
        errdefer self.vki.destroyInstance(self.instance, null);

        self.vki = try Dispatch.InstanceDispatch.load(self.instance, self.vkb.dispatch.vkGetInstanceProcAddr);
    }

    fn debugCallback(severity: vk.DebugUtilsMessageSeverityFlagsEXT, message_type: vk.DebugUtilsMessageTypeFlagsEXT, callback_data: ?*const vk.DebugUtilsMessengerCallbackDataEXT, user_data: ?*anyopaque) callconv(.C) u32 {
        if (callback_data != null) {
            const msg = callback_data.?.p_message;
            std.debug.print("(debugCallback) validation layer: {s}", .{msg});
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

    pub fn initVulkan(self: *VkApp) !void {
        try createInstance(self);
        try setupDebugMessenger(self);
        try createSurface(self);
        // pickPhysicalDevice();
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
        // _ = try self.gc.vkd.deviceWaitIdle(self.gc.dev);
    }

    fn cleanup(self: *VkApp) void {
        _ = self;
    }

    pub fn run(self: *VkApp) !void {
        try initWindow(self);
        try initVulkan(self);
        try mainLoop(self);
        cleanup(self);
    }
};
