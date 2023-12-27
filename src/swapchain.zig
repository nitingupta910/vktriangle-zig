const std = @import("std");
const vk = @import("vulkan.zig");
const Allocator = std.mem.Allocator;

pub const Swapchain = struct {
    surface_format: vk.SurfaceFormatKHR,
    present_mode: vk.PresentModeKHR,
    extent: vk.Extent2D,
    handle: vk.SwapchainKHR,

    swap_images: []vk.Image,
};
