const std = @import("std");
const vk = @import("vulkan.zig");
const c = @import("c.zig");
const shaders = @import("shaders");
const Swapchain = @import("swapchain.zig").Swapchain;
const VkApp = @import("vkapp.zig").VkApp;
const Allocator = std.mem.Allocator;

const app_name = "vulkan-zig triangle example";

pub fn main() !void {
    var app = VkApp{};
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    try app.run(allocator);
}
