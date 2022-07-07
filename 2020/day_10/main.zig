const std = @import("std");
const hash_map = std.hash_map;

const alloc: std.mem.Allocator = std.heap.page_allocator;
const stdout = std.io.getStdOut().writer();

fn readFile(filename: []const u8) !std.ArrayList(u32)
{
    var array = std.ArrayList(u32).init(std.heap.page_allocator);
    try array.append(0);
    
    var file = try std.fs.cwd().openFile(filename, .{});
    defer file.close();
    
    var reader = std.io.bufferedReader(file.reader()).reader();
    
    var max: u32 = 0;
    
    const max_bytes_per_line = 10;
    while (try reader.readUntilDelimiterOrEofAlloc(alloc, '\n', max_bytes_per_line)) |line| 
    {
        defer alloc.free(line);
        var num = try std.fmt.parseInt(u32, line, 10); // use line
        max  = std.math.max(max, num);
        
        try array.append(num);
    }

    try array.append(max + 3);
    
    return array;
} 

fn cmpU32(context: void, a: u32, b: u32) bool 
{
    return std.sort.asc(u32)(context, a, b);
}


fn partOne(array: []u32) u32 
{
    var len = array.len;
    var i: u32 = 1;
    
    var diffs = [3]u32{0, 0, 0};
    
    while(i < len)
    {
        var index = array[i] - array[i-1];
        diffs[index - 1] += 1;
        i += 1;
    }
    return diffs[0] * diffs[2];
}

fn _partTwo(array: []u32, idx: u32, target: u32, map: *hash_map.AutoHashMap(u32, u64)) u64
{
    var sum: u64 = 0;
    var i = idx;

    if(i >= array.len)
    {
        return 1;
    }

    while(i < array.len and (array[i] - target) <= 3)
    {
        // Wanted to use getOrPut here, but it segfaulted.
        // Idk if it was by bad, but it did 🤷‍♀️
        if(map.get(i)) |val|
        {
            sum += val;
        }
        else
        {
            var val = _partTwo(array, i + 1, array[i], map);
            map.put(i, val) catch { std.os.exit(0); };
            sum += val;
        }
        i += 1;
    }
    
    return sum;
}

fn partTwo(array: []u32) u64
{
    var idx: u32 = 1;
    var target: u32 = 0;
    var map = std.hash_map.AutoHashMap(u32, u64).init(alloc);
    defer map.deinit();

    return _partTwo(array, idx, target, &map);
}

pub fn main() !void {
    var array = try readFile("input");
    defer array.deinit();
    
    var list = array.toOwnedSlice();
    std.sort.sort(u32, list, {}, cmpU32);
    
    try stdout.print("{}\n", .{partOne(list)});
    try stdout.print("{}\n", .{partTwo(list)});
}