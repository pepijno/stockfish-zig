const std = @import("std");
const EnumField = std.builtin.Type.EnumField;

pub fn valuesFromFields(comptime E: type, comptime fields: []const EnumField) []const E {
    comptime {
        var result: [fields.len]E = undefined;
        for (fields) |f, i| {
            result[i] = @field(E, f.name);
        }
        return &result;
    }
}

pub fn values(comptime E: type) []const E {
    return comptime valuesFromFields(E, @typeInfo(E).Enum.fields);
}
