local function file_exists(path)
    local f = io.open(path, "r")
    if f ~= nil then
        io.close(f)
        return true
    else
        return false
    end
end

--- Get the last modification timestamp for the given file, given it exists.
local function get_file_timestamp(path)
    local f = io.popen("stat -c %Y " .. path)
    local timestamp = tonumber(f:read())
    f:close()

    return timestamp
end

RECOMPILED_SOMETHING = false
local function require_yuescript(module)
    local base_path = vim.fn.expand "~/.config/nvim/lua/csillag/"

    local lua_file = base_path .. module .. ".lua"
    local yue_file = base_path .. module .. ".yue"
    if not file_exists(lua_file) or get_file_timestamp(yue_file) > get_file_timestamp(lua_file) then
        print("Compiling `" .. yue_file .. "` into `" .. lua_file .. "`")
        os.execute("yue " .. yue_file)
        RECOMPILED_SOMETHING = true
    end
    return require("csillag." .. module)
end

require_yuescript "plugins"
require_yuescript "config"
require_yuescript "misc"
require_yuescript "mappings"

-- if RECOMPILED_SOMETHING then
--     print "Running :PackerCompile"
--     vim.cmd "PackerCompile"
-- end
