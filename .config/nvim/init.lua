local function file_exists(path)
    local f = io.open(path, "r")
    if f ~= nil then
        io.close(f)
        return true
    else
        return false
    end
end

local function require_yuescript(module)
    local base_path = vim.fn.expand "~/.config/nvim/lua/csillag/"

    local lua_file = base_path .. module .. ".lua"
    local yue_file = base_path .. module .. ".yue"
    if not file_exists(lua_file) then -- TODO or if the yue file is newer
        print("`" .. lua_file .. "` not found, compiling it from `" .. yue_file .. "`")
        os.execute("yue " .. yue_file)
    end
    return require("csillag." .. module)
end

require_yuescript "plugins"
require_yuescript "config"
require_yuescript "misc"
require_yuescript "mappings"

-- TODO auto PackerCompile
