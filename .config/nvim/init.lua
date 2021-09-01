local function file_exists(path)
    local f = io.open(path, "r")
    if f ~= nil then
        io.close(f)
        return true
    else
        return false
    end
end


local config_root = vim.fn.expand "~/.config/nvim/config"
local lua_config_path = config_root .. ".lua"
local yue_config_path = config_root .. ".yue"
if not file_exists(lua_config_path) then
    print("config.lua not found, compiling config.yue")
    os.execute("yue " .. yue_config_path)
end

dofile(lua_config_path)
