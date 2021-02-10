module XMonad.Csillag.Externals
    where

-- Terminal Emulator

term_spawn :: String
term_spawn = "st"

term_spawn' :: String -> String
term_spawn' cls = "st -c '" ++ cls ++ "'"

term_run :: String -> String
term_run cmd = "st -e sh -c '" ++ cmd ++ "'"

term_run' :: String -> String -> String
term_run' cls cmd = "st -c '" ++ cls ++ "' -e sh -c '" ++ cmd ++ "'"

-- Text Editor

texteditor_spawn :: String
texteditor_spawn = term_run "nvim"

texteditor_edit :: FilePath -> String
texteditor_edit path = term_run $ "nvim " ++ path

-- File Manager

filemanager_spawn :: String
filemanager_spawn = term_run "nnn -nA"

-- Browser

browser_spawn :: String
browser_spawn = "qutebrowser-quick"

browser_spawn_private :: String
browser_spawn_private = "qutebrowser-quick --private"

browser_open :: String -> String
browser_open url = "qutebrowser-quick '" ++ url ++ "'"

-- Camera Viewer

camview_spawn :: String
camview_spawn = "mpv --demuxer-lavf-format=video4linux2 --demuxer-lavf-o-set=input_format=mjpeg av://v4l2:/dev/video0 --no-cache --untimed --no-demuxer-thread || notify-send -u critical 'MPV - failed' 'failed to open camera'"

-- Screenshot Tool

scrot_screen :: String
scrot_screen = "cd ~/media/screenshots/ && scrot -z"

scrot_window :: String
scrot_window = "cd ~/media/screenshots/ && scrot -s -z"

scrot_thiswindow :: String
scrot_thiswindow = "cd ~/media/screenshots/ && scrot -u -z"

-- Compositor

compositor_spawn :: String
-- compositor_spawn = "picom --experimental-backend"
compositor_spawn = "sh -c 'picom --glx-fshader-win \"$(cat ~/.config/picom-opacity-shader.glsl)\"'"

compositor_restart :: String
compositor_restart = "pkill -USR1 picom"

compositor_kill :: String
compositor_kill = "pkill picom"

-- Screen Configuration

set_screen_orientation :: String -> Int -> String
set_screen_orientation orient screen = "sh -c 'xrandr --output \"" ++ screen_name ++ "\" --rotate " ++ orient ++ "'; xinput --map-to-output 'ELAN2513:00 04F3:23EF' \"" ++ screen_name ++ "\""
    where screen_name = "$(xrandr -q | grep \" connected\" | cut -d\\  -f 1 | sed \"" ++ show (succ screen) ++ "q;d\")"

-- System Monitor

system_monitor :: String
system_monitor = "btm --battery"

-- Calculator

calculator_spawn :: String
calculator_spawn = term_run "insect"
