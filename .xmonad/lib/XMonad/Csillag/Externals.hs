module XMonad.Csillag.Externals
    where


-- Terminal Emulator

term_spawn :: String
-- term_spawn = "alacritty"
term_spawn = "st"

term_spawn' :: String -> String
-- term_spawn' cls = "alacritty --class '" ++ cls ++ "," ++ cls ++ "'"
-- term_spawn' cls = "st -c '" ++ cls ++ "," ++ cls ++ "'"
term_spawn' cls = "st -c '" ++ cls ++ "'"

term_run :: String -> String
term_run cmd = "st -e sh -c 'sleep 0.1 && " ++ cmd ++ "'"

term_run' :: String -> String -> String
-- term_run' cls cmd = "alacritty --class '" ++ cls ++ "," ++ cls ++ "' -e sh -c 'sleep 0.1 && " ++ cmd ++ "'"
term_run' cls cmd = "st -c '" ++ cls ++ "' -e sh -c 'sleep 0.1 && " ++ cmd ++ "'"

-- Text Editor

texteditor_spawn :: String
texteditor_spawn = term_run "nvim"

texteditor_edit :: FilePath -> String
texteditor_edit path = term_run $ "nvim " ++ path

-- File Manager

filemanager_spawn :: String
filemanager_spawn = term_run "nnn"

-- Browser

browser_spawn :: String
browser_spawn = "qutebrowser-quick"

browser_spawn_private :: String
browser_spawn_private = "brave --new-window --incognito"

browser_open :: String -> String
browser_open url = "qutebrowser-quick '" ++ url ++ "'"

-- Camera Viewer

camview_spawn :: String
camview_spawn = "mpv --demuxer-lavf-format=video4linux2 --demuxer-lavf-o-set=input_format=mjpeg av://v4l2:/dev/video0 || notify-send -u critical 'MPV - failed' 'failed to open camera'"

-- Screenshot Tool

scrot_screen :: String
scrot_screen = "cd ~/media/screenshots/ && scrot -z"

scrot_window :: String
scrot_window = "cd ~/media/screenshots/ && scrot -s -z"

scrot_thiswindow :: String
scrot_thiswindow = "cd ~/media/screenshots/ && scrot -u -z"

-- Compositor

compositor_spawn :: String
compositor_spawn = "picom --experimental-backend"

compositor_kill :: String
compositor_kill = "pkill picom"
