module XMonad.Csillag.Externals where

-- Terminal Emulator

termSpawn :: String
termSpawn = "alacritty"

termSpawn' :: String -> String
termSpawn' cls = "alacritty --class '" ++ cls ++ "," ++ cls ++ "'"

termRun :: String -> String
termRun cmd = "alacritty -e sh -c '" ++ cmd ++ "'"

termRun' :: String -> String -> String
termRun' cls cmd = "alacritty --class '" ++ cls ++ "," ++ cls ++ "' -e sh -c '" ++ cmd ++ "'"

-- Text Editor

texteditorSpawn :: String
texteditorSpawn = "neovide --multigrid"

-- File Manager

filemanagerSpawn :: String
filemanagerSpawn = termRun "lf" -- "nnn -nA"

-- Browser

browserSpawn :: String
browserSpawn = "brave --new-window"

browserSpawnPrivate :: String
browserSpawnPrivate = "brave --new-window --incognito"

browserOpen :: String -> String
browserOpen url = "brave --new-window '" ++ url ++ "'"

-- Camera Viewer

camviewSpawn :: String
camviewSpawn = "mpv --demuxer-lavf-format=video4linux2 --demuxer-lavf-o-set=input_format=mjpeg av://v4l2:/dev/video0 --no-cache --untimed --no-demuxer-thread || notify-send -u critical 'MPV - failed' 'failed to open camera'"

-- Screenshot Tool

scrotSaveAndCopyToClipboard :: String
-- scrotSaveAndCopyToClipboard = " -e 'xclip -selection clipboard -target image/png -i $f'"
scrotSaveAndCopyToClipboard = " | tee ~/media/screenshots/\"$(date +'%Y-%m-%d-%H%M%S.png')\" | xclip -selection clipboard -target image/png"

scrotScreen :: String
scrotScreen = "maim" ++ scrotSaveAndCopyToClipboard

scrotWindow :: String
scrotWindow = "maim --select" ++ scrotSaveAndCopyToClipboard

scrotRegion :: String
scrotRegion = "maim --select" ++ scrotSaveAndCopyToClipboard

scrotThiswindow :: String
scrotThiswindow = "maim --window $(xdotool getactivewindow)" ++ scrotSaveAndCopyToClipboard

-- Compositor

compositorSpawn :: String
compositorSpawn = "picom --experimental-backends"

-- compositor_spawn = "sh -c 'picom --glx-fshader-win \"$(cat ~/.config/picom-opacity-shader.glsl)\"'"

compositorRestart :: String
compositorRestart = "pkill -USR1 picom"

compositorKill :: String
compositorKill = "pkill picom"

-- Screen Configuration

setScreenOrientation :: String -> Int -> String
setScreenOrientation orient screen = "sh -c 'xrandr --output \"" ++ screen_name ++ "\" --rotate " ++ orient ++ "'; xinput --map-to-output 'ELAN2513:00 04F3:23EF' \"" ++ screen_name ++ "\""
  where
    screen_name = "$(xrandr -q | grep \" connected\" | cut -d\\  -f 1 | sed \"" ++ show (succ screen) ++ "q;d\")"

-- System Monitor

systemMonitor :: String
systemMonitor = "btm --battery"

-- Calculator

calculatorSpawn :: String
calculatorSpawn = termRun "insect"
