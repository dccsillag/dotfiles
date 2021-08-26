#!/usr/bin/env python3

from typing import Tuple, List, Dict
import re
import os
import subprocess
import time


HEIGHT = 30
FONT = "FantasqueSansMono Nerd Font-12"
FONTWIDTH = 9
BACKGROUND_COLOR = "#121212"
FOREGROUND_COLOR = "#CCE0EA"

MODULES_LEFT = [
    "cpu",
    "mem",
    "swap",
    "temp",
]
MODULES_CENTER = [
    "datetime",
]
MODULES_RIGHT = [
    "donotdisturb",
    "screenshare",
    "vpn",
    "music",
    "mic",
    "wifi",
    "battery",
]

def get_module_exec(module_name: str) -> str:
    script_dir = os.path.dirname(os.path.realpath(__file__))
    return os.path.join(script_dir, "modules", module_name + ".sh")

def spawn_process(argv: List[str]) -> subprocess.Popen:
    process = subprocess.Popen(argv, stdout=subprocess.PIPE)
    os.set_blocking(process.stdout.fileno(), False) # NOTE: only works on Unix-like :)
    return process

def init_modules(modules: List[str]) -> Dict[str, Tuple[str, subprocess.Popen]]:
    return {module: ("…", spawn_process([get_module_exec(module)]))
            for module in modules}

modules_left   = init_modules(MODULES_LEFT)
modules_center = init_modules(MODULES_CENTER)
modules_right  = init_modules(MODULES_RIGHT)

def textwidth(text: str):
    without_escapes = re.sub(r"\^[a-z]+\([^)]*\)", "", text)
    return len(without_escapes)*FONTWIDTH

def join_modules(modules: Dict[str, Tuple[str, subprocess.Popen]]) -> str:
    return SEP.join(text for _, (text, _) in modules.items() if textwidth(text) > 0)


SEP = " ^fg() "

dzen_process = subprocess.Popen(
    [
        "dzen2",
        "-xs", "0",
        "-x", "0",
        "-y", "0",
        "-h", str(HEIGHT),
        "-fn", FONT,
        "-dock",
        "-bg", BACKGROUND_COLOR,
        "-fg", FOREGROUND_COLOR,
    ],
    stdin=subprocess.PIPE
)

while True:
    for modules in (modules_left, modules_center, modules_right):
        for module, (_, process) in modules.items():
            if process.poll() is not None:
                modules[module] = "KILLED: " + module, process
                continue

            new_content = process.stdout.readline().decode("utf-8")
            if not new_content:
                continue
            new_content = new_content.rstrip().split("\n")[-1]

            modules[module] = new_content, process

    left_text   = join_modules(modules_left)
    center_text = join_modules(modules_center)
    right_text  = join_modules(modules_right)

    text_to_show = ""
    text_to_show += "^p(_LEFT)^fg()"
    text_to_show += left_text
    text_to_show += "^p(_CENTER)^p(-%d)^fg()" % (textwidth(center_text)//2)
    text_to_show += center_text
    text_to_show += "^p(_RIGHT)^p(-%d)^fg()" % textwidth(right_text)
    text_to_show += right_text

    dzen_process.stdin.write((text_to_show + "\n").encode("utf-8"))
    dzen_process.stdin.flush()

    time.sleep(0.1)
