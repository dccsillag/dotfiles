# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

let
  allowUnfreePredicate = pkg: builtins.elem (pkgs.lib.getName pkg) [
    "slack"
    "discord"
    "zoom"
    "steam"
    "steam-original"
    "steam-unwrapped"
    "steam-run"
    "android-studio-stable"
    "snes9x-gtk"

    "nvidia-x11"
    "nvidia-settings"
    "corefonts"

    "open-webui"
  ];

  unstable = import <nixos-unstable> { config.allowUnfreePredicate = allowUnfreePredicate; };

  pkgs2311 = import (builtins.fetchTarball "https://github.com/NixOS/nixpkgs/archive/nixos-23.11.tar.gz") {
    system = pkgs.stdenv.hostPlatform.system;
  };

  # 1. The Nody Greeter Derivation
  nodyGreeter = pkgs.buildNpmPackage rec {
    pname = "nody-greeter";
    version = "1.6.2"; # Latest stable release

    # Force Node 18 because Node 22's V8 engine breaks the node-gtk C++ bindings
    nodejs = pkgs2311.nodejs_18;

    # Tell NPM scripts not to bypass Nix and download external binaries
    ELECTRON_SKIP_BINARY_DOWNLOAD = "1";
    PUPPETEER_SKIP_DOWNLOAD = "1";

    src = pkgs.fetchFromGitHub {
      owner = "JezerM";
      repo = "nody-greeter";
      rev = version;
      fetchSubmodules = true;
      hash = "sha256-c9AANNLLKC5Rqb0BAJBr+sCCaJb/0cTa7VQgKKbayro=";
    };

    # Nix requires a hash for the entire node_modules dependency tree.
    # It will fail on the second run and give you this hash.
    npmDepsHash = "sha256-rKgXK9TyC1Rf4TUHTA0OQZ7FS8o2l3fF61H8+dEWC4o=";

    # Dependencies for building native Node modules (node-gtk)
    nativeBuildInputs = with pkgs; [
      pkg-config
      python311
      gobject-introspection
      vala
    ];

    buildInputs = with pkgs; [
      gtk3
      lightdm
      glib
      pkgs2311.electron
    ];

    # Override the default npm build phase to match Nody's instructions
    buildPhase = ''
      npm pkg set devDependencies.electron="${pkgs2311.electron.version}"

      npm run rebuild

      # Overwrite the broken theme script with a harmless 'echo' command.
      # This bypasses the submodule typo entirely since we just want Litarvan anyway.
      npm pkg set scripts.build:themes="echo 'Skipping broken default themes'"
      mkdir -p ./themes/themes/
      mkdir -p ./themes/themes/_vendor
      mkdir -p ./themes/_vendor
      mkdir -p ./node_modules/electron/dist/
      echo "dummy executable" > ./node_modules/electron/dist/electron

      npm run build
    '';

    installPhase = ''
      mkdir -p $out/bin $out/share/xgreeters $out/share/nody-greeter

      # 1. Copy the nested filesystem the packager created
      cp -r build/unpacked/* $out/share/nody-greeter/

      # 2. THE FIX: Dynamically find where the packager hid the app!
      # This searches the copied files for either 'resources/app' or 'resources/app.asar'
      APP_TARGET=$(find $out/share/nody-greeter | grep -E "resources/app$|resources/app.asar$" | head -n 1)

      # 3. Create the launcher pointing to the dynamically found path
      cat > $out/bin/nody-greeter <<EOF
      #!/bin/sh
      exec ${pkgs2311.electron}/bin/electron "$APP_TARGET" --no-sandbox --disable-gpu "\$@"
      EOF
      chmod +x $out/bin/nody-greeter

      # 4. Generate the desktop file
      cat > nody-greeter.desktop <<EOF
      [Desktop Entry]
      Name=nody-greeter
      Comment=LightDM greeter using web technologies
      Exec=$out/bin/nody-greeter
      Type=Application
      EOF

      # Put it in the standard location AND the root directory (your fix!)
      cp nody-greeter.desktop $out/share/xgreeters/
      cp nody-greeter.desktop $out/
    '';
    # installPhase = ''
    #   mkdir -p $out/bin $out/share/xgreeters $out/share/nody-greeter
    #   cp -r build/unpacked/* $out/share/nody-greeter/
    #
    #   # THE FIX: We let Nix correctly expand $out during the build,
    #   # but we escape the bash variables (\$APP_PATH and \$@) for runtime.
    #   cat > $out/bin/nody-greeter <<EOF
    #   #!/bin/sh
    #
    #   APP_PATH="$out/share/nody-greeter/resources/app.asar"
    #   if [ ! -f "\$APP_PATH" ]; then
    #     APP_PATH="$out/share/nody-greeter/resources/app"
    #   fi
    #
    #   exec ${pkgs.electron}/bin/electron "\$APP_PATH" --no-sandbox --disable-gpu "\$@"
    #   EOF
    #   chmod +x $out/bin/nody-greeter
    #
    #   # The desktop file also needs the unescaped $out
    #   cat > $out/share/xgreeters/nody-greeter.desktop <<EOF
    #   [Desktop Entry]
    #   Name=Nody Greeter
    #   Comment=LightDM greeter using web technologies
    #   Exec=$out/bin/nody-greeter
    #   Type=Application
    #   EOF
    #
    #   cp $out/share/xgreeters/nody-greeter.desktop $out/nody-greeter.desktop
    # '';
    # installPhase = ''
    #   mkdir -p $out/bin $out/share/xgreeters $out/share/nody-greeter
    #   cp -r build/unpacked/* $out/share/nody-greeter/
    #
    #   # THE FIX: Bulletproof the launcher, disable the sandbox, and add error logging!
    #   cat > $out/bin/nody-greeter <<EOF
    #   #!/bin/sh
    #
    #   # Determine if the app was packed into an asar archive or left as a directory
    #   APP_PATH="\$out/share/nody-greeter/resources/app.asar"
    #   if [ ! -f "\$APP_PATH" ]; then
    #     APP_PATH="\$out/share/nody-greeter/resources/app"
    #   fi
    #
    #   # Run system Electron with LightDM-safe flags and pipe all output to a log file
    #   exec ${pkgs.electron}/bin/electron "\$APP_PATH" --no-sandbox --disable-gpu "\\\$@" > /tmp/nody-greeter-crash.log 2>&1
    #   EOF
    #   chmod +x $out/bin/nody-greeter
    #
    #   cat > $out/share/xgreeters/nody-greeter.desktop <<EOF
    #   [Desktop Entry]
    #   Name=Nody Greeter
    #   Comment=LightDM greeter using web technologies
    #   Exec=$out/bin/nody-greeter
    #   Type=Application
    #   EOF
    # '';
    # installPhase = ''
    #   # Set up our system directories
    #   mkdir -p $out/bin $out/share/xgreeters $out/share/nody-greeter
    #
    #   # Copy the compiled Javascript/HTML resources over
    #   cp -r build/unpacked/* $out/share/nody-greeter/
    #
    #   # THE LAUNCHER FIX: Create a custom executable that ignores the dummy files
    #   # and instead forces the app to run using the native NixOS Electron engine.
    #   cat > $out/bin/nody-greeter <<EOF
    #   #!/bin/sh
    #   exec ${pkgs.electron}/bin/electron $out/share/nody-greeter/resources/app "\$@"
    #   EOF
    #   chmod +x $out/bin/nody-greeter
    #
    #   # Register the greeter with LightDM
    #   cat > $out/share/xgreeters/nody-greeter.desktop <<EOF
    #   [Desktop Entry]
    #   Name=Nody Greeter
    #   Comment=LightDM greeter using web technologies
    #   Exec=$out/bin/nody-greeter
    #   Type=Application
    #   EOF
    # '';
    # installPhase = ''
    #   mkdir -p $out/bin $out/share/xgreeters $out/etc/lightdm
    #   cp -r build/unpacked/* $out/
    #
    #   cat > $out/share/xgreeters/nody-greeter.desktop <<EOF
    #   [Desktop Entry]
    #   Name=Nody Greeter
    #   Comment=LightDM greeter using web technologies
    #   Exec=$out/bin/nody-greeter
    #   Type=Application
    #   EOF
    # '';
    # installPhase = ''
    #   # The default 'node make install' tries to write to root /usr and /etc
    #   # Instead, we manually grab the built output and put it in the Nix store
    #   mkdir -p $out/bin $out/share/xgreeters $out/etc/lightdm
    #
    #   # Copy the compiled electron app
    #   cp -r build/unpacked/* $out/
    #
    #   # Create the .desktop file so LightDM knows this greeter exists
    #   cat > $out/share/xgreeters/nody-greeter.desktop <<EOF
    #   [Desktop Entry]
    #   Name=Nody Greeter
    #   Comment=LightDM greeter using web technologies
    #   Exec=$out/bin/nody-greeter
    #   Type=Application
    #   EOF
    # '';
  };

  # 3. The Theme Derivation (Extracts the tarball natively)
  nodyThemeLitarvan = pkgs.stdenv.mkDerivation {
    name = "lightdm-theme-litarvan";
    src = pkgs.fetchurl {
      url = "https://github.com/Litarvan/lightdm-webkit-theme-litarvan/releases/download/v3.2.0/lightdm-webkit-theme-litarvan-3.2.0.tar.gz";
      hash = "sha256-lt0ujW5TbxtXHfbNBUtPlMVUvibxqSPvPHmMgLEyCwc=";
    };
    sourceRoot = ".";
    installPhase = ''
      mkdir -p $out
      cp -r * $out/
    '';
  };

  # Override llama-cpp to latest version with CUDA support
  llama-cpp =
    (pkgs.llama-cpp.override {
      cudaSupport = false;
      rocmSupport = false;
      metalSupport = false;
      # vulkanSupport = true;
      # Enable BLAS for optimized CPU layer performance (OpenBLAS)
      blasSupport = true;
    }).overrideAttrs
      (oldAttrs: rec {
        # version = "7205";
        version = "9433";
        src = pkgs.fetchFromGitHub {
          owner = "ggml-org";
          repo = "llama.cpp";
          tag = "b${version}";
          hash = "sha256-AKfjMx9SF4qyiSDRJqZN29fFJq7bMHpwguDuO6tQuf8=";
          leaveDotGit = true;
          postFetch = ''
            git -C "$out" rev-parse --short HEAD > $out/COMMIT
            find "$out" -name .git -print0 | xargs -0 rm -rf
          '';
        };
        # Enable native CPU optimizations (AVX, AVX2, etc.)
        cmakeFlags = (oldAttrs.cmakeFlags or []) ++ [
          "-DGGML_NATIVE=ON"
          "-DLLAMA_BUILD_EXAMPLES=ON"
        ];
        # nativeBuildInputs = (oldAttrs.nativeBuildInputs or []) ++ [
        #   pkgs.spirv-headers
        # ];
        # Disable Nix's march=native stripping
        preConfigure = ''
          export NIX_ENFORCE_NO_NATIVE=0
          ${oldAttrs.preConfigure or ""}
        '';
      });
in
{
  imports =
    [
      # Include the results of the hardware scan.
      ./hardware-configuration.nix
      # Include machine-local configuration.
      ./local-configuration.nix
    ];

  # nixpkgs.overlays = [
  #   (self: super: { nix-direnv = super.nix-direnv.override { enableFlakes = true; }; })
  # ];

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.systemd-boot.configurationLimit = 42;
  boot.loader.systemd-boot.memtest86.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  # boot.kernelPackages = pkgs.linuxPackages_6_0;
  boot.kernelPackages = pkgs.linuxPackages_latest;

  # "silent" boot:
  boot.plymouth = {
    enable = true;
    # theme = "spinner";  # solar
    # theme = "cuts_alt";
    # theme = "connect";
    # theme = "hexagon_dots";
    theme = "hexagon_dots_alt";
    themePackages = [ pkgs.adi1090x-plymouth-themes ];
    extraConfig = ''DeviceScale=1'';
  };
  boot.consoleLogLevel = 0;
  boot.initrd.verbose = false;
  boot.kernelParams = [ "quiet" "splash" "boot.shell_on_fail" "loglevel=3" "rd.systemd.show_status=false" "rd.udev.log_level=3" "udev.log_priority=3" ];
  boot.initrd.systemd.enable = true;

  # Setup a swapfile
  swapDevices = [
    { device = "/swapfile"; }
  ];

  networking.hostName = pkgs.lib.removeSuffix "\n" (pkgs.lib.readFile /etc/nixos/hostname);
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.
  networking.networkmanager.enable = true;

  environment.etc.hosts.mode = "0644"; # make /etc/hosts editable by root for vpn-slice

  # hardware.enableAllFirmware = true;
  # hardware.firmware = [ pkgs.linux-firmware ];
  # hardware.enableRedistributableFirmware = true;

  hardware.bluetooth.enable = true;
  services.blueman.enable = true;

  virtualisation.libvirtd = {
    enable = true;
    qemu = {
      package = pkgs.qemu_kvm;
      # runAsRoot = true;
      swtpm.enable = true;
    };
  };

  # # Set your time zone.
  # time.timeZone = "America/Sao_Paulo";
  services.automatic-timezoned.enable = true;

  # The global useDHCP flag is deprecated, therefore explicitly set to false here.
  # Per-interface useDHCP will be mandatory in the future, so this generated config
  # replicates the default behaviour.
  networking.useDHCP = false;

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Select internationalisation properties.
  # i18n.defaultLocale = "en_US.UTF-8";
  # console = {
  #   font = "Lat2-Terminus16";
  #   keyMap = "us";
  # };

  # Setup Vulkan
  hardware.graphics.enable = true;

  # # Setup Intel GPU
  # services.xserver.videoDrivers = [ "intel" ];
  # hardware.graphics.extraPackages = with pkgs; [
  #   vpl-gpu-rt
  #   libvdpau-va-gl
  #   intel-media-driver
  #   intel-compute-runtime
  # ];
  # hardware.graphics.extraPackages32 = with pkgs.pkgsi686Linux; [ intel-vaapi-driver ];

  # # Setup NVIDIA GPU
  # services.xserver.videoDrivers = [ "nvidia" ];
  # hardware.opengl = {
  #   enable = true;
  #   driSupport = true;
  #   driSupport32Bit = true;
  # };
  # hardware.nvidia = {
  #   package = config.boot.kernelPackages.nvidiaPackages.stable;
  #
  #   modesetting.enable = false;  # true;
  #   powerManagement.enable = false;
  #   powerManagement.finegrained = false;
  #   open = false;
  #   nvidiaSettings = true;
  #
  #   # For laptop:
  #   prime = {
  #     intelBusId = "PCI:0:2:0";  # pci@0000:00:02.0 ==> 00:02.0 ==> 0:2:0
  #     nvidiaBusId = "PCI:1:0:0";  # pci@0000:01:00.0 ==> 01:00.0 ==> 1:0:0
  #
  #     # sync.enable = true;
  #     reverseSync.enable = true;
  #     allowExternalGpu = false;
  #   };
  # };

  # Enable the X11 windowing system.
  services.xserver.enable = true;
  services.displayManager.ly = {
    enable = true;
    settings = {
      asterisk = "0x2022";
      # bigclock = "en";
      clear_password = true;
      margin_box_h = 4;
      margin_box_v = 1;
      blank_box = false;
      hide_borders = true;
      hide_version_string = true;
      brightness_up_key = null;
      brightness_down_key = null;
    };
  };
  # services.xserver.displayManager.lightdm = {
  #   enable = true;
  #   greeters.gtk.enable = false;
  #   greeter = {
  #     enable = true;
  #     name = "nody-greeter";
  #     package = nodyGreeter;
  #   };
  # };
  # environment.etc."lightdm/themes/litarvan".source = nodyThemeLitarvan;
  # environment.etc."lightdm/web-greeter.yml".text = ''
  #   greeter:
  #     theme: litarvan
  #   theme:
  #     dir: /etc/lightdm/themes
  # '';
  # services.xserver.desktopManager.gnome.enable = true;
  services.xserver.windowManager.xmonad = {
    enable = true;
    enableContribAndExtras = true;
    extraPackages = haskellPackages: with haskellPackages; [
      #containers_0_6_5_1
      # directory_1_3_7_1
      aeson
      utf8-string
      process_1_6_28_0
      xmobar
      bimap
      JuicyPixels
    ];
  };
  services.xserver.windowManager.awesome = {
    enable = true;
    luaModules = with pkgs.luaPackages; [
      vicious
    ];
  };
  # services.xserver.desktopManager.gnome.enable = true;

  # Configure keymap in X11
  # services.xserver.layout = "us";
  # services.xserver.xkbOptions = "eurosign:e";

  # Enable CUPS to print documents.
  services.printing = {
    enable = true;
    browsing = true;
    drivers = with pkgs; [ hplip ];
  };
  services.avahi = {
    enable = true;
    nssmdns4 = true;
  };

  # Enable sound.
  # sound.enable = true;
  # services.pulseaudio.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
    # wireplumber.extraConfig.bluetoothEnhancements = {
    #   "monitor.bluez.properties" = {
    #     "bluez5.enable-sbc-xq" = true;
    #     "bluez5.enable-msbc" = true;
    #     "bluez5.enable-hw-volume" = true;
    #     "bluez5.roles" = [ "hsp_hs" "hsp_ag" "hfp_hf" "hfp_ag" ];
    #   };
    # };
  };
  # hardware.bluetooth.settings = {
  #   General = {
  #     Enable = "Source,Sink,Media,Socket";
  #   };
  # };

  # Enable touchpad support (enabled default in most desktopManager).
  services.libinput.enable = true;
  services.xserver.wacom.enable = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.daniel = {
    isNormalUser = true;
    extraGroups = [ "wheel" "networkmanager" "libvirt" "libvirtd" "kvm" "qemu-libvirtd" ]; # Enable ‘sudo’ for the user.
    shell = pkgs.zsh;
  };

  programs.zsh.enable = true;

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; let
    yuescript = stdenv.mkDerivation rec {
      name = "yuescript";
      version = "0.9.5";
      src = fetchFromGitHub {
        owner = "pigpigyyy";
        repo = "Yuescript";
        rev = "738154d37dd4ec20b09acd0f9f81601d0dc069ba";
        sha256 = "XyNczRcNsKWYhcYZN84qqDuvO4O69syuQIR5gT4h68s=";
        fetchSubmodules = true;
      };

      installPhase = ''mkdir -p $out/bin && make install DESTDIR=$out INSTALL_PREFIX= SHELL=${bash}/bin/bash'';
    };

    my-zathura = stdenv.mkDerivation rec {
      name = "zathura";
      version = "nightly-2023-11-27";
      # FIXME new fetch
      src = fetchFromGitHub {
        owner = "pigpigyyy";
        repo = "Yuescript";
        rev = "738154d37dd4ec20b09acd0f9f81601d0dc069ba";
        sha256 = "XyNczRcNsKWYhcYZN84qqDuvO4O69syuQIR5gT4h68s=";
        fetchSubmodules = true;
      };

      installPhase = ''mkdir -p $out/bin && make install DESTDIR=$out INSTALL_PREFIX= SHELL=${bash}/bin/bash'';
    };

    # streambinder-vpnc = stdenv.mkDerivation rec {
    #   name = "vpnc";
    #   version = "0.5.3";
    #   src = fetchFromGitHub {
    #     owner = "streambinder";
    #     repo = "vpnc";
    #     rev = "c8bb5371b881f8853f191c495e762f834c9def5d";
    #     sha256 = "1j1p83nfc2fpwczjcggsby0b44hk97ky0s6vns6md3awlbpgdn57";
    #     fetchSubmodules = true;
    #   };
    #
    #   buildInputs = [ pkg-config perl libgcrypt gnutls ];
    #
    #   postPatch = ''patchShebangs src/makeman.pl'';
    #
    #   makeFlags = [
    #     "PREFIX=$(out)"
    #     "ETCDIR=$(out)/etc/vpnc"
    #     "SCRIPT_PATH=$(out)/etc/vpnc/vpnc-script"
    #   ];
    # };

    # my-eww = rustPlatform.buildRustPackage rec {
    #   pname = "eww";
    #   version = "0.3.0";
    #   src = fetchFromGitHub {
    #     owner = "elkowar";
    #     repo = pname;
    #     rev = "0b0715fd505200db5954432b8a27ed57e3e6a72a";
    #     sha256 = "sha256-wtrq8crcN7fdNAkCqKHrPpptP4FOEQwReUnSFcCMQzs=";
    #   };
    #   cargoSha256 = "sha256-3hGA730g8E4rwQ9V0wSLUcAEmockXi+spwp50cgf0Mw=";
    #   nativeBuildInputs = [ pkg-config ];
    #   buildInputs = [ gtk3 ] ++ lib.optional false gtk-layer-shell;
    #   buildNoDefaultFeatures = false;
    #   buildFeatures = lib.optional false "wayland";
    #   cargoBuildFlags = [ "--bin" "eww" ];
    #   cargoTestFlags = cargoBuildFlags;
    #   RUSTC_BOOTSTRAP = 1;
    # };
    neovim-nightly = (builtins.getFlake "github:neovim/neovim?dir=contrib").packages.x86_64-linux.default;
  in
  [
    linux-firmware

    # Text editor
    vim
    (unstable.neovim.override {
      withPython3 = true;
      extraPython3Packages = p: with p; [
        pynvim
        jupyter-client
        pillow
        cairosvg

        # python-lsp-server
        # pylsp-mypy
        # python-lsp-black
      ];
    })
    yuescript
    unstable.typst

    # LSPs
    rust-analyzer
    zls
    clang-tools # this provides clangd
    lldb
    lua-language-server
    pyright
    # python39Packages.python-lsp-server # already present way later
    haskell-language-server
    texlab
    # TODO vimls
    # rnix-lsp
    unstable.tinymist
    # unstable.aider-chat  # not quite an LSP, but...
    harper
    basedpyright
    typescript-language-server
    unstable.ty

    uv

    # Download tools
    wget
    curl
    #unstable.youtube-dl
    unstable.yt-dlp
    git # ... and git
    git-lfs
    gitoxide
    unstable.jujutsu
    unstable.jjui
    gh

    # Misc linux utils
    lshw
    pciutils
    usbutils
    lsof
    file
    nix-index
    comma
    socat
    nixos-shell

    # Misc tools
    ripgrep
    fd
    procs
    eza
    delta
    bat
    viu
    tokei
    highlight
    tldr
    fzf
    broot
    entr
    jq
    pfetch
    onefetch
    parallel
    sshfs
    rclone
    gpp
    # unstable.taskell
    hyperfine
    zoxide
    unstable.vhs
    # smartcat
    (llm.withPlugins {
      llm-anthropic = true;
      llm-gemini = true;
      llm-groq = true;
      llm-ollama = true;
      llm-jq = true;
    })
    unstable.opencode
    unstable.gemini-cli
    # llama-cpp
    unstable.runpodctl

    # Development tools
    rustfmt
    clippy
    cargo-edit
    cargo-audit
    cargo-bloat
    #cargo-crev
    cargo-criterion
    #cargo-cross
    cargo-deny
    cargo-expand
    cargo-fuzz
    cargo-license
    cargo-udeps
    cargo-valgrind
    cargo-watch
    cargo-flamegraph
    unstable.cargo-show-asm
    cargo-tarpaulin
    # cargo-llvm-cov
    cargo-nextest
    python3Packages.jupytext

    # Image tools
    imagemagick

    # Archive tools
    atool
    zip
    unzip
    bzip2
    gzip

    # System monitors
    htop
    bottom

    # File manager
    lf
    yazi

    # Calculator
    unstable.numbat

    # Pandoc
    pandoc
    # TODO pandoc-citeproc
    # TODO pandoc-crossref

    # ZSH
    starship
    direnv
    nix-direnv

    # # VPN
    # streambinder-vpnc
    # vpnc-scripts
    # vpn-slice
    unstable.proton-vpn-cli

    # Desktop
    # nodyGreeter
    pulseaudio
    unstable.picom
    xterm
    alacritty
    unstable.kitty
    neovide
    stack
    eww # my-eww
    dzen2
    polybar
    rofi
    rofi-pass
    dunst
    (python3.withPackages (ps: with ps; [
      pynvim
      jupyter-client
      pillow
      cairosvg

      # python-lsp-server
      rope
      # pylsp-mypy
      # python-lsp-ruff
      # python-lsp-black
    ]))
    ueberzugpp
    scrot
    maim
    feh
    zenity
    xmodmap
    xwininfo
    wmctrl
    xdotool
    xdo
    xcape
    xbindkeys
    xclip
    pulsemixer
    libnotify
    libsecret
    brightnessctl
    pamixer
    screenkey
    xlayoutdisplay
    mons
    gnome-boxes
    dnsmasq  # for VM networking
    bottles
    napari
    galaxy-buds-client

    # GTK themes
    arc-theme
    ayu-theme-gtk
    materia-theme

    # Password manager
    pass
    pinentry-gnome3
    pinentry-tty

    # GUI Programs
    luakit
    # unstable.qutebrowser
    brave
    firefox
    mpv
    libreoffice
    arandr
    pavucontrol
    unstable.nsxiv
    zathura
    sioyek
    xournalpp
    rnote
    slack
    unstable.discord
    unstable.mailspring
    gnome-calendar
    geary
    thunderbird
    # snes9x-gtk
    ryubing
    unstable.dolphin-emu
    blender

    # Remote access
    sunshine
  ];
  programs.slock.enable = true;

  programs.steam = {
    enable = true;
    remotePlay.openFirewall = true;
    dedicatedServer.openFirewall = true;
    localNetworkGameTransfers.openFirewall = true;
  };

  xdg.mime.defaultApplications = {
    "application/pdf" = "zathura.desktop";
    "image/png" = "nsxiv.desktop";
    "image/jpeg" = "nsxiv.desktop";
    "video/mp4" = "mpv.desktop";
  };

  # trace: warning: xdg-desktop-portal 1.17 reworked how portal implementations are loaded, you
  # should either set `xdg.portal.config` or `xdg.portal.configPackages`
  # to specify which portal backend to use for the requested interface.
  #
  # https://github.com/flatpak/xdg-desktop-portal/blob/1.18.1/doc/portals.conf.rst.in
  #
  # If you simply want to keep the behaviour in < 1.17, which uses the first
  # portal implementation found in lexicographical order, use the following:
  xdg.portal.config.common.default = "*";
  fonts.packages = with pkgs; [
    google-fonts
    corefonts
  ] ++ builtins.filter lib.attrsets.isDerivation (builtins.attrValues nerd-fonts);

  programs.firejail = {
    enable = true;
    wrappedBinaries = {
      zoom = {
        executable = "${unstable.zoom-us}/bin/zoom";
      };
    };
  };

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = true;
    #pinentryFlavor = "gnome3";
  };
  services.pcscd.enable = true;

  # List services that you want to enable:

  services.locate.enable = true;

  programs.fuse.userAllowOther = true;
  systemd.services.NetworkManager-wait-online.enable = false;
  # systemd.services.rcloneGDrive = {
  #   wantedBy = [ "default.target" ]; # [ "multi-user.target" ]
  #   after = [ "network-online.target" ];
  #   wants = [ "network-online.target" ];
  #   description = "rclone: Remote FUSE filesystem for cloud storage config mygoogledrive";
  #   serviceConfig = {
  #     Type = "notify";
  #     User = "daniel";
  #     ExecStartPre = ''-${pkgs.coreutils}/bin/mkdir -p /home/daniel/mnt/mygoogledrive'';
  #     # ExecStart = ''${pkgs.rclone}/bin/rclone mount --config=/home/daniel/.config/rclone/rclone.conf --vfs-cache-mode writes --vfs-cache-max-size 100M --log-level INFO --log-file /tmp/rclone-mygoogledrive.log --umask 022 --allow-other mygoogledrive: /home/daniel/mnt/mygoogledrive'';
  #     ExecStart = ''${pkgs.rclone}/bin/rclone mount --config=/home/daniel/.config/rclone/rclone.conf --vfs-cache-mode writes --vfs-cache-max-size 100M --log-level INFO --log-file /tmp/rclone-mygoogledrive.log --umask 022 --allow-other mygoogledrive: /home/daniel/mnt/mygoogledrive'';
  #     ExecStop = ''${pkgs.fuse}/bin/fusermount -u /home/daniel/mnt/mygoogledrive'';
  #     Restart = "always";
  #     RestartSec = "10s";
  #     Environment = [ "PATH=/run/wrappers/bin:$PATH" ];
  #   };
  # };
  security.wrappers = {
    fusermount.source = "${pkgs.fuse}/bin/fusermount";
  };

  # # Configure llama-swap as a systemd service
  # systemd.services.llama-swap = {
  #   description = "llama-swap - OpenAI compatible proxy with automatic model swapping";
  #   after = [ "network.target" ];
  #   wantedBy = [ "multi-user.target" ];
  #
  #   serviceConfig = {
  #     Type = "simple";
  #     User = "daniel";
  #     Group = "users";
  #     # Point to your declarative config file
  #     ExecStart = "${pkgs.llama-swap}/bin/llama-swap --config /etc/llama-swap/config.yaml --listen 0.0.0.0:9292 --watch-config";
  #     Restart = "always";
  #     RestartSec = 10;
  #
  #     # Environment for CUDA support
  #     Environment = [
  #       "PATH=/run/current-system/sw/bin"
  #       "LD_LIBRARY_PATH=/run/opengl-driver/lib:/run/opengl-driver-32/lib"
  #     ];
  #   };
  # };
  # services.llama-cpp = {
  #   package = llama-cpp;
  #   enable = true;
  #   # model = "/home/daniel/library/models/Gemma-4-E4B-It-7.5B-BF16.Q8_0.gguf";
  #   model = "/usr/models/Gemma-4-E4B-It-7.5B-BF16.Q8_0.gguf";
  #   # model = "/usr/models/Gemma-4-E4B-It-7.5B-BF16.Q4_K_M.gguf";
  #   host = "0.0.0.0";
  #   port = 9091;
  # };
  # services.open-webui = {
  #   enable = true;
  #   host = "0.0.0.0";
  #   port = 9090;
  #   environment = {
  #     "WEBUI_AUTH" = "False";
  #   };
  # };

  services.flatpak.enable = true;
  xdg.portal.enable = true;

  # Ollama:
  services.ollama.enable = true;

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;
  services.openssh.settings.X11Forwarding = true;
  # services.openssh.ports = [ 49106 ];
  programs.mosh.enable = true;

  services.tailscale.enable = true;
  networking.firewall.checkReversePath = "loose";

  # # Enable remote desktop
  # services.xrdp = {
  #   enable = true;
  #   defaultWindowManager = "xmonad";
  # };
  # services.x2goserver.enable = true;

  services.libinput = {
    mouse = {
      naturalScrolling = true;
    };
    touchpad = {
      naturalScrolling = true;
      accelProfile = "flat";
      # tappingButtonMap = "lrm";
    };
  };

  services.redshift = {
    enable = true;
    temperature.day = 6500;
    temperature.night = 4500;  # default is 4500
  };
  location.provider = "geoclue2";
  services.geoclue2 = {
    enable = true;
    # geoProviderUrl = "https://api.beacondb.net/v1/geolocate";
    geoProviderUrl = "https://www.googleapis.com/geolocation/v1/geolocate?key=YOUR_API_KEY_HERE";
  };
  services.xserver.displayManager.sessionCommands = ''
    # Start the Geoclue authorization agent for Redshift
    systemctl --user start geoclue-agent.service &
  '';

  # Enable the keyring for Mailspring
  services.gnome.gnome-keyring.enable = true;
  security.pam.services.login.enableGnomeKeyring = true;
  programs.seahorse.enable = true;
  services.xserver.updateDbusEnvironment = true;

  # For GNOME Calendar:
  programs.dconf.enable = true;
  services.gnome.evolution-data-server.enable = true;
  services.gnome.gnome-online-accounts.enable = true;

  # Open ports in the firewall.
  networking.firewall.allowedTCPPorts = [ 3389 8080 ];
  networking.firewall.allowedUDPPorts = [ 8080 ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # LocalSend
  programs.localsend.enable = true;

  # KDE Connect
  programs.kdeconnect.enable = true;

  # virt-manager
  programs.virt-manager.enable = true;

  # virtualisation.docker = {
  #   enable = true;
  #   rootless = {
  #     enable = true;
  #     setSocketVariable = true;
  #   };
  # };

  programs.nix-ld.enable = true;

  powerManagement.enable = true;
  services.thermald.enable = true;
  services.auto-cpufreq.enable = true;
  services.auto-cpufreq.settings = {
    battery = {
      governor = "powersave";
      turbo = "never";
    };
    charger = {
      governor = "powersave";
      turbo = "auto";
    };
  };

  nixpkgs.config.allowUnfreePredicate = allowUnfreePredicate;
  nix.settings.experimental-features = [ "nix-command" "flakes" ];

  # Upgrade automatically once a day:
  #system.autoUpgrade.enable = true;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "21.11"; # Did you read the comment?

}
