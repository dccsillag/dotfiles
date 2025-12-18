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
  ];

  unstable = import <nixos-unstable> { config.allowUnfreePredicate = allowUnfreePredicate; };
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
  boot.loader.efi.canTouchEfiVariables = true;
  # boot.kernelPackages = pkgs.linuxPackages_6_0;
  boot.kernelPackages = pkgs.linuxPackages_latest;
  # boot.kernelParams = [ "i915.force_probe=64a0" ];

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

  virtualisation.libvirtd.enable = true;

  # Set your time zone.
  time.timeZone = "America/Sao_Paulo";

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
  # services.xserver.displayManager.defaultSession = "none+xmonad";
  services.xserver.displayManager.gdm.enable = true;
  services.xserver.displayManager.gdm.autoSuspend = false;
  # services.xserver.desktopManager.gnome.enable = true;
  services.xserver.windowManager.xmonad = {
    enable = true;
    enableContribAndExtras = true;
    extraPackages = haskellPackages: with haskellPackages; [
      #containers_0_6_5_1
      # directory_1_3_7_1
      aeson
      utf8-string
      process_1_6_26_0
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
    extraGroups = [ "wheel" "networkmanager" "libvirt" ]; # Enable ‘sudo’ for the user.
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
        jupyter_client
        pillow
        cairosvg

        python-lsp-server
        pylsp-mypy
        python-lsp-black
      ];
    })
    yuescript
    unstable.typst

    # LSPs
    rust-analyzer
    zls
    clang-tools # this provides clangd
    lldb
    sumneko-lua-language-server
    pyright
    # python39Packages.python-lsp-server # already present way later
    haskell-language-server
    texlab
    # TODO vimls
    # rnix-lsp
    unstable.tinymist
    unstable.aider-chat  # not quite an LSP, but...
    harper
    basedpyright
    nodePackages.typescript-language-server
    unstable.ty

    uv

    # Download tools
    wget
    curl
    #unstable.youtube-dl
    unstable.yt-dlp
    git # ... and git
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
    difftastic
    bat
    viu
    tokei
    highlight
    tldr
    fzf
    broot
    entr
    jq
    neofetch
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
    cargo-show-asm
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

    # Desktop
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
      jupyter_client
      pillow
      cairosvg

      python-lsp-server
      rope
      pylsp-mypy
      # python-lsp-ruff
      python-lsp-black
    ]))
    ueberzugpp
    scrot
    maim
    feh
    zenity
    xorg.xmodmap
    xorg.xwininfo
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
    bottles
    napari
    galaxy-buds-client

    # GTK themes
    arc-theme
    ayu-theme-gtk
    materia-theme

    # Password manager
    pass
    pinentry
    pinentry-gnome3
    pinentry-tty

    # GUI Programs
    luakit
    # unstable.qutebrowser
    brave
    mpv
    libreoffice
    arandr
    pavucontrol
    unstable.nsxiv
    zathura
    xournalpp
    rnote
    slack
    unstable.discord
    mailspring  # unstable.mailspring
    gnome-calendar
    geary
    thunderbird
    snes9x-gtk
    ryujinx
    unstable.dolphin-emu

    # Remote access
    sunshine
  ];

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
    };
  };

  # Enable the keyring for Mailspring
  services.gnome.gnome-keyring.enable = true;

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

  # virtualisation.docker = {
  #   enable = true;
  #   rootless = {
  #     enable = true;
  #     setSocketVariable = true;
  #   };
  # };

  powerManagement.enable = true;
  services.thermald.enable = true;
  services.auto-cpufreq.enable = true;
  services.auto-cpufreq.settings = {
    battery = {
      governor = "powersave";
      turbo = "never";
    };
    charger = {
      governor = "performance";
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
