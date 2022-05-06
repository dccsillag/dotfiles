# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

let
  allowUnfreePredicate = pkg: builtins.elem (pkgs.lib.getName pkg) [
    "slack"
    "discord"
    "write_stylus"
    "zoom"
  ];

  unstable = import <nixos-unstable> { config.allowUnfreePredicate = allowUnfreePredicate; };
in {
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  nixpkgs.overlays = [
    (import (builtins.fetchTarball {
      url = https://github.com/nix-community/neovim-nightly-overlay/archive/master.tar.gz;
    }))
  ];

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.kernelPackages = pkgs.linuxPackages_latest;

  # Setup a swapfile
  swapDevices = [
    { device = "/swapfile"; }
  ];

  networking.hostName = "lanczos"; # Define your hostname.
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.
  networking.networkmanager.enable = true;

  environment.etc.hosts.mode = "0644"; # make /etc/hosts editable by root for vpn-slice

  hardware.bluetooth.enable = true;
  services.blueman.enable = true;

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

  # Enable the X11 windowing system.
  services.xserver.enable = true;
  services.xserver.windowManager.xmonad.enable = true;

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
    nssmdns = true;
  };

  # Enable sound.
  sound.enable = true;
  hardware.pulseaudio.enable = true;

  # Enable touchpad support (enabled default in most desktopManager).
  services.xserver.libinput.enable = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.daniel = {
    isNormalUser = true;
    extraGroups = [ "wheel" "networkmanager" ]; # Enable ‘sudo’ for the user.
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
        rev = "b933fbfce34e9afc798e6882a96586b496ac432f";
        sha256 = "0bkzalbbdrx09qgv6lk3ynxlknypw99vy7lk2i2rrlpl5x4c597p";
        fetchSubmodules = true;
      };

      installPhase = ''mkdir -p $out/bin && make install DESTDIR=$out INSTALL_PREFIX= SHELL=${bash}/bin/bash'';
    };

    streambinder-vpnc = stdenv.mkDerivation rec {
      name = "vpnc";
      version = "0.5.3";
      src = fetchFromGitHub {
        owner = "streambinder";
        repo = "vpnc";
        rev = "c8bb5371b881f8853f191c495e762f834c9def5d";
        sha256 = "1j1p83nfc2fpwczjcggsby0b44hk97ky0s6vns6md3awlbpgdn57";
        fetchSubmodules = true;
      };

      buildInputs = [ pkg-config perl libgcrypt gnutls ];

      postPatch = ''patchShebangs src/makeman.pl'';

      makeFlags = [
        "PREFIX=$(out)"
        "ETCDIR=$(out)/etc/vpnc"
        "SCRIPT_PATH=$(out)/etc/vpnc/vpnc-script"
      ];
    };
  in [
    # Text editor
    vim
    neovim-nightly
    yuescript

    # LSPs
    rust-analyzer
    clang-tools # this provides clangd
    lldb
    sumneko-lua-language-server
    pyright
    python39Packages.python-lsp-server
    haskell-language-server
    texlab
    # TODO vimls
    rnix-lsp

    # Download tools
    wget
    curl
    youtube-dl
    yt-dlp
    git # ... and git

    # Misc linux utils
    lshw
    pciutils
    usbutils
    lsof
    file

    # Misc tools
    ripgrep
    fd
    procs
    exa
    delta
    bat
    viu
    tokei
    highlight
    tldr
    fzf
    entr
    jq
    neofetch
    parallel
    sshfs
    gpp
    unstable.taskell

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
    cargo-asm

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

    # Pandoc
    pandoc
    # TODO pandoc-citeproc
    # TODO pandoc-crossref

    # ZSH
    starship
    direnv

    # VPN
    streambinder-vpnc
    vpnc-scripts
    vpn-slice

    # Desktop
    xterm
    alacritty
    unstable.neovide
    stack
    eww
    dzen2
    dunst
    python3
    scrot
    feh
    gnome.zenity
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
    brightnessctl
    pamixer
    screenkey
    xlayoutdisplay
    scrot

    # Password manager
    pass
    pinentry
    pinentry-gnome

    # GUI Programs
    luakit
    unstable.qutebrowser
    brave
    mpv
    libreoffice
    arandr
    pavucontrol
    sxiv
    zathura
    xournalpp
    write_stylus
    slack
    unstable.discord
    mailspring
  ];

  fonts.fonts = with pkgs; [
    nerdfonts
  ];

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

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;

  # Enable the keyring for Mailspring
  services.gnome.gnome-keyring.enable = true;

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  nixpkgs.config.allowUnfreePredicate = allowUnfreePredicate;

  # Upgrade automatically once a day:
  system.autoUpgrade.enable = true;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "21.11"; # Did you read the comment?

}
