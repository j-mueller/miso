{ nixpkgs ? (import <nixpkgs> {}).fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";
    rev = "45ffca6ff84ca5e00842900802af577dcfb3e84f";
    sha256 = "11vnmlix4xkifrlpz4a13r6dnncrwnjibnd2j5sl7zb9vklj40lc";
  }
} :
let
  pkgs = import nixpkgs {
    overlays = [(self: super: {
      chromedriver = self.callPackage ./chromedriver.nix { gconf = self.gnome2.GConf; };
      selenium-server-standalone = self.callPackage ./selenium-server.nix {};
    })];
  };
  ghcjs = pkgs.haskell.packages.ghcjsHEAD;
  ghc = pkgs.haskell.packages.ghc802;
  miso-ghc = ghc.callPackage ./miso-ghc.nix {};
  miso-ghcjs = ghcjs.callPackage ./miso-ghcjs.nix {};
in {
  test = import "${nixpkgs}/nixos/tests/make-test.nix" {
    name = "foobar";
    machine = { config, ... }: {
      environment.systemPackages = [ pkgs.selenium-server-standalone pkgs.google-chrome ];
      virtualisation.memorySize = 4096;
      virtualisation.cores = 2;
      users.users = {
        selenium = {
          isNormalUser = true;
        };
      };
      services.xserver.displayManager.auto.user = "selenium";
      systemd.services.selenium = {
        serviceConfig = {
          ExecStart = "${pkgs.selenium-server-standalone}/bin/selenium-server";
          User = "selenium";
        };
        environment.DISPLAY = ":0.0";
        wantedBy = [ "default.target" ];
      };
      systemd.services.selenium.enable = true;
      imports = [ "${nixpkgs}/nixos/tests/common/x11.nix" ];
      nixpkgs.config.allowUnfree = true;
    };
    testScript = ''
      $machine->waitForX;
      $machine->waitForUnit("selenium.service");
      $machine->execute('ln -s $(which google-chrome-stable) /usr/bin/google-chrome');
      my ($status, $out) = $machine->execute("${miso-ghc}/bin/selenium-server ${miso-ghcjs}/bin/selenium-client.jsexe");
      print STDERR $out;
      die "miso test suite failed!\n" if $status != 0;
    '';
  };
  inherit miso-ghc;
}
