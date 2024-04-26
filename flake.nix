{
description = "fixme: trackerless issue managament";

inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    hbs2.url = "git+http://git.hbs2/BTThPdHKF8XnEq4m6wzbKHKA6geLFK4ydYhBXAqBdHSP?ref=0.24.1";
    hbs2.inputs.nixpkgs.follows = "nixpkgs";
    haskell-flake-utils.url = "github:ivanovs-4/haskell-flake-utils";

    suckless-conf.url = "git+https://git.hbs2.net/JAuk1UJzZfbDGKVazSQU5yYQ3NGfk4gVeZzBCduf5TgQ";
    suckless-conf.inputs.nixpkgs.follows = "nixpkgs";

    db-pipe.url = "git+https://git.hbs2.net/5xrwbTzzweS9yeJQnrrUY9gQJfhJf84pbyHhF2MMmSft?ref=generic-sql";
    db-pipe.inputs.nixpkgs.follows = "nixpkgs";
};

outputs = { self, nixpkgs, haskell-flake-utils, ... }@inputs:
    haskell-flake-utils.lib.simpleCabal2flake {
      inherit self nixpkgs;
      # systems = [ "x86_64-linux" ];

      name = "fixme";

      haskellFlakes = with inputs; [ suckless-conf hbs2 db-pipe ];

      packagePostOverrides = { pkgs }: with pkgs; with haskell.lib; [
       disableExecutableProfiling
       disableLibraryProfiling
       dontBenchmark
       dontCoverage
       dontDistribute
       dontHaddock
       dontHyperlinkSource
       doStrip
       enableDeadCodeElimination
       justStaticExecutables

       dontCheck
      ];

      # Additional build intputs of the default shell
      shellExtBuildInputs = {pkgs}: with pkgs; [
        haskellPackages.haskell-language-server
      ];

      # Wether to build hoogle in the default shell
      # shellWithHoogle = true;

    };
}
