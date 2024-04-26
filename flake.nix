{
description = "fixme: trackerless issue managament";

inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    hbs2.url = "git+http://git.hbs2/BTThPdHKF8XnEq4m6wzbKHKA6geLFK4ydYhBXAqBdHSP?ref=0.24.1";
    hbs2.inputs.nixpkgs.follows = "nixpkgs";
    haskell-flake-utils.url = "github:ivanovs-4/haskell-flake-utils";
    suckless-conf.url = "github:voidlizard/suckless-conf";
    suckless-conf.inputs.nixpkgs.follows = "nixpkgs";
};

outputs = { self, nixpkgs, haskell-flake-utils, ... }@inputs:
    haskell-flake-utils.lib.simpleCabal2flake {
      inherit self nixpkgs;
      # systems = [ "x86_64-linux" ];

      name = "fixme";

      haskellFlakes = [ inputs.suckless-conf inputs.hbs2 ];

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
