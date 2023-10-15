{
description = "fixme: trackerless issue managament";

inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    haskell-flake-utils.url = "github:ivanovs-4/haskell-flake-utils";
    suckless-conf.url = "github:voidlizard/suckless-conf";
    suckless-conf.inputs.nixpkgs.follows = "nixpkgs";
};

outputs = { self, nixpkgs, haskell-flake-utils, ... }@inputs:
    haskell-flake-utils.lib.simpleCabal2flake {
      inherit self nixpkgs;
      # systems = [ "x86_64-linux" ];

      name = "fixme";

      haskellFlakes = [ inputs.suckless-conf ];

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
