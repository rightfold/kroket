{pkgs ? import ./nix/pkgs.nix {}}:
rec {
    examples = pkgs.callPackage ./examples {
        frituur = frituur;
        mosterd = mosterd;
        ragout = ragout;
    };
    frituur = pkgs.haskellPackages.callPackage ./frituur {};
    mosterd = pkgs.callPackage ./mosterd {};
    ragout = pkgs.callPackage ./ragout {
        frituur = frituur;
        mosterd = mosterd;
    };
}
