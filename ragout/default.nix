{stdenv, frituur, mosterd}:
stdenv.mkDerivation {
    name = "ragout";
    src = ./.;
    phases = ["unpackPhase" "buildPhase" "installPhase"];
    buildInputs = [frituur mosterd];
    buildPhase = ''
        cp '${mosterd}/configuration.dhall' 'build'
        frituur './build/translate.dhall'
    '';
    installPhase = ''
        mkdir -p "$out/lib"
        mv 'ragout.bitterbal' "$out/lib"
    '';
}
