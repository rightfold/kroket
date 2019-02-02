{stdenv}:
stdenv.mkDerivation {
    name = "mosterd";
    src = ./.;
    phases = ["unpackPhase" "installPhase"];
    installPhase = ''
        mkdir "$out"
        cp 'configuration.dhall' "$out"
    '';
}
