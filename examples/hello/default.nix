{stdenv, frituur, mosterd, ragout}:
stdenv.mkDerivation {
    name = "hello";
    src = ./.;
    buildInputs = [frituur ragout];
    phases = ["unpackPhase" "buildPhase" "installPhase"];
    buildPhase = ''
        cp '${mosterd}/configuration.dhall' 'build'
        cp '${ragout}/lib/ragout.bitterbal' .
        frituur './build/translate.dhall'
        frituur './build/link.dhall'
    '';
    installPhase = ''
        mkdir -p "$out/www"
        mv 'hello.php' "$out/www"
    '';
}
