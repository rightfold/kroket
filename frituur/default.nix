{ mkDerivation
, aeson
, alex
, array
, base
, bytestring
, containers
, dhall
, happy
, hashable
, lens
, mtl
, text
, transformers
, unordered-containers }:
mkDerivation {
    pname = "frituur";
    version = "0.0.0.0";
    license = null;
    src = builtins.filterSource (p: t: p != toString ./dist) ./.;
    buildTools = [
        alex
        happy
    ];
    buildDepends = [
        aeson
        array
        base
        bytestring
        hashable
        containers
        dhall
        lens
        mtl
        text
        transformers
        unordered-containers
    ];
}
