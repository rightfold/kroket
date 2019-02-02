{callPackage, frituur, mosterd, ragout}:
{
    hello = callPackage ./hello {
        frituur = frituur;
        mosterd = mosterd;
        ragout = ragout;
    };
}
