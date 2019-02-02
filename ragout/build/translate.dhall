let lib = ./configuration.dhall in
{
    target = lib.target.php,
    command = lib.command.translate "ragout.bitterbal" [
        "src/abstract/category.kroket",
        "src/coerce.kroket",
        "src/hazards.kroket",
        "src/io.kroket",
        "src/panic.kroket",
        "src/text/utf8.kroket"
    ]
}
