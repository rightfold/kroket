let lib = ./configuration.dhall in
{
    target = lib.target.php,
    command = lib.command.translate "hello.bitterbal" [
        "src/hello.kroket"
    ]
}
