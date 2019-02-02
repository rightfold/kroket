let lib = ./configuration.dhall in
{
    target = lib.target.php,
    command = lib.command.link "hello.php" [
        "hello.bitterbal",
        "ragout.bitterbal"
    ]
}
