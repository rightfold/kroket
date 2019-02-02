{
    target = {
        php = <Php = {=} | Native : {}>
    },
    command = {
        translate = \(objectPath : Text) -> \(sourcePaths : List Text) ->
            < Translate = {objectPath = objectPath, sourcePaths = sourcePaths}
            | Link : {executablePath : Text, objectPaths : List Text} >,

        link = \(executablePath : Text) -> \(objectPaths : List Text) ->
            < Link = {executablePath = executablePath, objectPaths = objectPaths}
            | Translate : {objectPath : Text, sourcePaths : List Text} >
    }
}
