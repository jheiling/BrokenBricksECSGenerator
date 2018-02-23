#load "Generator.fs"
open System.IO
open ECS.Generator



let folder = Path.Combine (__SOURCE_DIRECTORY__, "example")

Components.mkComponent folder "test" |> Components.writeFile

{ Components.mkComponent folder "test1" with 
    namespace' = Some "testNamespace"
    imports = ["System.Collections.Generic"]
    fields = [Components.mkField "field0" "float"; { Components.mkField "field1" "List<int>" with showInInspector = false }] 
    constructor = true
    wrapper = Some Components.defaultWrapper }
|> Components.writeFile

Systems.mkSystem folder "test" |> Systems.writeFile

{ Systems.mkSystem folder "test1" with
    namespace' = Some "testNamespace"
    components = [Systems.mkComponent "test" "testComponent"; Systems.mkComponent "test1" "test1Component"] }
|> Systems.writeFile