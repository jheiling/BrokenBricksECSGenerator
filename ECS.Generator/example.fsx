#load "Generator.fs"
open System.IO
open ECS.Generator.Components



let folder = Path.Combine (__SOURCE_DIRECTORY__, "example")

mkComponent folder "Test" |> writeFile

{ mkComponent folder "Test1" with 
    namespace' = Some "TestNamespace"
    imports = ["System.Collections.Generic"]
    fields = [mkField "Field0" "float"; { mkField "Field1" "List<int>" with showInInspector = false }] 
    constructor = true
    wrapper = Some defaultWrapper }
|> writeFile