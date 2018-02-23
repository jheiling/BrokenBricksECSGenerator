module ECS.Generator

open System.IO



let addTab = sprintf "    %s"

let import = sprintf "using %s;"
let imports xs = List.map import xs @ if List.isEmpty xs then [] else [""; ""; ""]

let block inner = "{" :: (List.map addTab) inner @ ["}"]

let namespace' name inner = match name with Some n -> sprintf "namespace %s" n :: block inner | None -> inner



module Components =
    type Wrapper = {
        showInMenu : bool
        allowMultiple : bool }

    type Field = {
        name : string
        type' : string
        showInInspector : bool }

    type Component = {
        folder : string
        namespace' : string option
        name : string
        imports : string list
        fields : Field list
        constructor : bool
        wrapper : Wrapper option }

    let defaultWrapper = {
        showInMenu = true
        allowMultiple = false }

    let mkField name type' = {
        name = name
        type' = type'
        showInInspector = true }

    let mkComponent folder name = {
        folder = folder
        namespace' = None
        name = name
        imports = []
        fields = []
        constructor = false
        wrapper = None }

    let getImports c =
        List.distinct (
            (if Option.isSome c.wrapper then ["System"] else []) @
            (if (match c.wrapper with Some w -> w.showInMenu || not w.allowMultiple | None -> false) || 
                List.exists (fun f -> f.showInInspector) c.fields then ["UnityEngine"] else []) @
            c.imports)
    
    let component' name inner = 
        sprintf "public struct %sComponent : IComponent%s" name (if List.isEmpty inner then " { }" else "") :: 
        if List.isEmpty inner then [] else block inner

    let field f = sprintf "%spublic %s %s;" (if f.showInInspector then "" else "[HideInInspector] ") f.type' f.name

    let constructor name fields =
        if List.isEmpty fields then [] 
        else
            let args = List.map (fun f -> sprintf "%s %s" f.type' (f.name.ToLower ())) fields |> String.concat ", "
            let inits = List.map (fun (f : Field) -> sprintf "%s = %s;" f.name (f.name.ToLower ())) fields
            "" :: sprintf "public %sComponent(%s)" name args :: block inits

    let wrapper namespace' name = function
        | Some w ->
            "" ::
            (if w.showInMenu then [sprintf "[AddComponentMenu(\"%sComponents/%s\")]" (match namespace' with Some n -> n + "/" | None -> "") name] else []) @
            (if w.allowMultiple then [] else ["[DisallowMultipleComponent]"]) @
            [sprintf "public class %sDataComponent : ComponentDataWrapper<%sComponent> { }" name name]
        | None -> []

    let code c =
        imports (getImports c) @
        namespace' c.namespace' (
            (if Option.isSome c.wrapper then ["[Serializable]"] else []) @ 
            component' c.name (List.map field c.fields @ if c.constructor then constructor c.name c.fields else []) @
            wrapper c.namespace' c.name c.wrapper)

    let writeFile c =
        Directory.CreateDirectory c.folder |> ignore
        File.WriteAllLines (Path.Combine (c.folder, sprintf "%s%sComponent.cs" c.name (if Option.isSome c.wrapper then "Data" else "")), code c)