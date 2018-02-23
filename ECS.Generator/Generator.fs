module ECS.Generator

open System
open System.IO



let firstToLower text =
    if String.IsNullOrWhiteSpace text then text
    else Char.ToString (Char.ToLower text.[0]) + if text.Length > 1 then text.Substring 1 else String.Empty

let firstToUpper text =
    if String.IsNullOrWhiteSpace text then text
    else Char.ToString (Char.ToUpper text.[0]) + if text.Length > 1 then text.Substring 1 else String.Empty



let preprocessNamespace = function Some n -> Some <| firstToUpper n | None -> None

let preprocessPrivateFieldName name =
    let name = firstToLower name
    if name.StartsWith "_" then name else "_" + name



let addTab = sprintf "    %s"

let import = sprintf "using %s;"

let imports = List.distinct >> fun xs -> List.map import xs @ if List.isEmpty xs then [] else [""; ""; ""]

let block inner = "{" :: (List.map addTab) inner @ ["}"]

let namespace' name inner = match name with Some n -> sprintf "namespace %s" n :: block inner | None -> inner

let type' kind base' name inner =  
    sprintf "public %s %s : %s%s" kind name base' (if List.isEmpty inner then " { }" else "") :: if List.isEmpty inner then [] else block inner

let struct' = type' "struct"

let class' = type' "class"



let writeFile folder name code =
    Directory.CreateDirectory folder |> ignore
    File.WriteAllLines (Path.Combine (folder, name + ".cs"), List.toArray code)



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



    let defaultWrapper = { showInMenu = true; allowMultiple = false }

    let mkField name type' = { name = name; type' = type'; showInInspector = true }

    let mkComponent folder name = { folder = folder; namespace' = None; name = name; imports = []; fields = []; constructor = false; wrapper = None }



    let preprocessField (f : Field) = { f with name = firstToUpper f.name }

    let preprocessComponent c = { c with namespace' = preprocessNamespace c.namespace'; name = firstToUpper c.name; fields = List.map preprocessField c.fields }


    
    let imports c =
        (if Option.isSome c.wrapper then ["System"] else []) @
        (if (match c.wrapper with Some w -> w.showInMenu || not w.allowMultiple | None -> false) || 
            List.exists (fun f -> f.showInInspector) c.fields then ["UnityEngine"] else []) @
        ["ECS"] @ 
        c.imports
        |> imports

    let component' c =
        let field f = sprintf "%spublic %s %s;" (if f.showInInspector then "" else "[HideInInspector] ") f.type' f.name
        let constructor c =
            if c.constructor && not <| List.isEmpty c.fields
            then
                let args = List.map (fun f -> sprintf "%s %s" f.type' (firstToLower f.name)) c.fields |> String.concat ", "
                let inits = List.map (fun (f : Field) -> sprintf "%s = %s;" f.name (firstToLower f.name)) c.fields
                "" :: sprintf "public %sComponent(%s)" c.name args :: block inits
            else []
        (if Option.isSome c.wrapper then ["[Serializable]"] else []) @
        struct' "IComponent" (c. name + "Component") (List.map field c.fields @ constructor c)

    let wrapper c =
        match c.wrapper with
        | Some w ->
            [""; ""; ""] @
            (if w.showInMenu 
             then [sprintf "[AddComponentMenu(\"%sComponents/%s\")]" (match c.namespace' with Some n -> n + "/" | None -> "") c.name] 
             else []) @
            (if w.allowMultiple then [] else ["[DisallowMultipleComponent]"]) @
            class' (sprintf "ComponentDataWrapper<%sComponent>" c.name) (c.name + "DataComponent") []
        | None -> []

    let namespace' c = namespace' c.namespace' (component' c @ wrapper c)

    let code c = imports c @ namespace' c 



    let writeFile = preprocessComponent >> fun c -> code c |> writeFile c.folder (sprintf "%s%sComponent" c.name <| if Option.isSome c.wrapper then "Data" else "")



module Systems =
    type Component = {
        name : string
        type' : string }

    type System = {
        folder : string
        namespace' : string option
        name : string
        imports : string list 
        components : Component list }



    let mkComponent name type' = { name = name; type' = type' }

    let mkSystem folder name = { folder = folder; namespace' = None; name = name; imports = []; components = [] }



    let preprocessComponent (c : Component) = { name = preprocessPrivateFieldName c.name; type' = firstToUpper c.type' }

    let preprocessSystem s = 
        { s with namespace' = preprocessNamespace s.namespace'; name = firstToUpper s.name; components = List.map preprocessComponent s.components }

    

    let imports s = imports ("ECS" :: s.imports)

    let system s = 
        let cField c = sprintf "[InjectTuple] ComponentArray<%s> %s;" c.type' c.name
        class' "ComponentSystem" (s.name + "System") (List.map cField s.components)

    let namespace' s = namespace' s.namespace' (system s)

    let code s = imports s @ namespace' s 

    let writeFile = preprocessSystem >> fun s -> code s |> writeFile s.folder (s.name + "System")
        