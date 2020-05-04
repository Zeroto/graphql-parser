module Graphql.Validator

open Parser

let validate (ast: AST list) =
  // go through all the interfaces and types to see if all field defined types are available
  let interfaces = ast |> List.choose (function | Interface (a,b) -> Some (a,b) | _ -> None)
  let types = ast |> List.choose (function | AST.Type (a,b,c) -> Some (a,b,c) | _ -> None)

  let validTypeNames =
    ast
    |> List.choose 
        (function
        | AST.Scalar (s, _) -> Some s
        | AST.Interface (n,_) -> Some n
        | AST.Type (n,_,_) -> Some n
        | AST.Enum (n,_) -> Some n
        | AST.Schema _ -> None
        )
  
  let rec getRootType =
    function
    | FieldType.Type s -> s
    | FieldType.List l -> getRootType l

  let failedInterfaces =
    interfaces
    |> List.collect (fun (n, fields) ->
      let failed =
        fields
        |> List.filter (fun f -> 
          let t = f.``type`` |> getRootType
          not <| List.contains t validTypeNames
        )
      failed |> List.map (fun s -> (n,s.name, s.``type`` |> getRootType))
    )

  let failedTypes =
    types
    |> List.collect (fun (n, _, fields) ->
      let failed =
        fields
        |> List.filter (fun f -> 
          let t = f.``type`` |> getRootType
          not <| List.contains t validTypeNames
        )
      failed |> List.map (fun s -> (n,s.name, s.``type`` |> getRootType))
    )

  let failures =
    failedInterfaces
    |> List.append failedTypes
    |> List.map (fun (typename, field, ``type``) -> sprintf "Unknown type %s for field %s in type %s" ``type`` field typename)
  
  if failures |> List.isEmpty then
    Ok ast
  else
    Error failures