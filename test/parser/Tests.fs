module Tests

open System
open Xunit
open Graphql.Parser

[<Fact>]
let ``Is able to parse the example schema`` () =
  let data = System.IO.File.ReadAllText "data/example.graphql"

  let result = parse data
  Assert.True((match result with | Ok _ -> true | Error _ -> false), sprintf "Failed: %A" result)

[<Fact>]
let ``Is able to parse scalars`` () =
  let data = System.IO.File.ReadAllText "data/scalar.graphql"

  let expected = Ok [
    Scalar "Test"
    Scalar "Test2"
    Scalar "ScalarWithALongNameScalarWithALongNameScalarWithALongNameScalarWithALongNameScalarWithALongNameScalarWithALongNameScalarWithALongNameScalarWithALongNameScalarWithALongNameScalarWithALongNameScalarWithALongName"
    Scalar "WithSpacesBehind"
  ]
  let result = parse data
  Assert.isEqual expected result
  ()

[<Fact>]
let ``Is able to parse comments and ignore them`` () =
  let data = System.IO.File.ReadAllText "data/comments.graphql"

  let expected = Ok []
  let result = parse data
  Assert.isEqual expected result
  ()

let createParameter name ``type`` required =
  {name=name; ``type``=FieldType.Type ``type``; required=required}
let createSimpleField name ``type`` required =
  {``type`` = {name=name; ``type``=FieldType.Type ``type``; required=required}; parameters=[]; directives=[]}
let createListField name ``type`` required =
  {``type`` = {name=name; ``type``=FieldType.List ``type``; required=required}; parameters=[]; directives=[]}
let createFieldWithParameters name ``type`` required parameters =
  {``type`` = {name=name; ``type``=FieldType.Type ``type``; required=required}; parameters=parameters; directives=[]}


[<Fact>]
let ``Is able to parse simple types`` () =
  let data = System.IO.File.ReadAllText "data/simpleTypes.graphql"

  let expected = Ok [
    AST.Type ("Test", [createSimpleField "field" "String" false])
    AST.Type ("Test2", [createSimpleField "field1" "String" false; createSimpleField "field2" "String" false])
    AST.Type ("Test3", [createListField "listField" "String" false])
  ]
  let result = parse data
  Assert.isEqual expected result

[<Fact>]
let ``Is able to parse types with required fields`` () =
  let data = System.IO.File.ReadAllText "data/requiredFields.graphql"

  let expected = Ok [AST.Type ("Test", [createSimpleField "requiredField" "String" true])]
  let result = parse data
  Assert.isEqual expected result

[<Fact>]
let ``Is able to parse types with fields with parameters`` () =
  let data = System.IO.File.ReadAllText "data/parameterFields.graphql"

  let expected = Ok [AST.Type ("Test", [
    createFieldWithParameters "field1" "String" false [createParameter "param1" "String" false]
    createFieldWithParameters "field2" "String" false [createParameter "param2" "String" true]
    createFieldWithParameters "field3" "String" false [createParameter "param3" "String" false; createParameter "param4" "String" false]
  ])]
  let result = parse data
  Assert.isEqual expected result