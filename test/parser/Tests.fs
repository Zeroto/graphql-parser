module Parser.Tests

open System
open Xunit
open Graphql.Parser


let createParameter name ``type`` required =
  {name=name; ``type``=FieldType.Type ``type``; required=required; directives=[]}
let createSimpleField name ``type`` required =
  {name=name; ``type``=FieldType.Type ``type``; required=required; parameters=[]; directives=[]}
let createListField name ``type`` required =
  {name=name; ``type``=FieldType.List ``type``; required=required; parameters=[]; directives=[]}
let createFieldWithParameters name ``type`` required parameters =
  {name=name; ``type``=FieldType.Type ``type``; required=required; parameters=parameters; directives=[]}



[<Fact>]
let ``Is able to parse the example schema`` () =
  let data = System.IO.File.ReadAllText "data/example.graphql"

  let result = parse data
  Assert.True((match result with | Ok _ -> true | Error _ -> false), sprintf "Failed: %A" result)

[<Fact>]
let ``Is able to parse scalars`` () =
  let data = System.IO.File.ReadAllText "data/scalar.graphql"

  let expected = Ok [
    Scalar ("Test",[])
    Scalar ("Test2",[])
    Scalar ("ScalarWithALongNameScalarWithALongNameScalarWithALongNameScalarWithALongNameScalarWithALongNameScalarWithALongNameScalarWithALongNameScalarWithALongNameScalarWithALongNameScalarWithALongNameScalarWithALongName",[])
    Scalar ("WithSpacesBehind",[])
  ]
  let result = parse data
  Assert.isEqual expected result
  ()

[<Fact>]
let ``Is able to parse comments and ignore them`` () =
  let data = System.IO.File.ReadAllText "data/comments.graphql"

  let expected = Ok [AST.Type ("Test", None, [createSimpleField "field" "String" false])]
  let result = parse data
  Assert.isEqual expected result
  ()

[<Fact>]
let ``Is able to parse simple types`` () =
  let data = System.IO.File.ReadAllText "data/simpleTypes.graphql"

  let expected = Ok [
    AST.Type ("Test", None, [createSimpleField "field" "String" false])
    AST.Type ("Test2", None, [createSimpleField "field1" "String" false; createSimpleField "field2" "String" false])
    AST.Type ("Test3", None, [createListField "listField" (FieldType.Type "String") false])
    AST.Type ("Test4", None, [createListField "listField" (FieldType.List (FieldType.Type "String")) false])
  ]
  let result = parse data
  Assert.isEqual expected result

[<Fact>]
let ``Is able to parse types with required fields`` () =
  let data = System.IO.File.ReadAllText "data/requiredFields.graphql"

  let expected = Ok [AST.Type ("Test", None, [createSimpleField "requiredField" "String" true])]
  let result = parse data
  Assert.isEqual expected result

[<Fact>]
let ``Is able to parse types with fields with parameters`` () =
  let data = System.IO.File.ReadAllText "data/parameterFields.graphql"

  let expected = Ok [AST.Type ("Test", None, [
    createFieldWithParameters "field1" "String" false [createParameter "param1" "String" false]
    createFieldWithParameters "field2" "String" false [createParameter "param2" "String" true]
    createFieldWithParameters "field3" "String" false [createParameter "param3" "String" false; createParameter "param4" "String" false]
  ])]
  let result = parse data
  Assert.isEqual expected result

[<Fact>]
let ``Is able to parse enums`` () =
  let data = System.IO.File.ReadAllText "data/enums.graphql"

  let expected = Ok [AST.Enum ("Test", ["testValue1"; "testValue2"])]
  let result = parse data
  Assert.isEqual expected result

[<Fact>]
let ``Is able to parse interfaces`` () =
  let data = System.IO.File.ReadAllText "data/interfaces.graphql"

  let expected = Ok [
    AST.Interface ("Test", [createSimpleField "field" "String" false])
    AST.Type ("TestImpl", Some "Test", [createSimpleField "field" "String" false])
  ]
  let result = parse data
  Assert.isEqual expected result

[<Fact>]
let ``Is able to parse directives`` () =
  let data = System.IO.File.ReadAllText "data/directives.graphql"

  let createFieldWithDirectives name ``type`` directives: Field =
    {
      name = name
      ``type`` = FieldType.Type ``type``
      required = true
      parameters = []
      directives = directives
    }

  let expected = Ok [
    AST.Scalar ("Test", [("type", [("name","Test")])])
    AST.Scalar ("Test2", [("type", [("name","Test")])])
    AST.Type ("TestType", None, [
      createFieldWithDirectives "field1" "string" [("directive", [])]
      createFieldWithDirectives "field2" "string" [("directive", []); ("directive", [])]
      createFieldWithDirectives "field3" "string" [("directive", [("withParam", "Test")])]
      createFieldWithDirectives "field4" "string" [("directive", [("withParam", "Test");("withParam2", "Test2")])]
      {
        name = "field5"
        ``type`` = FieldType.Type "string"
        required = true
        parameters = [{name="testParam"; ``type``=FieldType.Type "string"; required=false; directives=[("paramDirective",[])]}]
        directives = []
      }
    ])
  ]
  let result = parse data
  Assert.isEqual expected result

[<Fact>]
let ``Is able to parse schema`` () =
  let data = System.IO.File.ReadAllText "data/schema.graphql"

  let expected = Ok [
    AST.Schema {query = Some (FieldType.Type "String", [("directive",[])]); mutation = Some (FieldType.Type "String", [("directive",[])]); directives = [("directive", [])]}
  ]
  let result = parse data
  Assert.isEqual expected result

[<Fact>]
let ``returns an error when schema has more than one query`` () =
  let data = System.IO.File.ReadAllText "data/schema_multipleQueries.graphql"

  let succeeded = parse data |> function | Ok _ -> true | Error _ -> false
  Assert.False(succeeded)

[<Fact>]
let ``returns an error when schema has more than one mutation`` () =
  let data = System.IO.File.ReadAllText "data/schema_multipleMutations.graphql"

  let succeeded = parse data |> function | Ok _ -> true | Error _ -> false
  Assert.False(succeeded)