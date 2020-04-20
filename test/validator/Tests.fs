module Validator.Tests

open System
open Xunit

open Graphql.Parser
open Graphql.Validator

let isEqual a b =
  Assert.True((a = b), sprintf "Expected: %A\nGot: %A" a b)

[<Fact>]
let ``returns Ok when all types are present`` () =
  let data = System.IO.File.ReadAllText "data/Ok.graphql"

  let result = parse data
  match result with
  | Ok ast ->
    let validateResult = validate ast
    Assert.True((match validateResult with | Ok () -> true | Error _ -> false), sprintf "Failed: %A" validateResult)
  | Error x -> Assert.True(false,  sprintf "Failed: %A" x)

[<Fact>]
let ``returns Error when not all types are present`` () =
  let data = System.IO.File.ReadAllText "data/Error.graphql"

  let result = parse data
  match result with
  | Ok ast ->
    let validateResult = validate ast
    Assert.False((match validateResult with | Ok () -> true | Error _ -> false), sprintf "Failed: %A" validateResult)
  | Error x -> Assert.True(false,  sprintf "Failed: %A" x)
