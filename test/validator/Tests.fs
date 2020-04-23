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

  let result =
    parse data
    |> Result.mapError List.singleton
    |> Result.bind validate
  
  Assert.True((match result with | Ok _ -> true | Error _ -> false), sprintf "Failed: %A" result)

[<Fact>]
let ``returns Error when not all types are present`` () =
  let data = System.IO.File.ReadAllText "data/Error.graphql"

  let result =
    parse data
    |> Result.mapError List.singleton
    |> Result.bind validate

  Assert.False((match result with | Ok _ -> true | Error _ -> false), sprintf "Failed: %A" result)
