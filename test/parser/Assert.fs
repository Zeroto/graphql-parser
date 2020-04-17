module Assert

open Xunit

let isEqual a b =
  Assert.True((a = b), sprintf "Expected: %A\nGot: %A" a b)