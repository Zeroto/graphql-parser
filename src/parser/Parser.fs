module Graphql.Parser

open FParsec
open System.Text.RegularExpressions

type FieldType =
  | Type of string
  | List of FieldType

type Value =
  | Identifier of string
  | String of string
  | Number of string

type Directive = (string * (string*Value) list)

type Type = {
  name: string
  ``type``: FieldType
  required: bool
  directives: Directive list
}

type Field = {
  name: string
  ``type``: FieldType
  required: bool
  parameters: Type list
  directives: Directive list
}

type Schema = {
  query: (FieldType * Directive list) option
  mutation: (FieldType * Directive list) option
  directives: Directive list
}

type AST =
  | Scalar of (string * Directive list)
  | Type of string * string option * Field list
  | Enum of string * string list
  | Interface of string * Field list
  | Schema of Schema

let isIdentifier c = isLetter c || isDigit c || c = '_'
let identifierParser = many1Satisfy2 isLetter isIdentifier



let valueParser =
  choice [
    identifierParser |>> Identifier
    skipChar '"' >>. charsTillString "\"" true 1000 |>> String
    many1Satisfy isDigit |>> Number
  ]
let keyValueParser =
  identifierParser
  .>> spaces
  .>> skipChar ':'
  .>> spaces
  .>>. valueParser
let keyValuesParser =
  sepBy keyValueParser (spaces >>. skipChar ',' >>. spaces)
let directiveParser =
  skipChar '@'
  >>. identifierParser
  .>>. (opt ( skipChar '(' >>. spaces >>. keyValuesParser .>> spaces .>> skipChar ')' ))
  |>> (fun (a,b) -> a,b |> Option.defaultValue [])
let scalarParser =
  skipStringCI "Scalar"
  >>. spaces
  >>. identifierParser
  .>> spaces
  .>>. (many (directiveParser .>> spaces))
  |>> (Scalar >> Some)

let fieldTypeParser, fieldTypeParserRef = createParserForwardedToRef<FieldType, unit>()
let fieldTypeListParser = (skipChar '[' >>. fieldTypeParser .>> skipChar ']' |>> List)
do fieldTypeParserRef := fieldTypeListParser <|> (identifierParser |>> FieldType.Type)

let parameterParser =
  identifierParser
  .>> spaces
  .>> skipChar ':'
  .>> spaces
  .>>. fieldTypeParser
  .>>. (opt (pchar '!') |>> Option.isSome)
  .>> spaces
  .>>. (many (directiveParser .>> spaces))
  |>> (fun (((a,b),c),d) -> {name = a; ``type`` = b; required = c; directives = d})

let parametersParser =
  opt (
    skipChar '('
    >>. spaces
    >>. sepBy (parameterParser .>> spaces) (skipChar ',' >>. spaces)
    .>> skipChar ')'
  )
  |>> function
      | None -> []
      | Some x -> x

let typeFieldParser =
  identifierParser
  .>> spaces
  .>>. parametersParser
  .>> spaces
  .>> skipChar ':'
  .>> spaces
  .>>. fieldTypeParser
  .>>. (opt (pchar '!') |>> Option.isSome)
  .>> spaces
  .>>. (many (directiveParser .>> spaces))
  |>> (fun ((((a,d),b),c),e) -> {name = a; ``type`` = b; required = c; parameters = d; directives = e})
  
let typeParser =
  skipStringCI "Type"
  >>. spaces 
  >>. identifierParser 
  .>> spaces
  .>>. (opt (skipStringCI "implements" >>. spaces >>. identifierParser .>> spaces))
  .>> skipChar '{' 
  .>> spaces
  .>>. many (typeFieldParser .>> spaces)
  .>> skipChar '}' 
  |>> (fun ((a,b),c) -> (AST.Type (a,b,c) |> Some))

let interfaceParser =
  skipStringCI "Interface"
  >>. spaces 
  >>. identifierParser 
  .>> spaces 
  .>> skipChar '{' 
  .>> spaces
  .>>. many (typeFieldParser .>> spaces)
  .>> skipChar '}' 
  |>> (AST.Interface >> Some)


type SchemaFieldName =
  | Query
  | Mutation

let schemaFieldNameParser =
  choice [
    skipStringCI "query" >>% Query
    skipStringCI "mutation" >>% Mutation
  ]
let schemaFieldParser =
  schemaFieldNameParser
  .>> spaces
  .>> skipChar ':'
  .>> spaces
  .>>. fieldTypeParser
  .>> spaces
  .>>. (many (directiveParser .>> spaces))

let schemaTypeParser =
  skipStringCI "Schema"
  .>> spaces 
  .>> skipChar '{' 
  .>> spaces
  >>. many (schemaFieldParser .>> spaces)
  .>> skipChar '}' 
  .>> spaces
  .>>. many (directiveParser .>> spaces)
  >>= fun (a,d) ->
    let counts = a |> List.countBy (fun ((t,_),_) -> t)
    let queryCount = counts |> List.tryFind (fun (x,_) -> x = Query) |> Option.map snd |> Option.defaultValue 0
    let mutationCount = counts |> List.tryFind (fun (x,_) -> x = Mutation) |> Option.map snd |> Option.defaultValue 0
    if queryCount <> 0 && queryCount <> 1 then
      fun (_) -> Reply(ReplyStatus.FatalError, expected "0 or 1 query field in schema")
    else if mutationCount <> 0 && mutationCount <> 1 then
      fun (_) -> Reply(ReplyStatus.FatalError, expected "0 or 1 mutation field in schema")
    else
      let query = a |> List.tryFind (fun ((t,_),_) -> t = Query) //((_, queryType), queryDirectives)
      let mutation = a |> List.tryFind (fun ((t,_),_) -> t = Mutation)

      let schema = {
        query = query |> Option.map (fun ((_, t), d) -> (t,d))
        mutation = mutation |> Option.map (fun ((_, t), d) -> (t,d))
        directives = d
      }
      preturn (AST.Schema schema |> Some)

let enumParser =
  skipStringCI "Enum"
  >>. spaces
  >>. identifierParser
  .>> spaces
  .>> skipChar '{'
  .>> spaces
  .>>. many (identifierParser .>> spaces)
  .>> skipChar '}'
  |>> (AST.Enum >> Some)

let astValue = choice [
  scalarParser
  typeParser
  enumParser
  interfaceParser
  schemaTypeParser
]

let stripComments (s:string) =
  let regex = Regex("#.*\n")
  regex.Replace(s, "")

let schemaParser = many (spaces >>. astValue .>> spaces) .>> eof |>> List.choose id

let parse schema =
  let result =
    stripComments schema
    |> run schemaParser 
  match result with
  | Success (r,_,_) ->
    Result.Ok r
  | Failure (error, _, _) ->
    Result.Error error
