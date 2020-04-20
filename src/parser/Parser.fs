module Graphql.Parser

open FParsec
open System.Text.RegularExpressions

type FieldType =
  | Type of string
  | List of FieldType

type Type = {
  name: string
  ``type``: FieldType
  required: bool
}

type Field = {
  ``type``: Type
  parameters: Type list
  directives: string list
}

type AST =
  | Scalar of string
  | Type of string * string option * Field list
  | Enum of string * string list
  | Interface of string * Field list

let isIdentifier c = isLetter c || isDigit c || c = '_'
let identifierParser = many1Satisfy2 isLetter isIdentifier

let scalarParser = skipStringCI "Scalar" >>. spaces >>. identifierParser |>> (Scalar >> Some)

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
  |>> (fun ((a,b),c) -> {name = a; ``type`` = b; required = c})

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
  .>>. (many (skipChar '@' >>. identifierParser .>> spaces))
  |>> (fun ((((a,d),b),c),e) -> {``type``= {name = a; ``type`` = b; required = c}; parameters = d; directives = e})
  
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
