module Graphql.Parser

open FParsec

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
  | Type of string * Field list

let isIdentifier c = isLetter c || isDigit c || c = '_'

let scalarParser = skipStringCI "Scalar" >>. spaces >>. many1Satisfy2 isLetter isIdentifier |>> (Scalar >> Some)
let commentParser () = skipChar '#' >>. skipRestOfLine true >>% None


let fieldTypeParser, fieldTypeParserRef = createParserForwardedToRef<FieldType, unit>()
let fieldTypeListParser = (skipChar '[' >>. fieldTypeParser .>> skipChar ']' |>> List)
do fieldTypeParserRef := fieldTypeListParser <|> (many1Satisfy2 isLetter isIdentifier |>> FieldType.Type)

let parameterParser =
  many1Satisfy2 isLetter isIdentifier
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
  many1Satisfy2 isLetter isIdentifier
  .>> spaces
  .>>. parametersParser
  .>> spaces
  .>> skipChar ':'
  .>> spaces
  .>>. fieldTypeParser
  .>>. (opt (pchar '!') |>> Option.isSome)
  .>> spaces
  .>>. (many (skipChar '@' >>. many1Satisfy2 isLetter isIdentifier .>> spaces))
  |>> (fun ((((a,d),b),c),e) -> {``type``= {name = a; ``type`` = b; required = c}; parameters = d; directives = e})
  
let typeParser =
  skipStringCI "Type"
  >>. spaces 
  >>. many1Satisfy2 isLetter isIdentifier 
  .>> spaces 
  .>> skipChar '{' 
  .>>. (many (spaces >>. ((typeFieldParser |>> Some) <|> commentParser()) .>> spaces) |>> List.choose id)
  .>> skipChar '}' 
  |>> (AST.Type >> Some)

let astValue = choice [
  scalarParser
  commentParser()
  typeParser
]

let schemaParser = many (spaces >>. astValue .>> spaces) .>> eof |>> List.choose id

let parse schema =
  let result = run schemaParser schema
  match result with
  | Success (r,_,_) ->
    Result.Ok r
  | Failure (error, _, _) ->
    Result.Error error
