
namespace FSettings

// todo: mapping
// todo: console arguments

open System
open Futility
open FParsec

type Value =
  | Plain of string
  | Quoted of char * string
with
  member o.Value =
    match o with
    | Plain s -> s
    | Quoted (c, s) -> s

type Line =
  | Setting of string * Value
  | Section of string
  | Comment of string
  | Unrecognized of string

module Parser =
  let spaces =
    manyChars (anyOf " \t")
  let eol =
    newline
    <|> (eof >>% '\n')
  let pComment =
    spaces
    >>. pchar ';'
    <|> pchar '#'
    .>>. restOfLine true
    |>> (fun (c, s) -> (string c) + s |> Comment)
  let pIdentChar =
    letter
    <|> digit
    <|> anyOf "`~!@$%^&*()-_+{}.<>/?:"
  let pIdent =
    letter
    .>>. manyChars pIdentChar
    |>> (fun (c, s) -> (string c) + s)
  let pSection =
    spaces
    >>. pchar '['
    >>. spaces
    >>. pIdent
    .>> spaces
    .>> pchar ']'
    .>> spaces
    .>> eol
    |>> (fun n -> Section n)
  let pQuotedValue c =
    let unescape c =
      match c with
      | 'n' -> '\n'
      | 'r' -> '\r'
      | 't' -> '\t'
      | c -> c
    let normalChar = satisfy (fun ch -> ch <> '\\' && ch <> c)
    let escapedChar = pstring "\\" >>. (anyOf ("\\nrt" + (string c)) |>> unescape)
    between
    <| pchar c
    <| pchar c
    <| (manyChars (normalChar <|> escapedChar))
    .>> spaces
    .>> eol
    |>> (fun s -> Quoted (c, s.Substring (0, s.Length)))
  let pPlainValue =
    manyChars (noneOf "\r\n")
    .>> spaces
    .>> eol
    |>> (fun s -> Plain s)
  let pValue =
    pQuotedValue '\''
    <|> pQuotedValue '"'
    <|> pPlainValue
  let pSetting =
    spaces
    >>. pIdent
    .>> spaces
    .>> pchar '='
    .>> spaces
    .>>. pValue
    .>> spaces
    |>> (fun (k, v) -> Setting (k, v))
  let pUnrecognized =
    restOfLine true
    |>> (fun s -> Unrecognized s)
  let pLine =
    attempt pComment
    <|> attempt pSection
    <|> attempt pSetting
    <|> pUnrecognized
  let pLines =
    manyTill pLine eof
  let parse s =
    match run pLines s with
    | Success (r, _, _) -> r
    | Failure (s, _, _) -> failwith s
