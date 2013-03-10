
namespace FSettings

open System
open Futility
open FParsec

type ExplicitSettingsAttribute () =
  inherit Attribute ()

type SettingAttribute (name : string) =
  inherit Attribute ()
  member o.Name = name

module private Parsers =
  let pPart suf =
    pfloat
    .>> spaces
    .>> pstring suf
    .>> notFollowedBy letter
    .>> spaces
    |> attempt
    |> opt
  let part n f =
    match n with
    | Some n -> f n
    | None -> TimeSpan.Zero
  let pSpan =
    spaces
    >>. pipe5
      (pPart "d")
      (pPart "h")
      (pPart "m")
      (pPart "s")
      (pPart "ms")
      (fun d h m s ms ->
        part d TimeSpan.FromDays
        + part h TimeSpan.FromHours
        + part m TimeSpan.FromMinutes
        + part s TimeSpan.FromSeconds
        + part ms TimeSpan.FromMilliseconds
      )

open Parsers

module TimeSpan =
  let parseHuman s =
    match run pSpan s with
    | Success (r, _, _) -> r
    | Failure (s, _, _) -> failwith s
  let toStringHuman (t : TimeSpan) =
    let ts n s =
      if n > 0 then (string n) + s
      else ""
    ts t.Days "d"
    + ts t.Hours "h"
    + ts t.Minutes "m"
    + ts t.Seconds "s"
    + ts t.Milliseconds "ms"


