
namespace FSettings

open System
open System.IO
open Futility

type IniFile (source) =
  let read ls =
    let current = ref None
    let globals = Dict()
    let settings = Dict<string, Dict<string, string>>()
    for l in ls do
      match l with
      | Setting (k, v) ->
        match !current with
        | None ->
          if globals.ContainsKey k then
            globals.[k] <- globals.[k] + " " + !v
          else
            globals.Add (k, !v)
        | Some current ->
          if settings.[current].ContainsKey k then
            settings.[current].[k] <- settings.[current].[k] + " " + !v
          else
            settings.[current].Add (k, !v)
      | Section s ->
        settings.Add (s, Dict ())
        current := Some s
      | _ -> ()
    globals, settings
  let lines =
    source
    |> Parser.parse
    |> ResizeArray.ofList
  let globals, settings = read lines
  let buildValue (s : string) =
    if s.Contains "\r" || s.Contains "\n" then
      if s.Contains "\"" then
        Quoted ('\'', s)
      else
        Quoted ('"', s)
    else
      Plain s
  let globalIndex key =
    let fi x = match x with Setting (k, _) when k = key -> true | _ -> false
    lines |> ResizeArray.findIndex fi
  let sectionIndex section =
    let fiSec x = match x with Section n when n = section -> true | _ -> false
    lines |> ResizeArray.findIndex fiSec
  let settingIndex sectionIndex key =
    let fiSet x = match x with Setting (k, _) when k = key -> true | _ -> false
    (lines.GetRange (sectionIndex, lines.Count - sectionIndex)
    |> ResizeArray.findIndex fiSet)
    + sectionIndex
  member o.Globals = seq globals.Keys
  member o.Global key =
    if globals.ContainsKey key then Some globals.[key]
    else None
  member o.Sections = seq settings.Keys
  member o.Keys section =
    if settings.ContainsKey section then
      seq settings.[section].Keys
    else
      seq []
  member o.Value section key =
    if settings.ContainsKey section then
      if settings.[section].ContainsKey key then
        Some settings.[section].[key]
      else
        None
    else
      None
  member o.SetGlobal key value =
    if globals.ContainsKey key then
      globals.[key] <- value
      let ix = globalIndex key
      lines.[ix] <- Setting (key, buildValue value)
    else
      globals.Add (key, value)
      lines.Insert (0, Setting (key, buildValue value))
  member o.SetValue section key value =
    let setting = Setting (key, buildValue value)
    if settings.ContainsKey section then
      let secIx = sectionIndex section
      if settings.[section].ContainsKey key then
        settings.[section].[key] <- value
        let setIx = settingIndex secIx key
        lines.[setIx] <- setting
      else
        settings.[section].Add (key, value)
        lines.Insert (secIx + 1, setting)
    else
      o.AddSection section
      settings.[section].Add (key, value)
      lines.Add setting
  member o.AddSection section =
    if settings.ContainsKey section then ()
    else
      settings.Add (section, Dict ())
      lines.Add (Section section)
  member o.RemoveGlobal key =
    if globals.ContainsKey key then
      globals.Remove key |> ignore
      let ix = globalIndex key
      lines.RemoveAt ix
    else ()
  member o.RemoveValue section key =
    if settings.ContainsKey section then
      if settings.[section].ContainsKey key then
        settings.[section].Remove key |> ignore
        let ix = settingIndex (sectionIndex section) key
        lines.RemoveAt ix
      else ()
    else ()
  member o.RemoveSection section =
    let on = ref false
    let rem = ResizeArray ()
    for line in lines do
      match line, !on with
      | Section n, _ when n = section ->
        on := true
        rem.Add line
      | Section _, _ -> on := false
      | _, true -> rem.Add line
      | _ -> ()
    for l in rem do lines.Remove l |> ignore
  member o.AsMap section =
    if settings.ContainsKey section then
      settings.[section]
      |> Seq.map (fun p -> p.Key, p.Value)
      |> Map.ofSeq
    else
      Map.empty
  member o.Item with get i = o.AsMap i
  member o.ContainsSection section =
    settings.ContainsKey section
  member o.Write (stream : Stream) =
    let writer = new StreamWriter (stream)
    for line in lines do
      let text =
        match line with
        | Section s -> "[" + s + "]"
        | Setting (k, Plain v) -> k + "=" + v
        | Setting (k, Quoted (c, v)) -> k + "=" + (string c) + v + (string c)
        | Comment t -> t
        | Unrecognized t -> t
      writer.WriteLine text
      writer.Flush ()
  member o.Save path =
    use file = File.Open (path, FileMode.Create, FileAccess.Write, FileShare.Read)
    o.Write file
  override o.ToString () =
    use s = new MemoryStream ()
    o.Write (s)
    s.Seek (0L, SeekOrigin.Begin) |> ignore
    let s = new StreamReader (s)
    s.ReadToEnd ()
