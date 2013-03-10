
module FSettings.Ini

open System
open System.IO
open Futility

let parse source = IniFile source
let load path = IniFile (File.ReadAllText path)
let save path (ini : IniFile) = ini.Save path
