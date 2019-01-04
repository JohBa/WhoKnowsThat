module Database

open Fable.Core
open Fable.Helpers.ReactNativeSimpleStore
open Fable.PowerPack
open Model

[<Emit("require($0)")>]
let private localResource(path:string): 'T = jsNative