module Home

open Fable.Helpers.ReactNative
open Fable.Helpers.ReactNative.Props
open Elmish

// Model
type Msg =
| StartGame
| Error of exn

type Model = { 
    StatusText: string
}

// Update
let update (msg:Msg) model : Model*Cmd<Msg> =
    match msg with
    | StartGame ->
        { model with StatusText = "Game started!"},
        Cmd.none

    | Error e ->
        { model with StatusText = string e.Message }, Cmd.none


let init () = { StatusText = "" }, Cmd.none

// View
let view (model:Model) (dispatch: Msg -> unit) =
      view [ Styles.sceneBackground ]
        [ text [ Styles.titleText ] "Who knows that?!"
          Styles.button "Start Game" (fun () -> dispatch StartGame)
          Styles.whitespace
          Styles.whitespace
          text [ Styles.smallText ] model.StatusText  ]
