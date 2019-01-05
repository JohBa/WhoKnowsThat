module Local.ChoosePlayer

open Fable.Helpers.ReactNative
open Elmish
open Fable.Helpers.ReactNative.Props


// Model
type Msg =
| SaveAndForward
| AcceptInput of string
| DeletePlayer of string
| AddNewPlayer
| PlayerNameChanged of string
| Error of exn

type Model = { 
    Players: Model.Player list
    StatusText: string
    IsAdding: bool
}

let init () = { StatusText = ""; Players = [{Id = System.Guid.NewGuid.ToString(); Name = "Johannes"}]; IsAdding = true }, Cmd.none

// Update
let update (msg:Msg) model : Model*Cmd<Msg> =
    match msg with
    | SaveAndForward ->
        model, Cmd.none // handled above

    | AddNewPlayer ->
        model, Cmd.none

    | AcceptInput name -> 
        { model with StatusText = "Number of players " + model.Players.Length.ToString() }, Cmd.none

    | DeletePlayer id ->
        model, Cmd.none

    | Error e ->
        { model with StatusText = string e.Message }, Cmd.none

    | PlayerNameChanged name ->
        model, Cmd.none

// View
let view (model:Model) (dispatch: Msg -> unit) =
    let players = 
        model.Players |> List.map (fun p -> 
            text [] p.Name
        )
    scrollView [ Styles.sceneBackground ]
      [ text 
          [ 
              Styles.titleText
          ] "Choose your players"
        view [ ViewProperties.Style [ FlexStyle.MarginTop 40. ] ]
          players
        view [ ViewProperties.Style [ FlexStyle.MarginTop 20. ] ] [
          button [
              ButtonProperties.Title "Add player"
              ButtonProperties.OnPress (fun () -> dispatch AddNewPlayer)
          ] [ ]
        ]
        Styles.whitespace
        Styles.whitespace
        text [ Styles.smallText ] model.StatusText  
      ]
