module Local.ChoosePlayer

open Fable.Helpers.ReactNative
open Elmish
open Fable.Helpers.ReactNative.Props
open Fable.Import.ReactNative
open Fable.Helpers.ReactNative.Props
open Fable.Import.ReactNative.Globals
open Fable.Import.ReactNative


// Model
type Msg =
| SaveAndForward
| AcceptInput
| DeletePlayer of string
| AddNewPlayer
| PlayerNameChanged of string
| Error of exn

type Model = { 
    PlayerName: string
    Players: Model.Player list
    StatusText: string
    IsAdding: bool
}

let init () = { PlayerName = ""; StatusText = ""; Players = [{Id = System.Guid.NewGuid.ToString(); Name = "Johannes"}]; IsAdding = true }, Cmd.none

// Update
let update (msg:Msg) model : Model*Cmd<Msg> =
    match msg with
    | SaveAndForward ->
        model, Cmd.none // handled above

    | AddNewPlayer ->
        { model with IsAdding = true }, Cmd.none

    | AcceptInput -> 
        let newPlayer : Model.Player = { Id = System.Guid.NewGuid().ToString(); Name = model.PlayerName }
        let playersList = [newPlayer] @ model.Players
        { model with IsAdding = false; Players = playersList; PlayerName = ""; StatusText = "Number of players " + playersList.Length.ToString() }, Cmd.none

    | DeletePlayer id ->
        model, Cmd.none

    | Error e ->
        { model with StatusText = string e.Message }, Cmd.none

    | PlayerNameChanged name ->
        { model with PlayerName = name }, Cmd.none

// View
let view (model:Model) (dispatch: Msg -> unit) =
    let players = 
        model.Players |> List.map (fun p -> 
            text [] p.Name
        )

    let addPlayer = 
        match model.IsAdding with
        | true ->
            view 
             [ 
                ViewProperties.Style [ 
                    FlexStyle.FlexDirection FlexDirection.Row
                    FlexStyle.MarginTop 3. 
                    FlexStyle.MarginBottom 3.
                    FlexStyle.AlignSelf Alignment.Stretch
                ] 
             ] [
              textInput [
                TextInput.TextInputProperties.AutoCorrect true
                TextInput.TextInputProperties.OnChangeText (PlayerNameChanged >> dispatch)
                TextInput.TextInputProperties.Style [
                    FlexStyle.Flex 1.
                    FlexStyle.AlignItems ItemAlignment.Stretch
                ]
              ] model.PlayerName
              button [
                  ButtonProperties.Title "Ok"
                  ButtonProperties.OnPress (fun () -> dispatch AcceptInput)
              ] [ ]
             ]
        | _ -> Styles.whitespace
     
    scrollView [ Styles.sceneBackground ]
      [ text 
          [ 
              Styles.titleText
          ] "Choose your players"
        view [ ViewProperties.Style [ FlexStyle.MarginTop 40. ] ]
          players
        addPlayer
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
