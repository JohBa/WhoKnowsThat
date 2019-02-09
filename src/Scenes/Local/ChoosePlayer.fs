module Local.ChoosePlayer

open Fable.Helpers.ReactNative
open Elmish
open Fable.Helpers.ReactNative.Props
open System
open Fable.Helpers.ReactNativeSimpleStore
open Fable


// Model
type Msg =
| Save
| Forward of int * Model.Game
| AcceptInput
| CancelAdding
| DeletePlayer of string
| AddNewPlayer
| SetGamePos of int
| PlayerNameChanged of string
| Error of exn

type Model = { 
    PlayerName: string
    Players: Model.Player list
    StatusText: string
    IsAdding: bool
    Game: Model.Game
    GamePos: int
}

let init () = 
    { 
        PlayerName = ""
        StatusText = ""
        Players = []
        IsAdding = false
        GamePos = 0
        Game = 
            {
                Date = DateTime.Now
                GameId = System.Guid.NewGuid().ToString()
                Players = []
                Questions = []
            }
    }, Cmd.none

let save (model : Model) = 
    let game : Model.Game = {
        Date = DateTime.Now
        GameId = System.Guid.NewGuid().ToString()
        Players = model.Players
        Questions = []
    }
    DB.add game

// Update
let update (msg:Msg) model : Model*Cmd<Msg> =
    match msg with
    | Save ->
        let updateModel, updateCmd = 
            if model.Players.Length > 1 then
                let game : Model.Game = {
                    GameId = System.Guid.NewGuid().ToString()
                    Players = model.Players
                    Questions = []
                    Date = DateTime.Now
                }
                { model with Game = game }, Cmd.ofPromise save model (fun _ -> Forward (model.GamePos, game)) Error
            else
                Toast.showLong "A game must have at least 2 players."
                model, Cmd.none
        updateModel, updateCmd

    | SetGamePos i ->
        { model with GamePos = i }, Cmd.none

    | Forward (gamePos, game) ->
        model, Cmd.none // handled above

    | AddNewPlayer ->
        { model with IsAdding = true }, Cmd.none

    | CancelAdding ->
        { model with IsAdding = false; PlayerName = "" }, Cmd.none
    | AcceptInput -> 
        let playersList = match model.PlayerName.Length with
                          | 0 -> model.Players
                          | _ ->
                                let newPlayer : Model.Player = { Id = System.Guid.NewGuid().ToString(); Name = model.PlayerName; Score = 0 }
                                model.Players @ [newPlayer]
        { model with IsAdding = false; Players = playersList; PlayerName = ""; StatusText = "Number of players " + playersList.Length.ToString() }, Cmd.none

    | DeletePlayer id ->
        let playersList = model.Players |> List.filter (fun x -> x.Id <> id)
        { model with Players = playersList }, Cmd.none

    | Error e ->
        { model with StatusText = string e.Message }, Cmd.none

    | PlayerNameChanged name ->
        { model with PlayerName = name }, Cmd.none

// View
let view (model:Model) (dispatch: Msg -> unit) =
    let renderPlayer (player: Model.Player) =
        view [ ]
         [
            view 
             [
                ViewProperties.Style 
                 [ 
                    FlexStyle.FlexDirection FlexDirection.Row
                    FlexStyle.MarginTop 3. 
                    FlexStyle.MarginBottom 3.
                    FlexStyle.AlignSelf Alignment.Stretch
                    FlexStyle.JustifyContent JustifyContent.Center
                 ] 
             ] [
                text [ 
                    TextProperties.Style [
                        FlexStyle.Flex 1.
                        FlexStyle.FlexDirection FlexDirection.Column
                        FlexStyle.AlignItems ItemAlignment.Center
                        FlexStyle.JustifyContent JustifyContent.Center
                        FlexStyle.MarginRight 20.
                        TextStyle.FontSize 17.
                    ] ] player.Name
                button [
                      ButtonProperties.Title "X"
                      ButtonProperties.OnPress (fun () -> dispatch (DeletePlayer player.Id))
                ] [ ]
             ]
            Styles.separatorView "#ddd"
         ]
        

    let addPlayer = 
        match model.IsAdding with
        | true ->
            view 
              [ 
                ViewProperties.Style [ 
                    FlexStyle.AlignSelf Alignment.Stretch
                    FlexStyle.Flex 1.
                    FlexStyle.JustifyContent JustifyContent.Center
                    FlexStyle.Margin 20.
                    FlexStyle.Height 240.
                    FlexStyle.Position Position.Absolute
                    ViewStyle.BackgroundColor "#fff"
                    ViewStyle.Elevation 40.
                    FlexStyle.Top 30.
                    FlexStyle.Left 1.
                    FlexStyle.Right 1.
                    FlexStyle.Padding 10.
                ] 
              ] [
              textInput [
                TextInput.TextInputProperties.AutoCorrect true
                TextInput.TextInputProperties.OnChangeText (PlayerNameChanged >> dispatch)
                TextInput.TextInputProperties.Style [
                    FlexStyle.AlignItems ItemAlignment.Stretch
                    FlexStyle.Flex 1.
                ]
              ] model.PlayerName
              view [ ViewProperties.Style [FlexStyle.FlexDirection FlexDirection.Row; FlexStyle.JustifyContent JustifyContent.FlexEnd; FlexStyle.Padding 10.]] [
                  button [
                   ButtonProperties.Title "Cancel"
                   ButtonProperties.Color "#bbb"
                   ButtonProperties.OnPress (fun () -> dispatch CancelAdding)] [ ]
                  Styles.whitespace
                  Styles.whitespace
                  Styles.whitespace
                  button [
                   ButtonProperties.Title "Add Player"
                   ButtonProperties.Color Styles.brandSuccess
                   ButtonProperties.OnPress (fun () -> dispatch AcceptInput)] [ ]
              ]
             ]
        | _ -> Styles.whitespace

    view [ ViewProperties.Style [ FlexStyle.Flex 1.] ] [
        scrollView [ Styles.sceneBackground ]
         [ text 
             [ 
                Styles.titleText
             ] "Choose your players"
           view [ ViewProperties.Style [ FlexStyle.MarginTop 40.; FlexStyle.MarginBottom 40. ] ] [
              flatList (model.Players |> List.toArray) [
                  KeyExtractor (Func<_,_,_>(fun (v) _ -> v.Id))
                  RenderItem (Func<_,_>(fun v -> renderPlayer v.item))
                  ItemSeparatorComponent (Styles.separatorView "#000")
              ] ]
           view [ ViewProperties.Style [ FlexStyle.MarginTop 20.; FlexStyle.JustifyContent JustifyContent.FlexEnd; FlexStyle.Flex 1. ] ] [
            button [
                ButtonProperties.Title "Start the game"
                ButtonProperties.OnPress (fun () -> dispatch Save)
            ] [ ]
           ]
           Styles.whitespace
           Styles.whitespace
           text [ Styles.smallText ] model.StatusText
         ]
        addPlayer
        Styles.fab 60. Styles.brandPrimary (localImage "${entryDir}/../images/plusthick_48x48.png") (fun () -> dispatch AddNewPlayer)
    ]
