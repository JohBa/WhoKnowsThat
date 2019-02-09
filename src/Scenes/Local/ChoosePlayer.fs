module Local.ChoosePlayer

open Fable.Helpers.ReactNative
open Elmish
open Fable.Helpers.ReactNative.Props
open System
open Fable.Helpers.ReactNativeSimpleStore


// Model
type Msg =
| Save
| Forward of int * Model.Game
| AcceptInput
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
        let game : Model.Game = {
            GameId = System.Guid.NewGuid().ToString()
            Players = model.Players
            Questions = []
            Date = DateTime.Now
        }
        { model with Game = game }, Cmd.ofPromise save model (fun _ -> Forward (model.GamePos, game)) Error

    | SetGamePos i ->
        { model with GamePos = i }, Cmd.none

    | Forward (gamePos, game) ->
        model, Cmd.none // handled above

    | AddNewPlayer ->
        { model with IsAdding = true }, Cmd.none

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
                    FlexStyle.FlexDirection FlexDirection.Row
                    FlexStyle.MarginTop 3. 
                    FlexStyle.MarginBottom 3.
                    FlexStyle.AlignSelf Alignment.Stretch
                    FlexStyle.Height 60.
                ] 
             ] [
              textInput [
                TextInput.TextInputProperties.AutoCorrect true
                TextInput.TextInputProperties.OnChangeText (PlayerNameChanged >> dispatch)
                TextInput.OnEndEditing (fun _ -> dispatch AcceptInput)
                TextInput.TextInputProperties.Style [
                    FlexStyle.Flex 1.
                    FlexStyle.AlignItems ItemAlignment.Stretch
                    FlexStyle.Height 55.
                ]
              ] model.PlayerName
             ]
        | _ -> Styles.whitespace
     
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
        addPlayer
        view [ ViewProperties.Style [ FlexStyle.MarginTop 20. ] ] [
          button [
              ButtonProperties.Title "Add player"
              ButtonProperties.OnPress (fun () -> dispatch AddNewPlayer)
          ] [ ]
        ]
        view [ ViewProperties.Style [ FlexStyle.MarginTop 20. ] ] [
          button [
              ButtonProperties.Title "Start the game"
              ButtonProperties.OnPress (fun () -> dispatch Save)
          ] [ ]
        ]
        Styles.whitespace
        Styles.whitespace
        text [ Styles.smallText ] model.StatusText  
      ]
