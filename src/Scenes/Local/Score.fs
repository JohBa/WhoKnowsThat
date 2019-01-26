module Local.Score

open Fable.Helpers.ReactNative
open Fable.Helpers.ReactNative.Props
open Elmish
open Fable.Helpers.ReactNativeSimpleStore
open Fable.Import.ReactNative
open Fable.Helpers.ReactNative.Props
open Fable.Import.ReactNative

type Status =
| NotStarted
| InProgress
| CannotSave
| Complete of string

// Model
type Msg =
| SetGame of Model.Game
| Forward of Model.Game
| Error of exn

type Model = { 
    Status: Status
    Game: Model.Game
}

let init () = 
    { 
      Game = { GameId = ""; Players = []; Questions = [] }
      Status = NotStarted
    }, Cmd.none

let update (msg:Msg) model : Model*Cmd<Msg> =
    match msg with
    | SetGame game ->
        { model with Game = game }, Cmd.none

    | Forward game ->
        model, Cmd.none // handled above

    | Error e ->
        { model with Status = Complete e.Message }, Cmd.none

let view (model:Model) (dispatch: Msg -> unit) =
    let sortedPlayers = model.Game.Players |> List.sortBy (fun p -> p.Score)

    let playerScore (player: Model.Player) (rank: int) = 
        let rankBg, rankColor =
            match rank with
            | 1 -> "#ffdb4a", "#a98700"
            | 2 -> "#acb6bf", "#586169"
            | 3 -> "#d29246", "#9a6428"
            | _ -> "#ced5db", "#848f98"
        view 
         [
            ViewProperties.Style 
             [
                FlexStyle.AlignSelf Alignment.Stretch
                FlexStyle.AlignItems ItemAlignment.Center
                FlexStyle.FlexDirection FlexDirection.Row
                FlexStyle.PaddingLeft 10.
                FlexStyle.PaddingRight 10.
                ViewStyle.BackgroundColor "#eee"
                FlexStyle.Height 55.
             ]
         ]
         [
            view 
             [
                ViewProperties.Style 
                 [
                    FlexStyle.Flex 1.
                    FlexStyle.FlexDirection FlexDirection.Row
                    FlexStyle.JustifyContent JustifyContent.FlexStart
                    FlexStyle.PaddingLeft 10.
                    FlexStyle.Width 20.
                    ViewStyle.BackgroundColor rankBg
                    TextStyle.Color rankColor
                    TextStyle.FontSize 19.
                 ]
             ]
             [
                text [] (rank.ToString())
             ]
            view 
             [
                ViewProperties.Style 
                 [
                    FlexStyle.Flex 1.
                    FlexStyle.FlexDirection FlexDirection.Row
                    FlexStyle.JustifyContent JustifyContent.Center
                    FlexStyle.PaddingLeft 10.
                    TextStyle.Color rankColor
                 ]
             ]
             [
                text [] player.Name
             ]
            view 
             [
                ViewProperties.Style 
                 [
                    FlexStyle.Flex 1.
                    FlexStyle.FlexDirection FlexDirection.Row
                    FlexStyle.JustifyContent JustifyContent.FlexEnd
                    FlexStyle.PaddingLeft 10.
                    FlexStyle.Width 30.
                    ViewStyle.BackgroundColor rankBg
                    TextStyle.Color rankColor
                 ]
             ]
             [
                text [] player.Score
             ]
         ]

    

    scrollView [ Styles.sceneBackground ]
        [ text [ Styles.titleText ] model.Question.Question
          text [ Styles.defaultText ] (sprintf "Player: %s" model.CurrentPlayer.Name)
          textInput [ 
            TextInput.TextInputProperties.AutoCorrect false
            TextInput.TextInputProperties.Multiline true
            TextInput.TextInputProperties.Style [
                FlexStyle.MarginTop 50.
                FlexStyle.MarginBottom 50.
              ]
            TextInput.TextInputProperties.OnChangeText (PlayerAnswerChanged >> dispatch)
          ] model.CurrentAnswer
          Styles.buttonWithDisabled "Antwort speichern" isDisabled (fun () -> dispatch SaveAnswer)
          text [ Styles.smallText ] 
            (match model.Status with
             | Complete s -> s
             | _ -> "")  
        ]
