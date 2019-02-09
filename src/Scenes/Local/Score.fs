module Local.Score

open Fable.Helpers.ReactNative
open Fable.Helpers.ReactNative.Props
open Elmish
open System
open Components

type Status =
| NotStarted
| InProgress
| CannotSave
| Complete of string

// Model
type Msg =
| SetGame of int * Model.Game
| Forward of int * Model.Game
| EndGame
| PageMsg of ActionBarPage.Msg
| Error of exn

type Model = { 
    Status: Status
    Game: int * Model.Game
    PageModel: ActionBarPage.Model
}

type PlayerRank = {
    Player: Model.Player
    Rank: int
}

let init () = 
    let pageModel, cmd = ActionBarPage.init()
    { 
      Game = (0, { GameId = ""; Players = []; Questions = [] })
      Status = NotStarted
      PageModel = pageModel
    }, Cmd.map PageMsg cmd

let update (msg:Msg) model : Model*Cmd<Msg> =
    match msg with
    | PageMsg msg ->
        let submodel, subcmd = ActionBarPage.update msg model.PageModel
        { model with PageModel = submodel }, Cmd.map PageMsg subcmd

    | SetGame (gamePos, game) ->
        { model with Game = (gamePos, game) }, Cmd.none

    | Forward (gamePos, game) ->
        model, Cmd.none // handled above

    | EndGame ->
        model, Cmd.none // handled above

    | Error e ->
        { model with Status = Complete e.Message }, Cmd.none

let view (model:Model) (dispatch: Msg -> unit) =
    let sortedPlayers = 
        snd(model.Game).Players 
        |> List.sortBy (fun p -> (~-)p.Score)
        |> List.mapi (fun i p -> {Player = p; Rank = i})

    let renderPlayerScore (player: PlayerRank) = 
        let rank = player.Rank + 1
        let rankBg, rankColor =
            match player.Rank with
            | 0 -> "#ffdb4a", "#a98700"
            | 1 -> "#acb6bf", "#586169"
            | 2 -> "#d29246", "#9a6428"
            | _ -> "#ced5db", "#848f98"
        view []
         [
          view 
           [
              ViewProperties.Style 
               [
                  FlexStyle.AlignItems ItemAlignment.Center
                  FlexStyle.FlexDirection FlexDirection.Row
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
                      FlexStyle.AlignSelf Alignment.Stretch
                      FlexStyle.FlexDirection FlexDirection.Row
                      FlexStyle.JustifyContent JustifyContent.Center
                      FlexStyle.AlignItems ItemAlignment.Center
                      FlexStyle.MaxWidth 45.
                      ViewStyle.BackgroundColor rankBg
                   ]
               ]
               [
                  text [ TextProperties.Style [TextStyle.Color rankColor; TextStyle.FontSize 19.]] (rank.ToString())
               ]
              view 
               [
                  ViewProperties.Style 
                   [
                      FlexStyle.Flex 1.
                      FlexStyle.AlignSelf Alignment.Stretch
                      FlexStyle.FlexDirection FlexDirection.Row
                      FlexStyle.JustifyContent JustifyContent.FlexStart
                      FlexStyle.AlignItems ItemAlignment.Center
                      FlexStyle.PaddingLeft 10.
                   ]
               ]
               [
                  text [ TextProperties.Style [TextStyle.Color rankColor]] player.Player.Name
               ]
              view 
               [
                  ViewProperties.Style 
                   [
                      FlexStyle.Flex 1.
                      FlexStyle.AlignSelf Alignment.Stretch
                      FlexStyle.FlexDirection FlexDirection.Row
                      FlexStyle.JustifyContent JustifyContent.Center
                      FlexStyle.AlignItems ItemAlignment.Center
                      FlexStyle.MaxWidth 55.
                   ]
               ]
               [
                  text [ TextProperties.Style [TextStyle.Color rankColor; TextStyle.FontSize 18.]] (player.Player.Score.ToString())
               ]
           ]           
          Styles.separatorView "#ced5db"
         ]

    let content = 
        scrollView [ Styles.sceneBackground ]
            [ text [ Styles.titleText ] "Current Score"
              view [ ViewProperties.Style [ FlexStyle.MarginTop 40.; FlexStyle.MarginBottom 40. ] ] [
                flatList (sortedPlayers |> List.toArray) [
                    KeyExtractor (Func<_,_,_>(fun (v) _ -> v.Rank.ToString()))
                    RenderItem (Func<_,_>(fun v -> renderPlayerScore v.item))
                ] ]
              Styles.button "Nächste Frage" (fun () -> dispatch (Forward model.Game))
              text [ Styles.smallText ] 
                (match model.Status with
                 | Complete s -> s
                 | _ -> "")  
            ]

    let endGameMenuEntry = ActionBarMenuEntry.menuEntry "Spiel beenden" (fun () -> dispatch EndGame)
    
    ActionBarPage.view "Ranking" [endGameMenuEntry] content model.PageModel (dispatch << PageMsg)

