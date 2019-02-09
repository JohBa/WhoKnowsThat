module ScoresList

open Fable.Helpers.ReactNative
open Elmish
open Fable.Helpers.ReactNative.Props
open Elmish.React
open Components
open System
open Fable.Import.ReactNative
open Fable.Helpers.ReactNative.Props
open Fable.Import.ReactNative
open Fable.Helpers.ReactNative.Props

// Model
type Msg =
| ShowGameScore of int
| ToHome
| GamesLoaded of (int * Model.Game)[]
| PageMsg of ActionBarPage.Msg
| Error of exn

type Model = { 
    StatusText: string
    ShowMenu: bool
    PageModel: ActionBarPage.Model
    Games: (int * Model.Game) []
}

let init () = 
    let pageModel, cmd = ActionBarPage.init()
    { StatusText = ""; ShowMenu = false; PageModel = pageModel; Games = [||] }, 
        Cmd.batch [
            Cmd.map PageMsg cmd
            Cmd.ofPromise Database.getIndexedGames () GamesLoaded Error]

// Update
let update (msg: Msg) model : Model*Cmd<Msg> =
    match msg with
    | ShowGameScore gameIndex ->
        model, Cmd.none // handled in app

    | PageMsg msg ->
        let submodel, subcmd = ActionBarPage.update msg model.PageModel
        { model with PageModel = submodel }, Cmd.map PageMsg subcmd

    | ToHome -> 
        model, Cmd.none // handled in app

    | GamesLoaded indexedGames ->
        { model with Games = indexedGames },
        Cmd.none

    | Error e ->
        { model with StatusText = string e.Message }, Cmd.none

// View
let view (model:Model) (dispatch: Msg -> unit) =
    let renderGame ((gamePos, game): int * Model.Game) =
        let content = sprintf "%s, %i Players" (game.Date.ToString "dd.MM.yyyy") game.Players.Length
        view []
         [
          view 
           [
              ViewProperties.Style 
               [
                  FlexStyle.AlignItems ItemAlignment.Center
                  FlexStyle.FlexDirection FlexDirection.Row
                  ViewStyle.BackgroundColor "#eee"
                  FlexStyle.MarginBottom 20.0
                  FlexStyle.Padding 10.0
                  FlexStyle.MinHeight 55.0
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
                      FlexStyle.JustifyContent JustifyContent.FlexStart
                      FlexStyle.AlignItems ItemAlignment.Center
                      FlexStyle.PaddingLeft 10.
                   ]
               ]
               [
                  text [ TextProperties.Style [TextStyle.Color "#000"; TextStyle.FontSize 16.0]] content
               ]
           ]           
         ]

    let content = 
        scrollView 
         [ 
            ViewProperties.Style 
             [ 
                FlexStyle.AlignSelf Alignment.Stretch
                FlexStyle.Padding 0.               
                ViewStyle.ShadowOpacity 0.8
                ViewStyle.ShadowRadius 3.
                FlexStyle.Flex 2.
             ] 
         ]
         [ 
           view [ ViewProperties.Style [ FlexStyle.MarginTop 10.; FlexStyle.MarginBottom 10. ] ] 
            [
                flatList (model.Games) [
                    KeyExtractor (Func<_,_,_>(fun (i,_) _ -> i.ToString()))
                    RenderItem (Func<_,_>(fun v -> renderGame (fst(v.item), snd(v.item))))
                ]
            ]
           Styles.whitespace
           Styles.whitespace
           text [ Styles.smallText ] model.StatusText  
         ]

    let homeMenuEntry = ActionBarMenuEntry.menuEntry "Home" (fun () -> dispatch ToHome)
    
    ActionBarPage.view "Highscores" [homeMenuEntry] content model.PageModel (dispatch << PageMsg)
        
