module ScoresList

open Fable.Helpers.ReactNative
open Elmish
open Fable.Helpers.ReactNative.Props
open Elmish.React
open Components

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

    | GetQuestions ->
        { model with StatusText = "Load questions..." },
        Cmd.ofPromise Database.createQuestions () QuestionsLoaded Error

    | GetGames indexedGames ->
        { model with Games = indexedGames },
        Cmd.none

    | Error e ->
        { model with StatusText = string e.Message }, Cmd.none

    | QuestionsLoaded qs ->
        { model with StatusText = "Questions loaded: " + qs.ToString()}, Cmd.none

// View
let view (model:Model) (dispatch: Msg -> unit) =
    let gameIndex = model.Games.Length + 1 

    let content = 
        scrollView 
         [ 
            ViewProperties.Style 
             [ 
                FlexStyle.AlignSelf Alignment.Stretch
                FlexStyle.Padding 20.               
                ViewStyle.ShadowOpacity 0.8
                ViewStyle.ShadowRadius 3.
                FlexStyle.Flex 2.
             ] 
         ]
         [ 
           text 
             [ 
                 Styles.titleText
             ] "Who knows that?!"
           view [ ViewProperties.Style [ FlexStyle.MarginTop 50. ] ] 
            [
             button 
              [
                 ButtonProperties.Title "Start local Game"
                 ButtonProperties.OnPress (fun () -> dispatch (StartLocalGame gameIndex))
              ] [ ]
            ] 
           Styles.whitespace
           Styles.whitespace
           text [ Styles.smallText ] model.StatusText  
         ]
    let scoresMenuEntry = ActionBarMenuEntry.menuEntry "Scores" (fun () -> dispatch MenuTouched)
    
    ActionBarPage.view "Who Knows That?!" [scoresMenuEntry] content model.PageModel (dispatch << PageMsg)
        
