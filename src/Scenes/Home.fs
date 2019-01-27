module Home

open Fable.Helpers.ReactNative
open Elmish
open Fable.Helpers.ReactNative.Props
open Fable.Helpers.ReactNativeSimpleStore
open Elmish.React
open Components

// Model
type Msg =
| StartLocalGame
| MenuTouched
| GetQuestions
| PageMsg of ActionBarPage.Msg
| QuestionsLoaded of int
| Error of exn

type Model = { 
    StatusText: string
    ShowMenu: bool
    PageModel: ActionBarPage.Model
}

let init () = 
    let pageModel, cmd = ActionBarPage.init()
    { StatusText = ""; ShowMenu = false; PageModel = pageModel }, 
        Cmd.batch [
            Cmd.map PageMsg cmd
            Cmd.ofPromise DB.clear<Model.Game> () (fun _ -> GetQuestions) Error]

// Update
let update (msg: Msg) model : Model*Cmd<Msg> =
    match msg with
    | StartLocalGame ->
        model, Cmd.none // handled in app

    | PageMsg msg ->
        let submodel, subcmd = ActionBarPage.update msg model.PageModel
        //Toast.showLong (msg.ToString())
        { model with PageModel = submodel }, Cmd.map PageMsg subcmd

    | MenuTouched -> 
        Toast.showLong "menu touched"
        model, Cmd.none

    | GetQuestions ->
        { model with StatusText = "Load questions..." },
        Cmd.ofPromise Database.createQuestions () QuestionsLoaded Error

    | Error e ->
        { model with StatusText = string e.Message }, Cmd.none

    | QuestionsLoaded qs ->
        { model with StatusText = "Questions loaded: " + qs.ToString()}, Cmd.none

// View
let view (model:Model) (dispatch: Msg -> unit) =
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
                 ButtonProperties.OnPress (fun () -> dispatch StartLocalGame)
              ] [ ]
            ] 
           Styles.whitespace
           Styles.whitespace
           text [ Styles.smallText ] model.StatusText  
         ]
    let scoresMenuEntry = ActionBarMenuEntry.menuEntry "Scores" (fun () -> dispatch MenuTouched)
    
    ActionBarPage.view "Who Knows That?!" [scoresMenuEntry] content model.PageModel (dispatch << PageMsg)
        
