module Home

open Fable.Helpers.ReactNative
open Elmish
open Fable.Helpers.ReactNative.Props
open Fable.Helpers.ReactNativeSimpleStore
open Fable.Import.ReactNative
open Fable.Helpers.ReactNative.Props
open Fable.Helpers.ReactNative.ImageEditor
open Fable.Import.ReactNative
open Fable.Helpers.ReactNative.Props
open Fable.Import.ReactNative
open Fable.Helpers.ReactNative.Props
open Fable.Import.ReactNative

// Model
type Msg =
| StartLocalGame
| MenuTouched
| GetQuestions
| QuestionsLoaded of int
| Error of exn

type Model = { 
    StatusText: string
}

let init () = { StatusText = "" }, Cmd.ofPromise DB.clear<Model.Game> () (fun _ -> GetQuestions) Error

// Update
let update (msg:Msg) model : Model*Cmd<Msg> =
    match msg with
    | StartLocalGame ->
        model, Cmd.none // handled in app

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
    //let shadow : Fable.Helpers.ReactNative.Props.Sha = {}
    view [
        ViewProperties.Style 
             [ 
                FlexStyle.AlignSelf Alignment.Stretch
                FlexStyle.AlignItems ItemAlignment.Stretch
                FlexStyle.FlexDirection FlexDirection.Column
                FlexStyle.JustifyContent JustifyContent.FlexStart
                FlexStyle.Padding 0.
                FlexStyle.Flex 1.
                ViewStyle.BackgroundColor "#fff"
             ]
    ] [
        view 
         [ 
            ViewProperties.Style 
             [ 
                FlexStyle.AlignSelf Alignment.Stretch
                FlexStyle.AlignItems ItemAlignment.Center
                FlexStyle.FlexDirection FlexDirection.Row
                ViewStyle.ShadowColor "#000"
                ViewStyle.ShadowOpacity 0.8
                //ViewStyle.ShadowOffset 
                ViewStyle.ShadowRadius 3.
                FlexStyle.PaddingLeft 10.
                FlexStyle.PaddingRight 10.
                ViewStyle.BackgroundColor "#eee"
                FlexStyle.Height 55.
             ] 
         ] 
         [
            touchableWithoutFeedback [
                TouchableWithoutFeedbackProperties.OnPress (fun () -> dispatch MenuTouched)
                TouchableWithoutFeedbackProperties.Style [ FlexStyle.MarginRight 25.]
             ] [ image 
                  [ Source (localImage "${entryDir}/../images/bars_48x48.png")
                    ImageProperties.ResizeMode ResizeMode.Contain
                    ImageProperties.Style [
                      FlexStyle.Height 20.
                      FlexStyle.AlignSelf Alignment.Center
                    ]
                  ] ]
            text [ TextProperties.Style [ TextStyle.FontSize 14.; TextStyle.FontWeight FontWeight.Bold ] ] "Who Knows That"
         ]
        view 
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
         [ text 
             [ 
                 Styles.titleText
             ] "Who knows that?!"
           view [ ViewProperties.Style [ FlexStyle.MarginTop 50. ] ] [
             button [
                 ButtonProperties.Title "Start local Game"
                 ButtonProperties.OnPress (fun () -> dispatch StartLocalGame)
             ] [ ]
           ]
           Styles.whitespace
           Styles.whitespace
           text [ Styles.smallText ] model.StatusText  ]
    ]
