module Home

open Fable.Helpers.ReactNative
open Elmish
open Fable.Helpers.ReactNative.Props
open Fable.Helpers.ReactNativeSimpleStore
open Elmish.React
open Fable.Import.ReactNative
open Fable.Helpers.ReactNative.Props
open Fable.Import.ReactNative
open Fable.Helpers.ReactNative.Props
open Fable.Import.ReactNative
open Fable.Helpers.ReactNative.Props
open Fable.Import.ReactNative
open Fable.Helpers.ReactNative.Props
open Fable.Import.ReactNative
open Fable.Helpers.ReactNative.Props

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
    let shadow : Helpers.ShadowOffset = {width = 0.; height = 4.}
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
                ViewStyle.ShadowOffset shadow
                ViewStyle.ShadowRadius 8.
                ViewStyle.Elevation 4.
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
                 ]
             ]
             [
                text [ TextProperties.Style [ TextStyle.FontSize 17.; TextStyle.FontWeight FontWeight.Bold ] ] "Who Knows That"
             ]
            view 
             [ 
                ViewProperties.Style 
                 [
                    FlexStyle.JustifyContent JustifyContent.FlexEnd
                    FlexStyle.FlexDirection FlexDirection.Row
                    FlexStyle.PaddingLeft 10.
                    FlexStyle.PaddingRight 5.
                    FlexStyle.Flex 1.
                 ]
             ]
             [
                touchableNativeFeedback [
                    TouchableWithoutFeedbackProperties.OnPress (fun () -> dispatch MenuTouched)
                ] [ image 
                  [ Source (localImage "${entryDir}/../images/bars_48x48.png")
                    ImageProperties.ResizeMode ResizeMode.Contain
                    ImageProperties.Style [
                      FlexStyle.Height 20.
                      FlexStyle.AlignSelf Alignment.Center
                    ]
                  ] 
                ]
             ]
         ]
        view 
         [
            ViewProperties.Style
             [
                FlexStyle.ZIndex 99.
                FlexStyle.Position Position.Absolute
                ViewStyle.BackgroundColor "#ddd"
                ViewStyle.Elevation 5.
                FlexStyle.MarginTop 10.
                FlexStyle.MarginRight 10.
                FlexStyle.MinWidth 150.
                FlexStyle.MaxWidth 160.
                FlexStyle.Right 0.
             ]
         ] 
         [
            view [ ViewProperties.Style [ ViewStyle.BackgroundColor "#fff"; FlexStyle.Padding 10.] ] [
                text [] "foofsgsgsgagagagagagsgs"
            ]
            view [ ViewProperties.Style [ ViewStyle.BackgroundColor "#fff"; FlexStyle.Padding 10.] ] [
                text [] "foo"
            ]
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
