module Home

open Fable.Helpers.ReactNative
open Elmish
open Fable.Helpers.ReactNative.Props

// Model
type Msg =
| StartLocalGame
| GetQuestions
| QuestionsLoaded of int
| Error of exn

type Model = { 
    StatusText: string
}

let init () = { StatusText = "" }, Cmd.ofMsg GetQuestions

// Update
let update (msg:Msg) model : Model*Cmd<Msg> =
    match msg with
    | StartLocalGame ->
        model, Cmd.none // handled in app

    | GetQuestions ->
        { model with StatusText = "Load questions..." },
        Cmd.ofPromise Database.createQuestions () QuestionsLoaded Error

    | Error e ->
        { model with StatusText = string e.Message }, Cmd.none
    | QuestionsLoaded qs ->
        { model with StatusText = "Questions loaded: " + qs.ToString()}, Cmd.none

// View
let view (model:Model) (dispatch: Msg -> unit) =
      view [ Styles.sceneBackground ]
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
