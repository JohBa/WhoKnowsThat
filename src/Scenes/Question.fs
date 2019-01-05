module Question

open Fable.Helpers.ReactNative
open Fable.Helpers.ReactNative.Props
open Elmish
open Fable.Import.ReactNative


type Status =
| NotStarted
| InProgress
| Complete of string

// Model
type Msg =
| PlayerAnswerChanged of string
| GetQuestion
| QuestionLoaded of int * Model.Question
| SaveAnswer
| Error of exn

type Model = { 
    Status: Status
    Question: Model.Question
    PlayerAnswer: Model.PlayerAnswer
}

let init () = 
    { 
      Status = NotStarted
      Question = { CorrectAnswer = ""; Question = ""; Language = ""; TrueOrFalse = false }
      PlayerAnswer = { AnswerId = System.Guid.NewGuid().ToString(); Value = ""; PlayerId = "" } 
    }, Cmd.ofMsg GetQuestion

let update (msg:Msg) model : Model*Cmd<Msg> =
    match msg with
    | GetQuestion ->
        { model with Status = InProgress }, Cmd.ofPromise Database.getRandomQuestion () QuestionLoaded Error
    
    | QuestionLoaded (i, q) ->
        { model with Question = q; Status = Complete "" }, Cmd.none

    | PlayerAnswerChanged a -> 
        let answer = { model.PlayerAnswer with Value = a }
        { model with PlayerAnswer = answer }, Cmd.none

    | SaveAnswer -> 
        model, Cmd.none
    
    | Error e ->
        { model with Status = Complete e.Message }, Cmd.none

let view (model:Model) (dispatch: Msg -> unit) =
    view [ Styles.sceneBackground ]
        [ text [ Styles.titleText ] model.Question.Question
          textInput [ 
            TextInput.TextInputProperties.AutoCorrect false
            TextInput.TextInputProperties.Multiline true
            TextInput.TextInputProperties.Style [
                FlexStyle.MarginTop 50.
                FlexStyle.MarginBottom 50.
              ]
            TextInput.TextInputProperties.OnChangeText (PlayerAnswerChanged >> dispatch)
          ] model.PlayerAnswer.Value
          Styles.button "Antwort speichern" (fun () -> dispatch SaveAnswer)
          text [ Styles.smallText ] 
            (match model.Status with
             | Complete s -> s
             | _ -> "")  
        ]
