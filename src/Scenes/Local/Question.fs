module Local.Question

open Fable.Helpers.ReactNative
open Fable.Helpers.ReactNative.Props
open Elmish


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
| SetGame of Model.Game
| NextPlayer
| Error of exn

type Model = { 
    Status: Status
    Question: Model.Question
    PlayerAnswers: Model.PlayerAnswer list
    CurrentAnswer: string
    CurrentPlayer: Model.Player
    Game: Model.Game
    PlayersLeft: Model.Player list
}

let init () = 
    { 
      Game = { GameId = ""; Players = []; Questions = [] }
      Status = NotStarted
      Question = { CorrectAnswer = ""; Question = ""; Language = ""; TrueOrFalse = false }
      PlayerAnswers = []
      CurrentAnswer = ""
      CurrentPlayer = { Id = ""; Name = ""; Score = 0 }
      PlayersLeft = []
    }, Cmd.ofMsg GetQuestion

let update (msg:Msg) model : Model*Cmd<Msg> =
    match msg with
    | GetQuestion ->
        { model with Status = InProgress }, Cmd.ofPromise Database.getRandomQuestion () QuestionLoaded Error
    
    | SetGame game ->
        { model with Game = game; PlayersLeft = game.Players }, Cmd.ofMsg NextPlayer

    | NextPlayer ->
        let playersLeft = model.PlayersLeft.Tail
        { model with PlayersLeft = playersLeft; CurrentPlayer = model.PlayersLeft.Head }, Cmd.none

    | QuestionLoaded (i, q) ->
        { model with Question = q; Status = Complete "" }, Cmd.none

    | PlayerAnswerChanged a -> 
        { model with CurrentAnswer = a }, Cmd.none

    | SaveAnswer -> 
        model, Cmd.none
    
    | Error e ->
        { model with Status = Complete e.Message }, Cmd.none

let view (model:Model) (dispatch: Msg -> unit) =
    view [ Styles.sceneBackground ]
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
          Styles.button "Antwort speichern" (fun () -> dispatch SaveAnswer)
          text [ Styles.smallText ] 
            (match model.Status with
             | Complete s -> s
             | _ -> "")  
        ]
