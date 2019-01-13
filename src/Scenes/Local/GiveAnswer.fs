module Local.GiveAnswer

open Fable.Helpers.ReactNative
open Fable.Helpers.ReactNative.Props
open Elmish
open Fable.Helpers.ReactNativeSimpleStore
open Model

type Status =
| NotStarted
| InProgress
| Complete of string

type GameAnswer = 
| QuestionAnswer of Model.Question
| PlayerAnswer of Model.PlayerAnswer

type GivenAnswer = 
| CorrectAnswer of Model.Player
| WrongAnswer of Model.PlayerAnswer

// Model
type Msg =
| SetDecision of GameAnswer * Model.Player
| SetGame of Model.Game
| SaveGame
| NextPlayer
| Forward of Model.Game
| Error of exn

type Model = { 
    Status: Status
    Question: Model.Question
    Answers: GameAnswer list
    GivenAnswers: GivenAnswer list
    CurrentPlayer: Model.Player
    Game: Model.Game
    PlayersLeft: Model.Player list
}

let init () = 
    { 
      Status = NotStarted
      Question = { CorrectAnswer = ""; Question = ""; Language = ""; TrueOrFalse = false }
      Answers = []
      GivenAnswers = []
      CurrentPlayer = { Id = ""; Name = ""; Score = 0 }
      Game = { GameId = ""; Players = []; Questions = [] }
      PlayersLeft = []
    }, Cmd.none

let updateGame (game : Model.Game) = DB.update (0, game)

let saveGame (game : Model.Game) =  
    Cmd.ofPromise
        updateGame
        game
        (fun _ -> Forward game)
        Error

let update (msg:Msg) model : Model*Cmd<Msg> =
    match msg with
    | SetGame game ->
        let playerAnswers = game.Questions.Head.Answers |> List.map PlayerAnswer
        let correctAnswer = QuestionAnswer (game.Questions.Head.Question)
        let answers : GameAnswer list = [correctAnswer] @ playerAnswers
        { model with 
            Game = game
            PlayersLeft = game.Players
            Question = game.Questions.Head.Question
            Answers = answers 
        }, Cmd.ofMsg NextPlayer

    | NextPlayer ->
        let tail = model.PlayersLeft.Tail
        let head = model.PlayersLeft.Head
        { model with PlayersLeft = tail; CurrentPlayer = head }, Cmd.none

    | SetDecision (answer, player) ->
        let cmd = 
                match model.PlayersLeft.Length with
                | 0 -> Cmd.ofMsg SaveGame
                | _ -> Cmd.ofMsg NextPlayer

        let givenAnswer = 
            match answer with
            | QuestionAnswer _ -> CorrectAnswer player
            | PlayerAnswer x -> WrongAnswer x
        { model with GivenAnswers = [givenAnswer] @ model.GivenAnswers }, cmd
    
    | SaveGame ->
        // TODO: Calculate rest of score and save
        model, saveGame model.Game

    | Forward game ->
        Toast.showShort "all players have given their answer"
        model, Cmd.none // handled above

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
