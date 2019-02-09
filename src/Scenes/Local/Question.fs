module Local.Question

open Fable.Helpers.ReactNative
open Fable.Helpers.ReactNative.Props
open Elmish
open Fable.Helpers.ReactNativeSimpleStore

type Status =
| NotStarted
| InProgress
| CannotSave
| Complete of string

// Model
type Msg =
| PlayerAnswerChanged of string
| GetQuestion
| QuestionLoaded of int * Model.Question
| SaveAnswer
| SetGame of int * Model.Game
| SaveGame
| NextPlayer
| Forward of int * Model.Game
| Error of exn

type Model = { 
    Status: Status
    Question: Model.Question
    PlayerAnswers: Model.PlayerAnswer list
    CurrentAnswer: string
    CurrentPlayer: Model.Player
    Game: int * Model.Game
    PlayersLeft: Model.Player list
}

let init () = 
    { 
      Game = (0, { GameId = ""; Players = []; Questions = [] })
      Status = NotStarted
      Question = { CorrectAnswer = ""; Question = ""; Language = ""; TrueOrFalse = false }
      PlayerAnswers = []
      CurrentAnswer = ""
      CurrentPlayer = { Id = ""; Name = ""; Score = 0 }
      PlayersLeft = []
    }, Cmd.ofMsg GetQuestion

let updateGame (game : int * Model.Game) = DB.update (fst(game), snd(game))

let saveGame (game : int * Model.Game) = 
    Cmd.ofPromise
        updateGame
        game
        (fun _ -> Forward game)
        Error

let update (msg:Msg) model : Model*Cmd<Msg> =
    match msg with
    | GetQuestion ->
        { model with Status = InProgress }, Cmd.ofPromise Database.getRandomQuestion () QuestionLoaded Error
    
    | SetGame (gamePos, game) ->
        { model with Game = (gamePos, game); PlayersLeft = game.Players }, Cmd.ofMsg NextPlayer

    | NextPlayer ->
        let tail = model.PlayersLeft.Tail
        let head = model.PlayersLeft.Head
        { model with PlayersLeft = tail; CurrentPlayer = head }, Cmd.none

    | QuestionLoaded (_, q) ->
        { model with Question = q; Status = CannotSave }, Cmd.none

    | PlayerAnswerChanged a -> 
        let status =
            match a.Length with
            | 0 -> CannotSave
            | _ -> Complete ""
        { model with CurrentAnswer = a; Status = status }, Cmd.none

    | SaveAnswer -> 
        let cmd = 
            match model.PlayersLeft.Length with
            | 0 -> Cmd.ofMsg SaveGame
            | _ -> Cmd.ofMsg NextPlayer

        let playerAnswer : Model.PlayerAnswer = { PlayerId = model.CurrentPlayer.Id; Value = model.CurrentAnswer; AnswerId = System.Guid.NewGuid().ToString() }
        let answers = model.PlayerAnswers @ [playerAnswer]
        { model with PlayerAnswers = answers; CurrentAnswer = ""; Status = CannotSave }, cmd
    
    | SaveGame ->
        let gameQuestion : Model.GameQuestion = { Question = model.Question; Answers = model.PlayerAnswers }
        let newGame : int * Model.Game = (fst(model.Game), { snd(model.Game) with Questions = [gameQuestion] @ snd(model.Game).Questions })
        { model with Game = newGame }, saveGame newGame

    | Forward (gamePos, game) ->
        model, Cmd.none // handled above

    | Error e ->
        { model with Status = Complete e.Message }, Cmd.none

let view (model:Model) (dispatch: Msg -> unit) =
    let isDisabled = 
        match model.Status with
        | CannotSave -> true
        | _ -> false

    scrollView [ Styles.sceneBackground ]
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
          Styles.buttonWithDisabled "Antwort speichern" isDisabled (fun () -> dispatch SaveAnswer)
          text [ Styles.smallText ] 
            (match model.Status with
             | Complete s -> s
             | _ -> "")  
        ]
