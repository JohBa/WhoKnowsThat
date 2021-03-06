module Local.GiveAnswer

open Fable.Helpers.ReactNative
open Fable.Helpers.ReactNative.Props
open Elmish
open Fable.Helpers.ReactNativeSimpleStore
open Model
open System

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
| SetGame of int * Model.Game
| SaveGame
| NextPlayer
| Forward of int * Model.Game
| Error of exn

type Model = { 
    Status: Status
    Question: Model.Question
    Answers: GameAnswer list
    GivenAnswers: GivenAnswer list
    CurrentPlayer: Model.Player
    Game: int * Model.Game
    PlayersLeft: Model.Player list
}

let init () = 
    { 
      Status = NotStarted
      Question = { CorrectAnswer = ""; Question = ""; Language = ""; TrueOrFalse = false }
      Answers = []
      GivenAnswers = []
      CurrentPlayer = { Id = ""; Name = ""; Score = 0 }
      Game = 0, { GameId = ""; Players = []; Questions = []; Date = DateTime.Now }
      PlayersLeft = []
    }, Cmd.none

let updateGame (game : int * Model.Game) = DB.update (fst(game), snd(game))

let saveGame (game : int * Model.Game) =  
    Cmd.ofPromise
        updateGame
        game
        (fun _ -> Forward game)
        Error

let update (msg:Msg) model : Model*Cmd<Msg> =
    match msg with
    | SetGame (gamePos, game) ->
        let playerAnswers = game.Questions.Head.Answers |> List.map PlayerAnswer
        let correctAnswer = QuestionAnswer (game.Questions.Head.Question)
        let answers : GameAnswer list = [correctAnswer] @ playerAnswers
        { model with 
            Game = (gamePos, game)
            PlayersLeft = game.Players
            Question = game.Questions.Head.Question
            Answers = Helpers.shuffle answers 
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
        let newPlayers = 
            snd(model.Game).Players 
            |> List.map 
                (fun p -> 
                    let playerPoints = 
                        model.GivenAnswers 
                        |> List.filter 
                            (fun p' -> 
                                match p' with
                                | CorrectAnswer x -> x.Id = p.Id
                                | WrongAnswer x -> x.PlayerId = p.Id
                            )
                    let score =
                        playerPoints
                        |> List.map
                            (fun p'' -> 
                                match p'' with
                                | CorrectAnswer _ -> 2
                                | WrongAnswer _ -> 3)
                        |> List.sum
                    { p with Score = p.Score + score} 
                )
        let newGame = fst(model.Game), { snd(model.Game) with Players = newPlayers }
        { model with Game = newGame }, saveGame newGame

    | Forward (gamePos, game) ->
        model, Cmd.none // handled above

    | Error e ->
        { model with Status = Complete e.Message }, Cmd.none

let view (model:Model) (dispatch: Msg -> unit) =
    let answers = 
        model.Answers |> List.map (fun a -> 
            view 
             [
                ViewProperties.Style 
                 [ 
                    FlexStyle.FlexDirection FlexDirection.Row
                    FlexStyle.MarginTop 8. 
                    FlexStyle.MarginBottom 8.
                    FlexStyle.AlignSelf Alignment.Stretch
                    FlexStyle.JustifyContent JustifyContent.Center
                 ] 
             ] [
                match a with
                | QuestionAnswer x -> yield Styles.button x.CorrectAnswer (fun () -> dispatch (SetDecision (a, model.CurrentPlayer)))
                | PlayerAnswer x -> yield Styles.button x.Value (fun () -> dispatch (SetDecision (a, model.CurrentPlayer)))
             ]
        )
    
    scrollView [ Styles.sceneBackground ]
        [ text [ Styles.titleText ] model.Question.Question
          text [ Styles.defaultText ] (sprintf "Player: %s" model.CurrentPlayer.Name)
          view [] answers
          text [ Styles.smallText ] 
            (match model.Status with
             | Complete s -> s
             | _ -> "")  
        ]
