module Model

open System

type Question = {
    Question: string
    CorrectAnswer: string 
    Language: string
    TrueOrFalse: bool
}

type PlayerAnswer = {
    AnswerId: string
    PlayerId: string
    Value: string
}

type Player = {
    Id: string
    Name: string
    Score: int
}

type GameQuestion = {
    Question: Question
    Answers: PlayerAnswer list
}

type Game = {
    GameId: string
    Date: DateTime
    Players: Player list
    Questions: GameQuestion list
}

