module Model

open System

type Question = {
    Question: string
    CorrectAnswer: string 
}

type PlayerAnswer = {
    AnswerId: string
    PlayerId: string
    Value: string
}