module Model

open System

type Question = string

type Answer = {
    AnswerId: string
    PlayerId: string
    Value: string
}