module Database

open Fable.Core
open Fable.Helpers.ReactNativeSimpleStore
open Fable.PowerPack
open Model

[<Emit("require($0)")>]
let private localResource(path:string): 'T = jsNative

let createQuestions() =
    promise {
        try
            do! DB.clear<Question>()
            // Fetch demo data
            let questions: Question[] =
                localResource "${entryDir}/../data/Questions.json"
            // let! requests =
            //     Fetch.fetchAs<LocationCheckRequest[]>
            //         "https://raw.githubusercontent.com/fsprojects/fable-react_native-demo/master/demodata/LocationCheckRequests.json" []
            do! DB.addMultiple questions
            return questions.Length
        with
        | error -> return 0
    }

let getIndexedQuestions() =
    DB.getAll<Model.Question>()
    |> Promise.map (Array.mapi (fun i r -> i,r))

let getRandomQuestion() =
    promise {
            let rnd = System.Random()
            let! questions = getIndexedQuestions()
            return questions.[rnd.Next(questions.Length)]
    }   