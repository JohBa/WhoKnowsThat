module App

open Elmish
open Fable.PowerPack.Date
open Fable.Helpers.ReactNative
open Fable.Helpers.ReactNative.Props

[<RequireQualifiedAccess>]
type Page =
| Home
| Question
| LocalChoosePlayer
| LocalQuestion
| LocalGiveAnswer
| LocalScore

type Msg =
| NavigateTo of Page
| NavigateToRoot
| NavigateBack
| ExitApp
| HomeSceneMsg of Home.Msg
| QuestionSceneMsg of Question.Msg
| LocalChoosePlayerMsg of Local.ChoosePlayer.Msg
| LocalQuestionMsg of Local.Question.Msg
| LocalGiveAnswerMsg of Local.GiveAnswer.Msg
| LocalScoreMsg of Local.Score.Msg

type SubModel =
| HomeModel of Home.Model
| QuestionModel of Question.Model
| LocalChoosePlayerModel of Local.ChoosePlayer.Model
| LocalQuestionModel of Local.Question.Model
| LocalGiveAnswerModel of Local.GiveAnswer.Model
| LocalScoreModel of Local.Score.Model

type Model = {
    SubModel : SubModel
    NavigationStack: Page list
}

let wrap ctor msgCtor model (subModel,cmd)  =
    { model with SubModel = ctor subModel }, Cmd.map msgCtor cmd

let navigateTo page newStack model =
    match page with
    | Page.Home -> Home.init() |> wrap HomeModel HomeSceneMsg model
    | Page.Question -> Question.init() |> wrap QuestionModel QuestionSceneMsg model
    | Page.LocalChoosePlayer -> Local.ChoosePlayer.init() |> wrap LocalChoosePlayerModel LocalChoosePlayerMsg model
    | Page.LocalQuestion -> Local.Question.init() |> wrap LocalQuestionModel LocalQuestionMsg model
    | Page.LocalGiveAnswer -> Local.GiveAnswer.init() |> wrap LocalGiveAnswerModel LocalGiveAnswerMsg model
    | Page.LocalScore -> Local.Score.init() |> wrap LocalScoreModel LocalScoreMsg model
    |> fun (model,cmd) -> { model with NavigationStack = newStack },cmd

let init() =
    let subModel,cmd = Home.init() 
    { SubModel = HomeModel subModel
      NavigationStack = [Page.Home] }, Cmd.map HomeSceneMsg cmd

let update (msg:Msg) model : Model*Cmd<Msg> = 
    match msg with
    | HomeSceneMsg subMsg ->
        match model.SubModel with
        | HomeModel subModel -> 
            match subMsg with
            | Home.StartLocalGame ->
                model, Cmd.ofMsg (NavigateTo Page.LocalChoosePlayer)
            | _ ->
                Home.update subMsg subModel |> wrap HomeModel HomeSceneMsg model
        | _ -> model, Cmd.none
    | QuestionSceneMsg subMsg ->
        match model.SubModel with
        | QuestionModel subModel ->
            match subMsg with
            | _ -> 
                Question.update subMsg subModel |> wrap QuestionModel QuestionSceneMsg model
        | _ -> model, Cmd.none
    
    | LocalChoosePlayerMsg subMsg ->
        match model.SubModel with
        | LocalChoosePlayerModel subModel ->
            match subMsg with
            | Local.ChoosePlayer.Forward game ->
                model, Cmd.ofMsg (NavigateTo Page.LocalQuestion) @ Cmd.ofMsg (LocalQuestionMsg (Local.Question.Msg.SetGame game))
            | _ -> 
                Local.ChoosePlayer.update subMsg subModel |> wrap LocalChoosePlayerModel LocalChoosePlayerMsg model
        | _ -> model, Cmd.none

    | LocalQuestionMsg subMsg ->
        match model.SubModel with
        | LocalQuestionModel subModel ->
            match subMsg with
            | Local.Question.Forward game ->
                model, Cmd.ofMsg (NavigateTo Page.LocalGiveAnswer) @ Cmd.ofMsg (LocalGiveAnswerMsg (Local.GiveAnswer.Msg.SetGame game))
            | _ -> 
                Local.Question.update subMsg subModel |> wrap LocalQuestionModel LocalQuestionMsg model
        | _ -> 
            model, Cmd.none

    | LocalGiveAnswerMsg subMsg ->
        match model.SubModel with
        | LocalGiveAnswerModel subModel ->
            match subMsg with
            | Local.GiveAnswer.Forward game ->
                model, Cmd.ofMsg (NavigateTo Page.LocalScore) @ Cmd.ofMsg (LocalScoreMsg (Local.Score.Msg.SetGame game))
            | _ -> 
                Local.GiveAnswer.update subMsg subModel |> wrap LocalGiveAnswerModel LocalGiveAnswerMsg model
        | _ -> 
            model, Cmd.none
    
    | LocalScoreMsg subMsg ->
        match model.SubModel with
        | LocalScoreModel subModel ->
            match subMsg with
            | Local.Score.Forward game ->
                model, Cmd.ofMsg (NavigateTo Page.LocalQuestion) @ Cmd.ofMsg (LocalQuestionMsg (Local.Question.Msg.SetGame game))
            | _ -> 
                Local.Score.update subMsg subModel |> wrap LocalScoreModel LocalScoreMsg model
        | _ -> 
            model, Cmd.none

    | NavigateTo page -> 
        navigateTo page (page::model.NavigationStack) model

    | NavigateToRoot -> 
        let last = model.NavigationStack |> List.last
        navigateTo last [last] model

    | NavigateBack -> 
        match model.NavigationStack with
        | Page.LocalQuestion::_ -> 
            Toast.showShort "Not allowed to go back"
            model, Cmd.none
        | _::page::rest -> navigateTo page (page::rest) model
        | _ -> model, Cmd.ofMsg ExitApp

    | ExitApp -> 
        Fable.Helpers.ReactNative.exitApp() 
        model,Cmd.none

let view (model:Model) (dispatch: Msg -> unit) =
    match model.SubModel with
    | HomeModel model -> Home.view model (HomeSceneMsg >> dispatch)
    | QuestionModel model -> Question.view model (QuestionSceneMsg >> dispatch)
    | LocalChoosePlayerModel model -> Local.ChoosePlayer.view model (LocalChoosePlayerMsg >> dispatch)
    | LocalQuestionModel model -> Local.Question.view model (LocalQuestionMsg >> dispatch)
    | LocalGiveAnswerModel model -> Local.GiveAnswer.view model (LocalGiveAnswerMsg >> dispatch)
    | LocalScoreModel model -> Local.Score.view model (LocalScoreMsg >> dispatch)
