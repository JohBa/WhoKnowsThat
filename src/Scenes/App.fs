module App

open Elmish

[<RequireQualifiedAccess>]
type Page =
| Home
| Question

type Msg =
| NavigateTo of Page
| NavigateToRoot
| NavigateBack
| ExitApp
| HomeSceneMsg of Home.Msg
| QuestionSceneMsg of Question.Msg

type SubModel =
| HomeModel of Home.Model
| QuestionModel of Question.Model

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
            | Home.StartGame ->
                model, Cmd.ofMsg (NavigateTo Page.Question)
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

    | NavigateTo page -> 
        navigateTo page (page::model.NavigationStack) model

    | NavigateToRoot -> 
        let last = model.NavigationStack |> List.last
        navigateTo last [last] model

    | NavigateBack -> 
        match model.NavigationStack with
        | _::page::rest -> navigateTo page (page::rest) model
        | _ -> model, Cmd.ofMsg ExitApp

    | ExitApp -> 
        Fable.Helpers.ReactNative.exitApp() 
        model,Cmd.none

let view (model:Model) (dispatch: Msg -> unit) =
    match model.SubModel with
    | HomeModel model -> Home.view model (HomeSceneMsg >> dispatch)
    | QuestionModel model -> Question.view model (QuestionSceneMsg >> dispatch)
