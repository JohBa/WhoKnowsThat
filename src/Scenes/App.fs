module App

open Elmish
open Elmish.React

[<RequireQualifiedAccess>]
type Page =
| Home

type Msg =
| NavigateTo of Page
| NavigateBack
| ExitApp
| HomeSceneMsg of Home.Msg

type SubModel =
| HomeModel of Home.Model

type Model = {
    SubModel : SubModel
    NavigationStack: Page list
}

let wrap ctor msgCtor model (subModel,cmd)  =
    { model with SubModel = ctor subModel }, Cmd.map msgCtor cmd

let navigateTo page newStack model =
    match page with
    | Page.Home -> Home.init() |> wrap HomeModel HomeSceneMsg model
    |> fun (model,cmd) -> { model with NavigationStack = newStack },cmd

let update (msg:Msg) model : Model*Cmd<Msg> = 
    match msg with
    | HomeSceneMsg subMsg ->
        match model.SubModel with
        | HomeModel subModel -> 
            match subMsg with
            | _ ->
                Home.update subMsg subModel |> wrap HomeModel HomeSceneMsg model
        | _ -> model,Cmd.none

    | NavigateTo page -> 
        navigateTo page (page::model.NavigationStack) model

    | NavigateBack -> 
        match model.NavigationStack with
        | _::page::rest -> navigateTo page (page::rest) model
        | _ -> model,Cmd.ofMsg ExitApp

    | ExitApp -> 
        Fable.Helpers.ReactNative.exitApp() 
        model,Cmd.none

let init() =
    let subModel,cmd = Home.init() 
    { SubModel = HomeModel subModel
      NavigationStack = [Page.Home] }, Cmd.map HomeSceneMsg cmd

let view (model:Model) (dispatch: Msg -> unit) =
    match model.SubModel with
    | HomeModel model -> Home.view model (HomeSceneMsg >> dispatch)
