module internal Styles.ActionBarPage

open Fable.Helpers.ReactNative
open Fable.Helpers.ReactNative.Props
open Elmish
open Fable.Import.React

type Msg = 
    | HideMenu
    | ShowMenu

type Model = { ShowMenu: bool }

let init () = { ShowMenu = false }

let update (msg:Msg) model : Model*Cmd<Msg> =
    match msg with
    | ShowMenu -> { model with ShowMenu = true}, Cmd.none
    | HideMenu -> { model with ShowMenu = false}, Cmd.none

let view (entries: Fable.Import.React.ReactElement list) (content: ReactElement) (model:Model) (dispatch: Msg -> unit) =
    let shadow : Helpers.ShadowOffset = {width = 0.; height = 4.}

    let menu =
        let container element =
            touchableWithoutFeedback [ TouchableWithoutFeedbackProperties.OnPress (fun () -> dispatch HideMenu) ]
             [
                view 
                 [
                    ViewProperties.Style 
                     [ 
                        ViewStyle.Elevation 100.
                        FlexStyle.Position Position.Absolute
                        FlexStyle.Right 0.
                        FlexStyle.Left 0.
                        FlexStyle.Top 0.
                        FlexStyle.Bottom 0.
                        FlexStyle.ZIndex 99.
                        ViewStyle.BackgroundColor "transparent"
                     ]
                 ]
                 [
                    element
                 ]
             ]
        let menuRenderer =
             view 
              [
                 ViewProperties.Style
                  [
                     FlexStyle.ZIndex 999.
                     FlexStyle.Position Position.Absolute
                     ViewStyle.BackgroundColor "#ddd"
                     ViewStyle.Elevation 5.
                     FlexStyle.MarginTop 10.
                     FlexStyle.MarginRight 10.
                     FlexStyle.MinWidth 150.
                     FlexStyle.MaxWidth 160.
                     FlexStyle.Right 0.
                  ]
              ] 
              entries
        match model.ShowMenu with
        | false -> view [] []
        | true -> 
            container menuRenderer

    let actionBarPage menu dispatch =
        view [
                ViewProperties.Style 
                     [ 
                        FlexStyle.AlignSelf Alignment.Stretch
                        FlexStyle.AlignItems ItemAlignment.Stretch
                        FlexStyle.FlexDirection FlexDirection.Column
                        FlexStyle.JustifyContent JustifyContent.FlexStart
                        FlexStyle.Padding 0.
                        FlexStyle.Flex 2.
                        ViewStyle.BackgroundColor "#fff"
                     ]
            ] [
                view 
                 [ 
                    ViewProperties.Style 
                     [ 
                        FlexStyle.AlignSelf Alignment.Stretch
                        FlexStyle.AlignItems ItemAlignment.Center
                        FlexStyle.FlexDirection FlexDirection.Row
                        ViewStyle.ShadowColor "#000"
                        ViewStyle.ShadowOpacity 0.8
                        ViewStyle.ShadowOffset shadow
                        ViewStyle.ShadowRadius 8.
                        ViewStyle.Elevation 4.
                        FlexStyle.PaddingLeft 10.
                        FlexStyle.PaddingRight 10.
                        ViewStyle.BackgroundColor "#eee"
                        FlexStyle.Height 55.
                     ] 
                 ] 
                 [
                    view 
                     [ 
                        ViewProperties.Style 
                         [
                            FlexStyle.Flex 1.
                            FlexStyle.FlexDirection FlexDirection.Row
                            FlexStyle.JustifyContent JustifyContent.FlexStart
                            FlexStyle.PaddingLeft 10.
                         ]
                     ]
                     [
                        text [ TextProperties.Style [ TextStyle.FontSize 17.; TextStyle.FontWeight FontWeight.Bold ] ] "Who Knows That"
                     ]
                    view 
                     [ 
                        ViewProperties.Style 
                         [
                            FlexStyle.JustifyContent JustifyContent.FlexEnd
                            FlexStyle.FlexDirection FlexDirection.Row
                            FlexStyle.PaddingLeft 10.
                            FlexStyle.PaddingRight 5.
                            FlexStyle.Flex 1.
                         ]
                     ]
                     [
                        view [ViewProperties.Style [ ViewStyle.BorderRadius 40.; ViewStyle.Overflow Overflow.Hidden ]] [
                            touchableNativeFeedback [
                                TouchableWithoutFeedbackProperties.OnPress (fun () -> dispatch ShowMenu)
                            ] [ view [ ViewProperties.Style [ FlexStyle.Height 40.; FlexStyle.Width 40.; FlexStyle.JustifyContent JustifyContent.Center ]] [
                                 image 
                                  [ Source (localImage "${entryDir}/../images/bars_48x48.png")
                                    ImageProperties.ResizeMode ResizeMode.Contain
                                    ImageProperties.Style [
                                      FlexStyle.Height 15.
                                      FlexStyle.AlignSelf Alignment.Center
                                    ]
                                  ]
                                ] 
                            ]
                        ]
                     ]
                 ]
                menu
                content 
            ]
    
    actionBarPage