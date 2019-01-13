module internal Styles

open Fable.Helpers.ReactNative
open Fable.Helpers.ReactNative.Props
open Fable.Import.ReactNative

let [<Literal>] brandPrimary = "#428bca"
let [<Literal>] brandInfo = "#5bc0de"
let [<Literal>] brandSuccess = "#5cb85c"
let [<Literal>] brandDanger = "#d9534f"
let [<Literal>] brandWarning = "#f0ad4e"
let [<Literal>] brandSidebar = "#252932"

let [<Literal>] inverseTextColor = "#000"

let [<Literal>] textColor = "#444444"

let [<Literal>] shadowColor = "#000000"

let [<Literal>] backgroundColor = "#FFFFFF"
let [<Literal>] inputBackgroundColor = "#FFFFFF"

let [<Literal>] touched = "#5499C4"

let [<Literal>] fontSizeBase = 15.
let [<Literal>] smallFontSize = 10.
let [<Literal>] mediumFontSize = 12.
let [<Literal>] titleFontSize = 27.

let [<Literal>] borderRadius = 4.


let renderText fontSize =
    TextProperties.Style [ 
        TextStyle.Color textColor
        TextStyle.TextAlign TextAlignment.Center
        FlexStyle.Margin 3.
        TextStyle.FontSize fontSize
      ]

let defaultText<'a> = renderText fontSizeBase
let mediumText<'a> = renderText mediumFontSize
let smallText<'a> = renderText smallFontSize
let titleText<'a> = renderText titleFontSize

let whitespace<'a> = text [ smallText ] ""

let sceneBackground<'a> =
    ViewProperties.Style [ 
        FlexStyle.AlignSelf Alignment.Stretch
        FlexStyle.Padding 20.
        ViewStyle.ShadowColor shadowColor
        ViewStyle.ShadowOpacity 0.8
        ViewStyle.ShadowRadius 3.
        FlexStyle.Flex 1.
        ViewStyle.BackgroundColor backgroundColor
      ]

let viewPagerBackground<'a> =
    ViewPagerAndroidProperties.Style [
        FlexStyle.AlignSelf Alignment.Stretch
        FlexStyle.Padding 20.
        ViewStyle.ShadowColor shadowColor
        ViewStyle.ShadowOpacity 0.8
        ViewStyle.ShadowRadius 3.
        FlexStyle.JustifyContent JustifyContent.Center
        FlexStyle.Flex 1.
        ViewStyle.BackgroundColor backgroundColor
      ]
      
let buttonWithDisabled label isDisabled onPress =
    button [
        ButtonProperties.Disabled isDisabled
        ButtonProperties.Title label
        ButtonProperties.OnPress onPress
    ] [ ]

let button label onPress =
    buttonWithDisabled label false onPress

let separatorView separatorColor =
    view 
     [
        ViewProperties.Style [
            FlexStyle.Flex 1.
            ViewStyle.BackgroundColor "#FFF"
        ]       
     ]
     [
        view [
            ViewProperties.Style [
                FlexStyle.Height 1.
                ViewStyle.BackgroundColor separatorColor ] ] []
     ]