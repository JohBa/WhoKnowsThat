module Components.ActionBarMenuEntry

open Fable.Helpers.ReactNative
open Fable.Helpers.ReactNative.Props


let menuEntry (title: string) (onPress) =
    touchableNativeFeedback 
      [
         TouchableWithoutFeedbackProperties.OnPress onPress
      ]
      [
         view [ ViewProperties.Style [ ViewStyle.BackgroundColor "#fff"; FlexStyle.Padding 10.] ] [
             text [] title
         ]
      ]