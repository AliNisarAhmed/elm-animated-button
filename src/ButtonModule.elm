module ButtonModule exposing (..)

type MyButton msg = MyButton (Options msg) String

type alias Options msg =
  { startAnimation : Bool
  , onClick : Maybe msg
	}

createButton : String -> msg -> Button msg
createButton str msg = Button defaultOptions str

defaultOptions : Options msg
defaultOptions =
  { startAnimation : True
  , onClick : Nothing
  }