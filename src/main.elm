module Main exposing (..)

import Array exposing (Array)
import Browser
import Browser.Events exposing (onAnimationFrameDelta)
import Canvas exposing (shapes, rect, path, lineTo)
import Canvas.Settings exposing (fill, stroke)
import Color
import Html exposing (Html, button, div, text, pre, table, tr, td)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Task
import Time


-- Main

main =
  Browser.element 
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view 
    }


-- Model

type alias Gen =  Array ( Array Bool )

type State = Play | Pause

type alias Model =
  { gen : Gen
  , state :  State
  , beat : Int
  , width : Int
  }


init : () -> (Model, Cmd Msg)
init _ =
  ( { gen = initGen
    , state = Play
    , beat = 0
    , width = 3
    }
  , Cmd.none
  )


-- Update

type Msg
  = Seed
  | SetState State
  | NextGen
  | Tick Time.Posix
  | Frame Float


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Seed ->
      ( { model | gen = initGen }, Cmd.none)

    SetState state -> 
      ( { model | state = state }, Cmd.none )

    NextGen ->
      ( { model | gen = nextGen model.gen }, Cmd.none )

    Tick time ->
      ( { model | beat = modBy model.width (model.beat + 1) } , if model.beat == 0 then send NextGen else Cmd.none)

    Frame _ ->
      ( model, Cmd.none )


-- View

renderGen : Gen -> Html Msg
renderGen gen =
  let
    renderCell : Bool -> Html Msg
    renderCell cell =
      case cell of
        True -> td [] [ text "0" ]
        False -> td [] [ text "-" ]

    renderRow : Array Bool -> Html Msg
    renderRow row =
      Array.map renderCell row
      |> Array.toList
      |> tr []
  in

  Array.map renderRow gen
  |> Array.toList
  |> table []


renderGenC : Gen -> Html Msg
renderGenC gen =
  let
    width = 500
    height = 500
    cellWidth = 50
    cellHeight = 50
  in

  Canvas.toHtml (width, height) [ ]
    [ shapes [ stroke Color.black ] [ rect (0, 0) cellWidth cellHeight ] 
    , shapes [ stroke Color.black ] [ path (0, 0) [ lineTo (100, 100) ] ]
    ]


view : Model -> Html Msg
view { gen, state } =
  let
    width = 500
    height = 500
  in

  div []
    [ div [] 
      [ button [ onClick ( SetState ( if state == Play then Pause else Play ) ) ] 
        [ text ( if state == Play then "pause" else "play" ) ]
      ]
    , div [] [ button [ onClick NextGen ] [ text "next" ] ]
    , div [] [ renderGen gen ]
    , div [] [ renderGenC gen ]
    ]


-- Subscriptions

subscriptions : Model -> Sub Msg
subscriptions model =
  let
    state =
      case model.state of
        Play ->
          Time.every 500 Tick

        Pause ->
          Sub.none
  in

  Sub.batch [ state, onAnimationFrameDelta Frame ]


-- Game Logic

type alias Coordinate =
  { x : Int
  , y : Int
  }


initGen : Gen
initGen = 
  Array.fromList 
  [ Array.fromList [ False, False, False ]
  , Array.fromList [ True,  True,  True  ]
  , Array.fromList [ False, False, False ] 
  ]


get : Coordinate -> Gen -> Bool
get {x, y} gen =
  case (Array.get y gen) of
    Nothing -> False
    Just row ->
      case (Array.get x row) of
        Nothing -> False
        Just cell -> cell


neighborCount : Coordinate -> Gen -> Int
neighborCount {x, y} gen =
  Array.fromList [ ( -1, -1 ), ( 0, -1 ), ( 1, -1 ), ( 1, 0 ), ( 1, 1 ), ( 0, 1 ), ( -1, 1 ), ( -1, 0 ) ]
  |> Array.map ( \(dx, dy) -> { x = x + dx, y = y + dy } )
  |> Array.filter (\coord -> get coord gen)
  |> Array.length


lives : Coordinate -> Gen -> Bool
lives coord gen =
  let
    neighbors : Int
    neighbors = neighborCount coord gen
  in
  (neighbors == 3) || (get coord gen && neighbors == 2)


nextGen : Gen -> Gen
nextGen gen =
  Array.indexedMap
    ( \y row -> Array.indexedMap ( \x cell -> lives ( { x = x, y = y } ) gen ) row ) 
    gen


-- Send a message
send : msg -> Cmd msg
send msg =
  Task.succeed msg
  |> Task.perform identity
