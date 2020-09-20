module Main exposing (..)

import Browser
import Html exposing (..) 
import Html.Events as HE exposing (onClick)
import Html.Attributes exposing (style)
import Set exposing (Set)
import Http exposing (..)

import Json.Decode as Decode exposing (Decoder)
import String exposing (startsWith)
import Browser.Events exposing (onKeyDown, onMouseDown, onMouseUp, onClick)
import Debug
import Dict exposing (Dict)
import Browser.Events exposing (onMouseMove)
import Tuple exposing (..)


---- MODEL ----




type Model = Loading | Running GameState | Error

type alias GameState = 
    {guesses : Set String,
     phrase : String,
     keyPresses :Int,
     mouseDown : Bool,
     mousePos : (Float, Float)}
    


init : ( Model, Cmd Msg )
init = 
    ( Loading, fetchPhrase )

initialGameState = GameState Set.empty "" 0 False (0, 0)


fetchPhrase = 
    Http.get {
        url = "https://random-word-api.herokuapp.com/word?number=1" ,
        expect = Http.expectJson SetWord <| decode "0" 
    }


    

---- UPDATE ----


type Msg
    = Reset | 
    Guess String | 
    SetWord (Result Http.Error  String) | 
    CharKeyPress Char | 
    OtherKeyPress String | 
    MouseDown Float Float | 
    MouseUp Float Float | 
    MouseMove Float Float


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Guess c -> 
            case model of
                Running gameState ->(Running {gameState | guesses = Set.insert c gameState.guesses}, Cmd.none )

                _ -> (model, Cmd.none)

        Reset -> (Loading, fetchPhrase)

        SetWord result ->
             case result of
                 Ok phrase -> (Running {initialGameState | phrase = String.toUpper phrase}, Cmd.none)

                 Err res -> (Error, Cmd.none)

        CharKeyPress c -> 
            case model of 
                Running gameState -> (Running {gameState | keyPresses  = gameState.keyPresses+1, guesses = Set.insert (String.fromChar c) gameState.guesses}, Cmd.none)
            
                _ ->  (model, Cmd.none)

        OtherKeyPress s -> (model, Cmd.none)
            
        MouseDown x y ->
            case model of
                Running gameState -> (Running {gameState | mouseDown = True, mousePos = (x, y)}, Cmd.none)
                _ -> (model, Cmd.none)

        MouseUp x y ->
            case model of
                Running gameState -> (Running {gameState | mouseDown = False, mousePos = (x, y)}, Cmd.none)
                _ -> (model, Cmd.none)

        MouseMove x y -> 
            case model of
                Running gameState -> (Running {gameState | mousePos = (x, y)}, Cmd.none)
                _ -> (model, Cmd.none)
        
decode : String -> Decoder String
decode key = Decode.field key Decode.string




---- VIEW ----


view : Model -> Html Msg
view model =

    case model of
        Running gameState -> viewGameState gameState

        Loading -> div [] [h1 [] [text "LOADING"]]
        
        Error -> div [] [h1 [] [text "ERROR"]]


viewGameState: GameState -> Html Msg
viewGameState gameState = 
    let
                _ = Debug.log "gameState" gameState.guesses
                _ = Debug.log "targetWord" gameState.phrase
                phraseHTML = 
                    gameState.phrase 
                    |> String.split ""
                    |> List.map (\char -> tern (char == " ") " " (tern (Set.member char gameState.guesses ) char "_"))
                    |> List.map (\char -> span [] [text char])
                    |> div []


                failHTML = 
                    gameState.guesses
                    |> Set.toList
                    |> List.filter(\char -> not <| String.contains char gameState.phrase) 
                    |> List.map (\char -> li [] [text char])
                    |> ol []

                buttonsHTML = 
                 List.range 0 25
                 |> List.map (\n -> String.fromChar (Char.toUpper <| Char.fromCode (65 + n)))
                 |> List.map (\c -> button [ HE.onClick <| Guess c ] [ text c ])
                 |> div []


                clickHTML =
                   if gameState.mouseDown then
                    let
                       _ = (Debug.log "mousePos" gameState.mousePos)
                       mouseX = (String.fromFloat <| first gameState.mousePos) ++ "px"
                       mouseY = (String.fromFloat <| second gameState.mousePos) ++ "px"
                       in
                       button [style "position" "fixed", style "left" mouseX, style "top" mouseY] [text "THIS IS A MOVING BUTTON"]
                   else
                       div [] []
            in
            div []
                [phraseHTML,
                 buttonsHTML,
                 failHTML,
                 button [HE.onClick Reset ] [text "Reset"],
                 text gameState.phrase,
                 clickHTML
                ]




---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = subscriptions
        }

subscriptions : Model -> Sub Msg
subscriptions model = 
    case model of 
        Running gameState -> 
            if gameState.mouseDown then
                Sub.batch [onKeyDown keyDecoder, onMouseMove mouseMoveDecoder, onMouseUp mouseUpDecoder]
            else 
                Sub.batch [onKeyDown keyDecoder, onMouseDown mouseDownDecoder]
        _ -> Sub.none


    

mouseDownDecoder: Decoder Msg
mouseDownDecoder = Decode.map2 handleMouseDown (Decode.field "pageX" Decode.float) (Decode.field "pageY" Decode.float)

handleMouseDown : Float -> Float -> Msg
handleMouseDown x y = MouseDown x y

mouseUpDecoder: Decoder Msg
mouseUpDecoder = Decode.map2 handleMouseUp (Decode.field "pageX" Decode.float) (Decode.field "pageY" Decode.float)

handleMouseUp : Float -> Float -> Msg
handleMouseUp x y = MouseUp x y

mouseMoveDecoder: Decoder Msg
mouseMoveDecoder = Decode.map2 handleMouseMove (Decode.field "pageX" Decode.float) (Decode.field "pageY" Decode.float)

handleMouseMove : Float -> Float -> Msg
handleMouseMove x y = MouseDown x y



keyDecoder: Decoder Msg
keyDecoder = Decode.map toKey (Decode.field "key" Decode.string)

fullKeyDecoder: Decoder Msg
fullKeyDecoder = Decode.map toFullKey (Decode.dict Decode.string)

toKey : String -> Msg
toKey inputVal = 

    case String.uncons inputVal of
        Just (char, "") ->
            CharKeyPress <| Char.toUpper char

        _ -> OtherKeyPress inputVal

toFullKey : Dict String a -> Msg
toFullKey input =
    let
        _ = Debug.log "Dict" input
        
    in
      Guess "c"
   

---- UTIL ----
tern : Bool -> a -> a -> a
tern cond t f = if cond then t else f