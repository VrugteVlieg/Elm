module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Events exposing (..)
import Set exposing (Set)
import Http exposing (..)

import Json.Decode as Decode exposing (Decoder)
import String exposing (startsWith)


---- MODEL ----




type Model = Loading | Running GameState | Error

type alias GameState = 
    {guesses : Set String,
     phrase : String}
    


init : ( Model, Cmd Msg )
init = 
    ( Loading, fetchPhrase )


fetchPhrase = 
    Http.get {
        url = "https://random-word-api.herokuapp.com/word?number=1" ,
        expect = Http.expectJson SetWord <| decode "0" 
    }

---- UPDATE ----


type Msg
    = Reset | Guess String | SetWord (Result Http.Error  String)


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
                Ok phrase -> (Running {phrase = String.toUpper phrase, guesses = Set.empty}, Cmd.none)

                Err res -> (Error, Cmd.none)
        
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
                 |> List.map (\c -> button [ onClick <| Guess c ] [ text c ])
                 |> div []
            in
            div []
                [phraseHTML,
                 buttonsHTML,
                 failHTML,
                 button [onClick Reset ] [text "Reset"],
                 text gameState.phrase
                ]




---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }


---- UTIL ----
tern : Bool -> a -> a -> a
tern cond t f = if cond then t else f