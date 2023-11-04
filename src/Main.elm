module Main exposing (..)

import Browser
import Html exposing (Html, div, input, text)
import Html.Attributes exposing (placeholder, style, type_, value)
import Html.Events exposing (onInput)
import Http
import Json.Decode exposing (Decoder, at, map, string)
import Url.Builder



-- MAIN


main : Program () Model Msg
main =
    Browser.element { init = init, update = update, subscriptions = subscriptions, view = view }



-- MODEL


type alias Model =
    { cardId : Maybe String
    , cardInfo : Maybe CardInfo
    }


type alias CardInfo =
    { name : String }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { cardId = Nothing, cardInfo = Nothing }, Cmd.none )



-- UPDATE


type Msg
    = GotJson (Result Http.Error CardInfo)
    | GotId String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotJson result ->
            let
                _ =
                    Debug.log "result" result
            in
            case result of
                Ok cardInfo ->
                    let
                        _ =
                            Debug.log "cardInfo" cardInfo
                    in
                    ( { model | cardInfo = Just cardInfo }, Cmd.none )

                Err _ ->
                    ( { model | cardInfo = Nothing }, Cmd.none )

        GotId id ->
            let
                _ =
                    Debug.log "id" id

                resp =
                    getCardInfo id
            in
            ( { model | cardId = Just id }, resp )


cardsUrl : String -> String
cardsUrl id =
    Url.Builder.crossOrigin "https://api.magicthegathering.io/v1/cards/" [ id ] []


getCardInfo : String -> Cmd Msg
getCardInfo id =
    Http.get
        { url = cardsUrl id
        , expect = Http.expectJson GotJson responseDecoder
        }


responseDecoder : Decoder CardInfo
responseDecoder =
    map CardInfo
        (at
            [ "card", "name" ]
            string
        )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ viewInput "text" "Id" (Maybe.withDefault "" model.cardId) GotId
        , viewValidation model
        ]


emptyCardInfo : CardInfo
emptyCardInfo =
    CardInfo ""


viewInput : String -> String -> String -> (String -> msg) -> Html msg
viewInput t p v toMsg =
    input [ type_ t, placeholder p, value v, onInput toMsg ] []


viewValidation : Model -> Html msg
viewValidation model =
    div [ style "color" "green" ] [ text <| (Maybe.withDefault emptyCardInfo model.cardInfo).name ]
