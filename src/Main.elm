module Main exposing (..)

import Browser
import Html exposing (Html, button, div, img, input, text)
import Html.Attributes exposing (placeholder, src, style, type_, value, width)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode exposing (Decoder, at, field, index, list, map, string)
import Url.Builder



-- MAIN


main : Program () Model Msg
main =
    Browser.element { init = init, update = update, subscriptions = subscriptions, view = view }



-- MODEL


type alias Model =
    { searchName : Maybe String
    , cardInfo : Maybe CardInfo
    , cardImageUrl : Maybe String
    , searchResult : List String
    , draftedCards : List String
    }


type alias CardInfo =
    { name : String }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { searchName = Nothing, cardInfo = Nothing, searchResult = [], cardImageUrl = Nothing, draftedCards = [] }, Cmd.none )



-- UPDATE


type Msg
    = GotCardName (Result Http.Error CardInfo)
    | GotCardList (Result Http.Error (List String))
    | GotCardImage (Result Http.Error String)
    | GotSearchName String
    | SelectCard String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotCardName result ->
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

        GotCardImage result ->
            let
                _ =
                    Debug.log "result" result
            in
            case result of
                Ok cardImageUrl ->
                    let
                        _ =
                            Debug.log "cardImageUrl" cardImageUrl
                    in
                    ( { model | cardImageUrl = Just cardImageUrl }, Cmd.none )

                Err _ ->
                    ( { model | cardInfo = Nothing }, Cmd.none )

        GotCardList result ->
            let
                _ =
                    Debug.log "result" result
            in
            case result of
                Ok cardList ->
                    let
                        _ =
                            Debug.log "cardList" cardList
                    in
                    ( { model | searchResult = cardList }, Cmd.none )

                Err _ ->
                    ( { model | searchResult = [] }, Cmd.none )

        GotSearchName name ->
            let
                resp =
                    getCardListFromName name
            in
            ( { model | searchName = Just name }, resp )

        SelectCard name ->
            ( { model | draftedCards = List.append model.draftedCards [ name ] }, Cmd.none )


cardsUrl : String -> String
cardsUrl id =
    Url.Builder.crossOrigin "https://api.magicthegathering.io/v1/cards/" [ id ] []


cardsUrlByName : String -> String
cardsUrlByName name =
    Url.Builder.crossOrigin "https://api.magicthegathering.io/v1/cards" [] [ Url.Builder.string "name" name ]


getCardInfoFromName : String -> Cmd Msg
getCardInfoFromName name =
    Http.get
        { url = cardsUrlByName name
        , expect = Http.expectJson GotCardName cardsListHeadDecoder
        }


getCardInfoFromId : String -> Cmd Msg
getCardInfoFromId id =
    Http.get
        { url = cardsUrl id
        , expect = Http.expectJson GotCardName responseDecoder
        }


getCardListFromName : String -> Cmd Msg
getCardListFromName name =
    Http.get
        { url = cardsUrlByName name
        , expect = Http.expectJson GotCardList cardsListDecoder
        }


getCardImageFromName : String -> Cmd Msg
getCardImageFromName name =
    Http.get
        { url = cardsUrlByName name
        , expect = Http.expectJson GotCardImage cardsListHeadImgUrlDecoder
        }


responseDecoder : Decoder CardInfo
responseDecoder =
    map CardInfo
        (at
            [ "card", "name" ]
            Json.Decode.string
        )


cardsListHeadDecoder : Decoder CardInfo
cardsListHeadDecoder =
    map CardInfo (field "cards" (index 0 (field "name" string)))


cardsListHeadImgUrlDecoder : Decoder String
cardsListHeadImgUrlDecoder =
    field "cards" (index 0 (field "imageUrl" string))


cardsListDecoder : Decoder (List String)
cardsListDecoder =
    field "cards" (list (field "name" string))


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


cardNameButton : String -> Html Msg
cardNameButton item =
    div []
        [ button [ onClick (SelectCard item) ] [ text item ] ]


listItem : String -> Html Msg
listItem item =
    div []
        [ text item ]


view : Model -> Html Msg
view model =
    div []
        [ viewInput "text" "Id" (Maybe.withDefault "" model.searchName) GotSearchName
        , viewValidation model
        , img
            [ src <|
                Maybe.withDefault "" model.cardImageUrl
            , width 300
            ]
            []
        , div [] (List.map listItem model.draftedCards)
        , div [] (List.map cardNameButton model.searchResult)
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
