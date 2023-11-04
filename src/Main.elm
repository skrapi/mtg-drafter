module Main exposing (..)

import Browser
import Element exposing (text)
import Element.Background as Background
import Element.Input as Input
import Html exposing (Html)
import Http
import Json.Decode exposing (Decoder, at, field, index, list, map4, string)
import Style exposing (Style(..), styling)
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
    , searchResult : List CardInfo
    , draftedCards : List CardInfo
    }


type alias CardInfo =
    { name : String, manaCost : Maybe String, convertedManaCost : Maybe Int, cardType : String }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { searchName = Nothing, cardInfo = Nothing, searchResult = [], cardImageUrl = Nothing, draftedCards = [] }, Cmd.none )



-- UPDATE


type Msg
    = GotCardName (Result Http.Error CardInfo)
    | GotCardList (Result Http.Error (List CardInfo))
    | GotCardImage (Result Http.Error String)
    | GotSearchName String
    | SelectCard CardInfo


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotCardName result ->
            case result of
                Ok cardInfo ->
                    ( { model | cardInfo = Just cardInfo }, Cmd.none )

                Err _ ->
                    ( { model | cardInfo = Nothing }, Cmd.none )

        GotCardImage result ->
            case result of
                Ok cardImageUrl ->
                    ( { model | cardImageUrl = Just cardImageUrl }, Cmd.none )

                Err _ ->
                    ( { model | cardInfo = Nothing }, Cmd.none )

        GotCardList result ->
            let
                _ =
                    Debug.log "result of card list" result
            in
            case result of
                Ok cardList ->
                    ( { model | searchResult = unique cardList }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        GotSearchName name ->
            let
                _ =
                    Debug.log "search name" name

                resp =
                    if String.length name > 4 then
                        getCardListFromName name

                    else
                        Cmd.none
            in
            ( { model | searchName = Just name }, resp )

        SelectCard name ->
            let
                draftList =
                    [ name ]
                        |> List.append model.draftedCards
                        |> List.sortBy
                            (\n ->
                                case n.convertedManaCost of
                                    Just num ->
                                        if num == 0 && n.cardType == "Land" then
                                            -1

                                        else
                                            num

                                    Nothing ->
                                        -1
                            )
            in
            ( { model | draftedCards = draftList }, Cmd.none )


unique : List a -> List a
unique list =
    List.foldl
        (\a uniques ->
            if List.member a uniques then
                uniques

            else
                uniques ++ [ a ]
        )
        []
        list


cardsUrl : String -> String
cardsUrl id =
    Url.Builder.crossOrigin "https://api.magicthegathering.io/v1/cards/" [ id ] []


cardsUrlByName : String -> String
cardsUrlByName name =
    Url.Builder.crossOrigin "https://api.magicthegathering.io/v1/cards" [] [ Url.Builder.string "name" name ]


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
    map4 CardInfo
        (at
            [ "card", "name" ]
            Json.Decode.string
        )
        (at
            [ "card", "manaCost" ]
            (Json.Decode.maybe Json.Decode.string)
        )
        (at
            [ "card", "cmc" ]
            (Json.Decode.maybe Json.Decode.int)
        )
        (at
            [ "card", "type" ]
            Json.Decode.string
        )


cardsListHeadImgUrlDecoder : Decoder String
cardsListHeadImgUrlDecoder =
    field "cards" (index 0 (field "imageUrl" string))


cardsListDecoder : Decoder (List CardInfo)
cardsListDecoder =
    field "cards"
        (list
            (map4 CardInfo
                (field "name" string)
                (Json.Decode.maybe (field "manaCost" string))
                (Json.Decode.maybe (field "cmc" Json.Decode.int))
                (field "type" string)
            )
        )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


cardNameButton : CardInfo -> Element.Element Msg
cardNameButton card =
    Input.button [] { onPress = Just <| SelectCard card, label = mainText card.name }


h1 : String -> Element.Element msg
h1 val =
    Element.el (styling Style.Header)
        (text val)


mainText : String -> Element.Element msg
mainText val =
    Element.el (styling MainText)
        (text val)


cardDisplay : CardInfo -> Element.Element msg
cardDisplay card =
    Element.row [ Element.width Element.fill ]
        [ mainText card.name
        , mainText "  -  "
        , mainText (String.fromInt (Maybe.withDefault 0 card.convertedManaCost))
        , mainText "  -  "
        , mainText <| Maybe.withDefault "" card.manaCost
        ]


view : Model -> Html Msg
view model =
    Element.layout [ Background.color <| Style.colorPalette Style.PrimaryDark ] <|
        Element.column
            [ Element.width Element.fill, Element.padding 10, Element.spacing 7 ]
            [ h1 "MTG Drafter"
            , Element.row [ Element.padding 10, Element.spacing 7 ]
                [ Element.column [ Element.padding 10, Element.spacing 7, Element.alignTop ]
                    [ Input.text [ Element.width <| Element.px 300 ]
                        { placeholder = Just (Input.placeholder [] (mainText "type more than 4 chars"))
                        , onChange = GotSearchName
                        , text = Maybe.withDefault "" model.searchName
                        , label = Input.labelLeft [] (mainText "Search")
                        }
                    , Element.column [ Element.padding 10, Element.spacing 7 ] (List.map cardNameButton model.searchResult)
                    ]
                , Element.column [ Element.width <| Element.px 300, Element.padding 10, Element.spacing 7, Element.alignTop ]
                    [ mainText "Drafted Cards"
                    , Element.column [ Element.padding 10, Element.spacing 7 ]
                        (List.map cardDisplay
                            model.draftedCards
                        )
                    ]
                ]
            ]



-- , img
--     [ src <|
--         Maybe.withDefault "" model.cardImageUrl
--     , width 300
--     ]
--     []
