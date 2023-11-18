port module Main exposing (..)

import Browser
import Element exposing (text)
import Element.Background as Background
import Element.Input as Input
import Html exposing (Html)
import Http
import Icons exposing (toHtml)
import Json.Decode as D
import Json.Encode as E
import Style exposing (Style(..), styling)
import Url.Builder



-- MAIN


main : Program E.Value Model Msg
main =
    Browser.element { init = init, update = updateWithStorage, subscriptions = subscriptions, view = view }



-- MODEL


type alias Model =
    { searchName : String
    , searchResult : List CardInfo
    , draftedCards : List CardInfo
    }


type alias CardInfo =
    { name : String, manaCost : Maybe String, cmc : Maybe Int, cardType : String }


getManaCostList : CardInfo -> List Char
getManaCostList cardInfo =
    cardInfo.manaCost
        |> Maybe.withDefault ""
        |> String.toList
        |> List.filter (\c -> c /= '{' || c /= '}')


init : E.Value -> ( Model, Cmd Msg )
init flags =
    ( case D.decodeValue modelDecoder flags of
        Ok model ->
            let
                _ =
                    Debug.log "success in flags decoding" flags
            in
            model

        Err err ->
            let
                _ =
                    Debug.log "decode error" err
            in
            { searchName = ""
            , searchResult = []
            , draftedCards = []
            }
    , Cmd.none
    )



-- UPDATE


type Msg
    = GotCardList (Result Http.Error (List CardInfo))
    | GotSearchName String
    | SelectCard CardInfo
    | ResetDeck


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ResetDeck ->
            ( { model | draftedCards = [] }, Cmd.none )

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
            ( { model | searchName = name }, resp )

        SelectCard name ->
            let
                draftList =
                    [ name ]
                        |> List.append model.draftedCards
                        |> List.sortBy
                            (\n ->
                                case n.cmc of
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


getCardListFromName : String -> Cmd Msg
getCardListFromName name =
    Http.get
        { url = cardsUrlByName name
        , expect = Http.expectJson GotCardList cardsListDecoder
        }



-- responseDecoder : D.Decoder CardInfo
-- responseDecoder =
--     D.map4 CardInfo
--         (D.at
--             [ "card", "name" ]
--             D.string
--         )
--         (D.at
--             [ "card", "manaCost" ]
--             (D.maybe D.string)
--         )
--         (D.at
--             [ "card", "cmc" ]
--             (D.maybe D.int)
--         )
--         (D.at
--             [ "card", "type" ]
--             D.string
--         )


cardsListHeadImgUrlDecoder : D.Decoder String
cardsListHeadImgUrlDecoder =
    D.field "cards" (D.index 0 (D.field "imageUrl" D.string))


cardsListDecoder : D.Decoder (List CardInfo)
cardsListDecoder =
    D.field "cards"
        (D.list
            cardInfoDecoder
        )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


icon : Element.Element msg
icon =
    Element.el
        [ Element.centerX
        , Element.centerY
        , Element.width <| Element.px 600
        ]
    <|
        Element.html (toHtml <| Icons.numberedCircle 1)


cardNameButton : CardInfo -> Element.Element Msg
cardNameButton card =
    Input.button [] { onPress = Just <| SelectCard card, label = mainText card.name }


resetButton : Element.Element Msg
resetButton =
    Input.button [] { onPress = Just <| ResetDeck, label = mainText "Reset deck" }


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
        , mainText (String.fromInt (Maybe.withDefault 0 card.cmc))
        , mainText "  -  "
        , mainText <| Maybe.withDefault "" card.manaCost
        ]


view : Model -> Html Msg
view model =
    Element.layout [ Background.color <| Style.colorPaletteToColor Style.PrimaryDark ] <|
        Element.column
            [ Element.width Element.fill, Element.padding 10, Element.spacing 7 ]
            [ h1 "MTG Drafter"
            , resetButton
            , icon
            , Element.row [ Element.padding 10, Element.spacing 7 ]
                [ Element.column [ Element.padding 10, Element.spacing 7, Element.alignTop ]
                    [ Input.text [ Element.width <| Element.px 300 ]
                        { placeholder = Just (Input.placeholder [] (mainText "type more than 4 chars"))
                        , onChange = GotSearchName
                        , text = model.searchName
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
-- PORTS


port setStorage : E.Value -> Cmd msg



-- We want to `setStorage` on every update, so this function adds
-- the setStorage command on each step of the update function.
--
-- Check out index.html to see how this is handled on the JS side.
--


updateWithStorage : Msg -> Model -> ( Model, Cmd Msg )
updateWithStorage msg oldModel =
    let
        ( newModel, cmds ) =
            update msg oldModel

        _ =
            Debug.log "updated" E.encode 4 (encode newModel)
    in
    ( newModel
    , Cmd.batch [ setStorage (encode newModel), cmds ]
    )



-- JSON ENCODE/DECODE
-- type alias CardInfo =
--     { name : String, manaCost : Maybe String, cmc : Maybe Int, cardType : String }


encodeCardInfo : CardInfo -> E.Value
encodeCardInfo cardInfo =
    E.object
        [ ( "name", E.string cardInfo.name )
        , ( "manaCost", E.string <| Maybe.withDefault "" cardInfo.manaCost )
        , ( "cmc", E.int <| Maybe.withDefault 0 cardInfo.cmc )
        , ( "type", E.string cardInfo.cardType )
        ]


encode : Model -> E.Value
encode model =
    E.object
        [ ( "searchName", E.string model.searchName )
        , ( "searchResult", E.list encodeCardInfo model.searchResult )
        , ( "draftedCards", E.list encodeCardInfo model.draftedCards )
        ]


cardInfoDecoder : D.Decoder CardInfo
cardInfoDecoder =
    D.map4 CardInfo
        (D.field "name" D.string)
        (D.maybe (D.field "manaCost" D.string))
        (D.maybe (D.field "cmc" D.int))
        (D.field "type" D.string)



-- cardInfoFromStorageDecoder : D.Decoder CardInfo
-- cardInfoFromStorageDecoder =
--     D.map4 CardInfo
--         (D.field "name" D.string)
--         (D.maybe (D.field "manaCost" D.string))
--         (D.maybe (D.field "cmc" D.int))
--         (D.field "cardType" D.string)
-- type alias Model =
--     { searchName : String
--     , searchResult : List CardInfo
--     , draftedCards : List CardInfo
--     }


modelDecoder : D.Decoder Model
modelDecoder =
    D.map3 Model
        (D.field "searchName" D.string)
        (D.field "searchResult" <| D.list cardInfoDecoder)
        (D.field "draftedCards" <| D.list cardInfoDecoder)
