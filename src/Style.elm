module Style exposing (ColorPalette(..), MyFonts(..), Style(..), colorPalette, fontStack, styling)

import Element exposing (..)
import Element.Font as Font


type Style
    = Header
    | Subheader
    | MainText
    | SubText
    | Link


styling : Style -> List (Element.Attribute msg)
styling style =
    case style of
        Header ->
            [ Font.color (colorPalette PrimaryLight)
            , Font.size 36
            , Font.family
                [ fontStack Headline
                , fontStack Fallback
                ]
            ]

        Subheader ->
            [ Font.color (colorPalette PrimaryLight)
            , Font.size 22
            , Font.heavy
            , Font.family
                [ fontStack PrimarySans
                , fontStack Headline
                , fontStack Fallback
                ]
            ]

        MainText ->
            [ Font.color (colorPalette PrimaryLight)
            , Font.size 18
            , Font.medium
            , Font.family
                [ fontStack PrimarySans
                , fontStack Fallback
                ]
            ]

        Link ->
            [ Font.color (colorPalette PrimaryLight)
            , Font.size 18
            , Font.medium
            , Font.family
                [ fontStack PrimarySans
                , fontStack Fallback
                ]
            , Element.mouseOver [ Font.color <| colorPalette Yellow ]
            ]

        SubText ->
            [ Font.color (colorPalette PrimaryLight)
            , Font.size 16
            , Font.light
            , Font.family
                [ fontStack PrimarySans
                , fontStack Fallback
                ]
            ]


type MyFonts
    = Headline
    | PrimarySans
    | SecondarySans
    | PrimarySerif
    | SecondarySerif
    | Fallback


type ColorPalette
    = Primary
    | PrimaryDark
    | PrimaryLight
    | SecondaryDark
    | Red
    | Teal
    | Yellow


fontStack : MyFonts -> Font.Font
fontStack font =
    case font of
        Headline ->
            Font.external { url = "https://fonts.googleapis.com/css?family=Dosis", name = "dosis" }

        PrimarySans ->
            Font.external { url = "https://fonts.googleapis.com/css?family=Roboto", name = "roboto" }

        SecondarySans ->
            Font.sansSerif

        PrimarySerif ->
            Font.external { url = "https://fonts.googleapis.com/css?family=Esteban", name = "esteban" }

        SecondarySerif ->
            Font.serif

        Fallback ->
            Font.sansSerif


colorPalette : ColorPalette -> Element.Color
colorPalette color =
    case color of
        Primary ->
            rgba255 100 100 100 1

        --96 125 139 1
        PrimaryDark ->
            rgba255 45 45 42 1

        PrimaryLight ->
            rgba255 200 200 200 1

        SecondaryDark ->
            rgba255 76 76 71 1

        Red ->
            rgba255 193 73 83 1

        Teal ->
            rgba255 96 150 150 1

        -- #EFBB67
        Yellow ->
            rgba255 239 187 103 1
