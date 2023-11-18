module Style exposing (ColorPalette(..), MyFonts(..), Style(..), colorPaletteToColor, colorPaletteToHex, fontStack, styling)

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
            [ Font.color (colorPaletteToColor PrimaryLight)
            , Font.size 36
            , Font.family
                [ fontStack Headline
                , fontStack Fallback
                ]
            ]

        Subheader ->
            [ Font.color (colorPaletteToColor PrimaryLight)
            , Font.size 22
            , Font.heavy
            , Font.family
                [ fontStack PrimarySans
                , fontStack Headline
                , fontStack Fallback
                ]
            ]

        MainText ->
            [ Font.color (colorPaletteToColor PrimaryLight)
            , Font.size 18
            , Font.medium
            , Font.family
                [ fontStack PrimarySans
                , fontStack Fallback
                ]
            ]

        Link ->
            [ Font.color (colorPaletteToColor PrimaryLight)
            , Font.size 18
            , Font.medium
            , Font.family
                [ fontStack PrimarySans
                , fontStack Fallback
                ]
            , Element.mouseOver [ Font.color <| colorPaletteToColor Yellow ]
            ]

        SubText ->
            [ Font.color (colorPaletteToColor PrimaryLight)
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
    | MtgRed
    | MtgBlue
    | MtgWhite
    | MtgBlack
    | MtgGreen


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


colorPaletteToColor : ColorPalette -> Element.Color
colorPaletteToColor color =
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

        Yellow ->
            rgba255 239 187 103 1

        MtgRed ->
            rgba255 211 32 42 1

        MtgBlue ->
            rgba255 14 104 171 1

        MtgWhite ->
            rgba255 249 250 244 1

        MtgBlack ->
            rgba255 21 11 0 1

        MtgGreen ->
            rgba255 0 115 62 1


colorPaletteToHex : ColorPalette -> String
colorPaletteToHex color =
    case color of
        Primary ->
            "#646464"

        PrimaryDark ->
            "#2D2D2A"

        PrimaryLight ->
            "#C8C8C8"

        SecondaryDark ->
            "#4C4C47"

        Red ->
            "#C14953"

        Teal ->
            "#609696"

        Yellow ->
            "#EFBB67"

        MtgRed ->
            "#D3202A"

        MtgBlue ->
            "#0E68AB"

        MtgWhite ->
            "#F9FAF4"

        MtgBlack ->
            "#150B00"

        MtgGreen ->
            "#00733E"
