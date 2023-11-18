module Icons exposing (..)

import Html
import Style exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)


type Icon
    = -- `Never` is used here so that our Icon type doesn't need a type hole. Essentially, the `Never` is saying "this kind of Svg cannot produce messages ever"
      Icon (Svg Never)


toHtml : Icon -> Html.Html msg
toHtml (Icon icon) =
    -- "Html.map never" transforms `Svg msg` into `Svg Never`
    Html.map never icon


circle : ColorPalette -> Icon
circle color =
    let
        size =
            "20"

        rad =
            "8"

        pos =
            "10"
    in
    svg
        [ width size
        , height size
        , viewBox <| String.join " " [ "0", "0", size, size ]
        ]
        [ Svg.circle
            -- TODO remove this magic number, it should be primary dark
            [ fill <| colorPaletteToHex color

            -- TODO remove this magic number, it should be yellow
            , stroke <| colorPaletteToHex color
            , strokeWidth "1"
            , cy pos
            , cx pos
            , r rad
            ]
            []
        ]
        |> Icon


numberedCircle : Int -> Icon
numberedCircle num =
    let
        size =
            "20"

        rad =
            "40%"

        pos =
            "50%"
    in
    svg
        [ width size
        , height size
        , viewBox <| String.join " " [ "0", "0", size, size ]
        ]
        [ Svg.circle
            -- TODO remove this magic number, it should be primary dark
            [ fill <| colorPaletteToHex MtgWhite
            , cy pos
            , cx pos
            , r rad
            ]
            []
        , Svg.text_ [ x pos, y pos, textAnchor "middle", dominantBaseline "middle" ] [ Svg.text <| String.fromInt num ]
        ]
        |> Icon
