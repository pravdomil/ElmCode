module Markdown.Codec exposing (..)

import Codec
import Markdown.Block


headingLevel : Codec.Codec Markdown.Block.HeadingLevel
headingLevel =
    Codec.custom
        (\fn1 fn2 fn3 fn4 fn5 fn6 v ->
            case v of
                Markdown.Block.H1 ->
                    fn1

                Markdown.Block.H2 ->
                    fn2

                Markdown.Block.H3 ->
                    fn3

                Markdown.Block.H4 ->
                    fn4

                Markdown.Block.H5 ->
                    fn5

                Markdown.Block.H6 ->
                    fn6
        )
        |> Codec.variant0 "H1" Markdown.Block.H1
        |> Codec.variant0 "H2" Markdown.Block.H2
        |> Codec.variant0 "H3" Markdown.Block.H3
        |> Codec.variant0 "H4" Markdown.Block.H4
        |> Codec.variant0 "H5" Markdown.Block.H5
        |> Codec.variant0 "H6" Markdown.Block.H6
        |> Codec.buildCustom
