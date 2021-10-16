module FileSystem.Path exposing (Path, Segment, codec, fromString, segmentCodec, segmentFromString, segmentToString, toComparable, toString)

import Codec


type alias Path =
    List Segment


fromString : String -> Path
fromString a =
    if String.isEmpty a then
        []

    else
        a |> String.split "/" |> List.filter (String.isEmpty >> not) |> List.map Segment


toString : Path -> String
toString a =
    a |> List.map segmentToString |> String.join "/"


toComparable : Path -> List String
toComparable a =
    a |> List.map segmentToString


codec : Codec.Codec Path
codec =
    Codec.list segmentCodec



--


type Segment
    = Segment String


segmentFromString : String -> Maybe Segment
segmentFromString a =
    if String.isEmpty a || String.contains "/" a then
        Nothing

    else
        Just (Segment a)


segmentToString : Segment -> String
segmentToString (Segment a) =
    a


segmentCodec : Codec.Codec Segment
segmentCodec =
    Codec.string
        |> Codec.andThen
            (\v ->
                case segmentFromString v of
                    Just v2 ->
                        Codec.succeed v2

                    Nothing ->
                        Codec.fail "Cannot decode path segment."
            )
            (\(Segment v) -> v)
