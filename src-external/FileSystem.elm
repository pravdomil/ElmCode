module FileSystem exposing (..)

{-| <https://developer.mozilla.org/en-US/docs/Web/API/FileSystemHandle>
-}

import Codec
import File
import FileSystem.Path
import JavaScript
import Json.Decode
import Json.Encode
import Task


type FileOrDirectory
    = File_ File
    | Directory_ Directory


type Error
    = NotSupported
    | FileNotFound
    | SelectAborted
    | JavaScriptError JavaScript.Error



--


name : FileOrDirectory -> Maybe FileSystem.Path.Segment
name a =
    let
        b : Json.Decode.Value
        b =
            case a of
                File_ (File c) ->
                    c

                Directory_ (Directory c) ->
                    c
    in
    b
        |> Json.Decode.decodeValue (Json.Decode.field "name" Json.Decode.string)
        |> Result.toMaybe
        |> Maybe.andThen FileSystem.Path.segmentFromString



--


{-| <https://developer.mozilla.org/en-US/docs/Web/API/FileSystemFileHandle>
-}
type File
    = File Json.Decode.Value


createFile : Directory -> FileSystem.Path.Segment -> Task.Task Error File
createFile (Directory dir) segment =
    JavaScript.run "a.dir.getFileHandle(a.segment, { create: true })"
        (Json.Encode.object
            [ ( "dir", dir )
            , ( "segment", Codec.encoder FileSystem.Path.segmentCodec segment )
            ]
        )
        (Json.Decode.map File Json.Decode.value)
        |> Task.mapError toError


updateFile : String -> File -> Task.Task Error ()
updateFile data (File a) =
    JavaScript.run "a.file.createWritable().then(b => b.write(a.data).then(() => b.close()))"
        (Json.Encode.object
            [ ( "data", Json.Encode.string data )
            , ( "file", a )
            ]
        )
        (Json.Decode.succeed ())
        |> Task.mapError toError


toFile : File -> Task.Task Error File.File
toFile (File a) =
    JavaScript.run "a.getFile()"
        a
        File.decoder
        |> Task.mapError toError



--


{-| <https://developer.mozilla.org/en-US/docs/Web/API/FileSystemDirectoryHandle>
-}
type Directory
    = Directory Json.Decode.Value


createDirectory : Directory -> FileSystem.Path.Segment -> Task.Task Error Directory
createDirectory (Directory dir) segment =
    JavaScript.run "a.dir.getDirectoryHandle(a.segment, { create: true })"
        (Json.Encode.object
            [ ( "dir", dir )
            , ( "segment", Codec.encoder FileSystem.Path.segmentCodec segment )
            ]
        )
        (Json.Decode.map Directory Json.Decode.value)
        |> Task.mapError toError


listDirectory : Directory -> Task.Task Error (List FileSystem.Path.Segment)
listDirectory (Directory dir) =
    JavaScript.run "(async () => { var acc = []; for await (var b of a.keys()) acc.push(b); return acc; })()"
        dir
        (Json.Decode.list (Codec.decoder FileSystem.Path.segmentCodec))
        |> Task.mapError toError



--


read : Directory -> FileSystem.Path.Path -> Task.Task Error FileOrDirectory
read dir path =
    let
        fn : FileSystem.Path.Segment -> Task.Task Error FileOrDirectory -> Task.Task Error FileOrDirectory
        fn b acc =
            acc
                |> Task.andThen
                    (\v ->
                        case v of
                            Directory_ (Directory dir_) ->
                                let
                                    readFile_ : Task.Task Error File
                                    readFile_ =
                                        JavaScript.run "a.dir.getFileHandle(a.segment)"
                                            (Json.Encode.object
                                                [ ( "dir", dir_ )
                                                , ( "segment", Codec.encoder FileSystem.Path.segmentCodec b )
                                                ]
                                            )
                                            (Json.Decode.map File Json.Decode.value)
                                            |> Task.mapError toError

                                    readDirectory_ : Task.Task Error Directory
                                    readDirectory_ =
                                        JavaScript.run "a.dir.getDirectoryHandle(a.segment)"
                                            (Json.Encode.object
                                                [ ( "dir", dir_ )
                                                , ( "segment", Codec.encoder FileSystem.Path.segmentCodec b )
                                                ]
                                            )
                                            (Json.Decode.map Directory Json.Decode.value)
                                            |> Task.mapError toError
                                in
                                readFile_
                                    |> Task.map File_
                                    |> Task.onError
                                        (\_ ->
                                            readDirectory_
                                                |> Task.map Directory_
                                        )

                            File_ _ ->
                                Task.fail FileNotFound
                    )
    in
    path |> List.foldl fn (Task.succeed (Directory_ dir))


delete : Directory -> FileSystem.Path.Segment -> Task.Task Error ()
delete (Directory dir) segment =
    JavaScript.run "a.dir.removeEntry(a.segment)"
        (Json.Encode.object
            [ ( "dir", dir )
            , ( "segment", Codec.encoder FileSystem.Path.segmentCodec segment )
            ]
        )
        (Json.Decode.succeed ())
        |> Task.mapError toError



--


select : Task.Task Error Directory
select =
    JavaScript.run "showDirectoryPicker()"
        Json.Encode.null
        (Json.Decode.map Directory Json.Decode.value)
        |> Task.mapError toError



--


toError : JavaScript.Error -> Error
toError a =
    case a of
        JavaScript.Exception "ReferenceError" _ _ ->
            NotSupported

        JavaScript.Exception _ "8" _ ->
            FileNotFound

        JavaScript.Exception _ "20" _ ->
            SelectAborted

        _ ->
            JavaScriptError a
