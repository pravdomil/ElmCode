module Elm.Project.Utils exposing (..)

import Elm.Project
import FileSystem.Path


sourceDirectories : Elm.Project.Project -> List FileSystem.Path.Path
sourceDirectories a =
    case a of
        Elm.Project.Application b ->
            b.dirs |> List.map FileSystem.Path.fromString

        Elm.Project.Package _ ->
            [ FileSystem.Path.fromString "src"
            ]
