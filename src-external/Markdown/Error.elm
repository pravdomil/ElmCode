module Markdown.Error exposing (..)

import Parser
import Parser.Advanced


type alias Error =
    List (Parser.Advanced.DeadEnd String Parser.Problem)
