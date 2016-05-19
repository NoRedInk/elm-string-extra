module Tests (..) where

import ElmTest exposing (..)
import String.Extra exposing (toSentence)

all : Test
all =
  suite
    "toSentence"
    [ toSentence []
        |> assertEqual ""
        |> test "should be an empty string for empty lists"
    , toSentence ["lion"]
        |> assertEqual "lion"
        |> test "should be just a word for a list length 1"
    , toSentence ["lion", "tiger"]
        |> assertEqual "lion and tiger"
        |> test "should just give 'and' for list length 2"
    , toSentence ["lion", "tiger", "bear"]
        |> assertEqual "lion, tiger, and bear"
        |> test "should make a human readable list for list length 3"
    , toSentence ["lion", "tiger", "bear", "dog", "horse"]
        |> assertEqual "lion, tiger, bear, dog, and horse"
        |> test "should make a human readable list for list length n"
    ]

