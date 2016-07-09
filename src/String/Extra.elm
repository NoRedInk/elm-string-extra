module String.Extra exposing (pluralize, capitalize, toSentence, isWhitespace)

{-| Convenience functions for working with Strings

# Formatting
@docs capitalize, pluralize, toSentence

# Whitespace
@docs isWhitespace

-}

import Regex exposing (Regex)
import String
import Char


{-| Capitalize or uncapitalize the given string.

    capitalize True "foo"
    -- "Foo"

    capitalize False "BAR"
    -- "bAR"
-}
capitalize : Bool -> String -> String
capitalize shouldCapitalize str =
  case String.uncons str of
    Nothing ->
      str

    Just ( firstLetter, rest ) ->
      let
        newFirstLetter =
          if shouldCapitalize then
            Char.toUpper firstLetter
          else
            Char.toLower firstLetter
      in
        String.cons newFirstLetter rest


{-| Given a number, a singular string, and a plural string, returns the number
followed by a space, followed by either the singular string if the number was 1,
or the plural string otherwise.

    pluralize "elf" "elves" 2 == "2 elves"
    pluralize "elf" "elves" 1 == "1 elf"
    pluralize "elf" "elves" 0 == "0 elves"

-}
pluralize : String -> String -> number -> String
pluralize singular plural count =
  if count == 1 then
    "1 " ++ singular
  else
    (toString count) ++ " " ++ plural


{-| Converts a list of strings into a human formatted readable list


    toSentence [] ---> ""
    toSentence ["lions"] ---> "lions"
    toSentence ["lions", "tigers"] --> "lions and tigers"
    toSentence ["lions", "tigers", "bears"] --> "lions, tigers, and bears"

notes:
* It *DOES* include an oxford comma
* It *DOES NOT* include a period
* It *DOES NOT* include the phrase "...oh my!"

-}
toSentence : List String -> String
toSentence list =
  case list of
    [] ->
      ""

    x :: [] ->
      x

    x :: y :: [] ->
      x ++ " and " ++ y

    x :: y :: more ->
      toSentenceHelper (x ++ ", " ++ y) more


toSentenceHelper : String -> List String -> String
toSentenceHelper sentence list =
  case list of
    [] ->
      sentence ++ ""

    x :: [] ->
      sentence ++ ", and " ++ x

    x :: xs ->
      toSentenceHelper (sentence ++ ", " ++ x) xs


{-| Returns True iff the given String is 1 or more whitespace characters,
and nothing else.

(Whitespace is defined as the regular expression `\s` matcher.)

    isWhitespace ""   == False
    isWhitespace " "  == True
    isWhitespace "  " == True
    isWhitespace " x" == False
    isWhitespace "x " == False
-}
isWhitespace : String -> Bool
isWhitespace =
  Regex.contains isWhitespaceRegex


isWhitespaceRegex : Regex
isWhitespaceRegex =
  Regex.regex "^\\s+$"
