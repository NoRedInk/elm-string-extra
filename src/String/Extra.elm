module String.Extra (pluralize, isWhitespace) where

{-| Convenience functions for working with Strings

# Formatting
@docs capitalize pluralize

# Whitespace
@docs isWhitespace

-}

import Regex


capitalize str =
  case uncons str of
    Nothing ->
      str

    ( firstLetter, rest ) ->
      String.cons (String.toUpper firstLetter) rest


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


isWhitespaceRegex =
  Regex.regex "^\\s+$"
