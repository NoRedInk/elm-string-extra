module String.Extra where
{-| Convenience functions for working with Strings

# Formatting numbers
@docs pluralize

-}

import String


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
