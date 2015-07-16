module Util where


import String


isSuffixOf = String.endsWith


slash : String -> String -> String
slash a b =
  if "/" `isSuffixOf` a
    then a ++ b
    else a ++ "/" ++ b
