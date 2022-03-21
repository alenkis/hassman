-- |
module Theme where

import           System.Console.Pretty (Color (..), Pretty, Style (..), bgColor,
                                        color, style, supportsPretty)

success :: Pretty a => a -> a
success = color Green

info :: Pretty a => a -> a
info = color Yellow

danger :: Pretty a => a -> a
danger = color Red
