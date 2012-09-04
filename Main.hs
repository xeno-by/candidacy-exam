module Main where

import Printf(aif)
import Language.Haskell.TH

main = $(aif [| calculate |]
  [| putStrLn (show $(dyn "it")) |]
  [| error "does not compute" |])
      where calculate = 42