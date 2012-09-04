module Printf where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

aif :: Q Exp -> Q Exp -> Q Exp -> Q Exp
--aif if' then' else' =
--  [| let temp = $if'
--         it = temp
--     in if temp /= 0 then $then' else $else' |]
aif if'' then'' else'' =
    do { temp <- newName "temp"
--       ; it <- newName "it"
       ; if' <- if''
       ; then' <- then''
       ; else' <- else''
       ; let notEq = mkNameG_v "ghc-prim" "GHC.Classes" "/="
             it = mkName "it"
         in return (LetE [ValD (VarP temp) (NormalB if') [],
                       ValD (VarP it) (NormalB (VarE temp)) []]
                      (CondE (InfixE (Just (VarE temp)) (VarE notEq) (Just (LitE (IntegerL 0)))) then' else'))
       }
