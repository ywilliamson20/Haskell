module Pascal.Interpret 
(
    interpret
)
where

import Pascal.Data
import qualified Data.Map.Strict()
import Prelude hiding (map) 

-- TODO: define auxiliary functions to aid interpretation
-- Feel free to put them here or in different modules
-- Hint: write separate evaluators for numeric and
-- boolean expressions and for statements

-- make sure you write test unit cases for all functions
map :: (a -> b) -> [a] -> [b]  
map _ [] = []  
map f (x:xs) = f x : map f xs  

interpret :: Program -> String
-- TODO: write the interpreter

--interpret (x:y:tl) = ( "hi" ++ (show x)++(show y)) |>
--interpret (AssignR "hi" x) = "hello" ++ (show x) 
interpret (x:tl) = state x
interpret [] = "empty"


state :: Statement -> String
state (AssignR var x) = getExp x
state (AssignB var bool) = booExp bool
state (IfThen bool statement) = state statement
-- state (IfThen bool statement statement) = "hi"
-- state (WhileDo bool [list]) = "hi"
-- state (AssignR String Exp) = "hi"
-- state (AssignR String Exp) = "hi"
-- state x = "howdy" ++ (show x)
state _ = "empty list"


getExp :: Exp -> String
getExp (Op2 "+" exp exp1) = getExp exp 
getExp (Op2 "*" exp exp1) = show exp ++ (show exp1)
getExp (Op2 "-" exp exp1) = show exp ++ (show exp1)
getExp (Op2 "/" exp exp1) = show exp ++ (show exp1)
getExp (Op3 "sin" op) = show op
getExp (Op3 "cos" op) = show op 
getExp (Op3 "ln" op) = show op
getExp (Op3 "exp" op) = show op
getExp (Op3 "sqrt" op) = show op
getExp (FunCall text [exp]) = "function"
getExp (Real num) = show num
getExp (Var_R text) = show text
getExp _ = "not exp"


booExp :: BoolExp -> String
booExp (OpB text bool bool1) = show bool ++ (show bool1)
booExp (Not bool) = show bool
booExp (OpR text exp exp1) = show exp ++ (show exp1)
booExp (True_C) = "true"
booExp (False_C) = "false"
booExp (Var_B text) = show text
booExp _ = "not boo"
