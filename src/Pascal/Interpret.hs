module Pascal.Interpret 
(
    interpret
)
where

import Pascal.Data
import qualified Data.Map.Strict()
import Prelude hiding (map) 
import Control.Monad.Cont
import Data.Traversable
import qualified Data.Map as Map

-- TODO: define auxiliary functions to aid interpretation
-- Feel free to put them here or in different modules
-- Hint: write separate evaluators for numeric and
-- boolean expressions and for statements
type Env = Map.Map String String

-- getAllocation :: String String
-- getAllocation = do
-- 	env <- get
-- 	let store1 = 
data Map k a  = Bin {-# UNPACK #-} !Size !k a !(Map k a) !(Map k a)
              | Tip

type Size     = Int

-- make sure you write test unit cases for all functions
map :: (a -> b) -> [a] -> [b]  
map _ [] = []  
map f (x:xs) = f x : map f xs  

interpret :: Program -> String
-- TODO: write the interpreter
--interpret (x) = show( map state x)
interpret (x) = show( map state x )

declare :: VarDecBlock ->String 
declare (VarDecBlock [varDec]) = "var dec"

variable :: VarDec -> String
variable (VarDec [text] tip) =  "var dec1"
variable (VarDef_Float text num) =  "var dec1"
variable (VarDef_True text) =  "var dec1"
variable (VarDef_False text) =  "var dec1"

-- putIntoMap:: String ->

-- putIntoMap :: String -> Map.Map String String
-- putIntoMap varName = Map.empty 

-- empty :: Map k a
-- empty = Tip

-- singleton :: k -> a -> Map k a
-- singleton k x = Bin 1 k x Tip Tip

-- lazy :: a -> a
-- lazy a = a 

myMap :: Integer -> Map.Map Integer [Integer] 
myMap n = Map.fromList (map makePair [1..n]) 
   where makePair x = (x, [x])  
--insert :: Ord k => k -> a -> Map k a -> Map k a
-- create :: Integer -> Map Integer [Integer] 
-- create v = myMap
-- myMap :: String -> Exp -> Map.Map String Exp -> Map.Map String Exp
-- myMap n x = Map.insert n x
   

state :: Statement -> String
state (AssignR var x) =  show(myMap 3)
state (AssignB var bool) = booExp bool
state (IfThen bool statement) = state statement
state (IfThenElse bool statement statement1) = state statement
state (WhileDo bool [statement]) = booExp bool
state  (ForDo text num num1 [statement]) = state statement
state  (Case exp [caseLabel]) = getExp exp
    -- write() with no parameters
state  (WriteNewLine) = "\n"
    -- write(stuff, inside) with parameters
state  (WriteInside [writeParam]) = "params"
    -- TODO: add function and procedure blocks
state  (Func text text1 [varDec] text2 statement text3) = state statement
state  (Proc text text1 [varDec] text2 statement text3) = state statement
state _ = "empty list"


getExp :: Exp -> String
getExp (Op1 "+" exp) = getExp exp
getExp (Op2 "+" (Real exp) (Real exp1)) = show (exp + exp1)
getExp (Op2 "*" (Real exp) (Real exp1)) = show (exp * exp1)
getExp (Op2 "-" (Real exp) (Real exp1)) = show (exp - exp1)
getExp (Op2 "/" (Real exp) (Real exp1)) = show (exp / exp1)
getExp (Op3 "sin" (Real op)) = show (sin op)
getExp (Op3 "cos" (Real op)) = show (cos (op)) 
getExp (Op3 "ln" (Real op)) = show (log(op))
getExp (Op3 "exp" (Real op)) = show op
getExp (Op3 "sqrt" (Real op)) = show (sqrt op)
getExp (FunCall text [exp]) = "function"
getExp (Real num) = show num
getExp (Var_R text) = show text
getExp _ = "not exp"


-- eval ::Exp ->String
-- eval (exp,exp1) = show exp


booExp :: BoolExp -> String
booExp (OpB text bool bool1) = show bool ++ (show bool1)
booExp (Not bool) = show bool
booExp (OpR text exp exp1) = show exp ++ (show exp1)
booExp (True_C) = "true var"
booExp (False_C) = "false var"
booExp (Var_B text) = show text
booExp _ = "not boo"

cases :: CaseLabel ->String
cases (CaseLabel exp statement)= state statement

print :: WriteParam -> String
print (PrintID text) = show text
print (PrintTrue) = "true"
print (PrintFalse) = "false"
print (PrintFloat num) = show num
print (PrintString text) = show text

-- getType :: Type -> Float
-- getType (REAL) 