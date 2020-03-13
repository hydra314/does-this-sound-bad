module DoesThisSoundBad where

import Data.Map (Map,fromList,lookup,insert)
import Data.Maybe (fromJust)
import Data.Char
import Prelude hiding (lookup)


--
-- *    Abstract Syntax
--

-- |    Variables
type Var = String

-- |    Expressions include literals, arithmetic expressions, comparisons, and variable references
--     
--      Abstract syntax:
--
--      expr ::= int
--             | bool
--             | string
--             | tuple
--             | var
--             | expr + expr
--             | expr - expr
--             | expr * expr
--             | expr / expr
--             | expr == expr
--             | expr != expr
--             | expr > expr
--             | expr >= expr
--             | expr < expr
--             | expr <= expr
--             | `not` expr
--             | `define` func var expr
--             | `call` expr expr
data Expr = I Int
          | B Bool
          | S String
          | T Expr Expr
          | V Var
          | ADD Expr Expr
          | SUB Expr Expr
          | MUL Expr Expr
          | DIV Expr Expr
          | EQU Expr Expr
          | NEQ Expr Expr
          | GRT Expr Expr
          | GTE Expr Expr
          | LST Expr Expr
          | LTE Expr Expr
          | NOT Expr
          | Def Var Expr
          | Call Expr Expr
    deriving(Eq,Show)

-- |    Commands include if/else conditionals, loops, function definitions and calls, variable assignments, and command blocks
--
--      Abstract syntax:
--      
--      cmd ::= `if` expr cmd `else` cmd
--            | `while` expr cmd
--            | `assign` var expr
--            | { cmd * }
data Cmd = If Expr Cmd Cmd
         | While Expr Cmd
         | Assign Var Expr
         | Block [Cmd]
    deriving(Eq,Show)

-- |    Abstract syntax of types.
--     
--      type  ::=  `int`  |  `bool`
--
data Type = TInt | TBool | TString | TTuple
  deriving (Eq,Show)

-- |    Abstract syntax of declarations.
--
--      decl  ::=  var : type
--
type Decl = (Var,Type)

-- |    Abstract syntax of programs.
--
--      prog  ::=  decl* `begin` cmd
--
data Prog = P [Decl] Cmd
  deriving (Eq,Show)


--
-- *    Type System
--

-- | Variable environments. An environment maps variable names to the
--   things those variable names are bound to. During typing, each name
--   will be bound to a type, while during evaluation (semantics), each
--   name will be bound to a value.

type Env a = Map Var a

-- | Gets the true type of an Expr variable. Uses Maybe so it can
--   throw an error in the case of an unsupported type.
typeExpr :: Expr -> Env Type -> Maybe Type
typeExpr (I _)     _ = Just TInt
typeExpr (B _)     _ = Just TBool
typeExpr (T _ _)   _ = Just TTuple
typeExpr (ADD l r) m = case (typeExpr l m, typeExpr r m) of
                            (Just TInt, Just TInt) -> Just TInt
                            _                      -> Nothing
typeExpr (SUB l r) m = case (typeExpr l m, typeExpr r m) of
                            (Just TInt, Just TInt) -> Just TInt
                            _                      -> Nothing
typeExpr (MUL l r) m = case (typeExpr l m, typeExpr r m) of
                            (Just TInt, Just TInt) -> Just TInt
                            _                      -> Nothing
typeExpr (DIV l r) m = case (typeExpr l m, typeExpr r m) of
                            (Just TInt, Just TInt) -> Just TInt
                            _                      -> Nothing
typeExpr (EQU l r) m = case (typeExpr l m, typeExpr r m) of
                            (Just TInt, Just TInt) -> Just TBool
                            _                      -> Nothing
typeExpr (NEQ l r) m = case (typeExpr l m, typeExpr r m) of
                            (Just TInt, Just TInt) -> Just TBool
                            _                      -> Nothing                                                        
typeExpr (GRT l r) m = case (typeExpr l m, typeExpr r m) of
                            (Just TInt, Just TInt) -> Just TBool
                            _                      -> Nothing
typeExpr (GTE l r) m = case (typeExpr l m, typeExpr r m) of
                            (Just TInt, Just TInt) -> Just TBool
                            _                      -> Nothing
typeExpr (LST l r) m = case (typeExpr l m, typeExpr r m) of
                            (Just TInt, Just TInt) -> Just TBool
                            _                      -> Nothing
typeExpr (LTE l r) m = case (typeExpr l m, typeExpr r m) of
                            (Just TInt, Just TInt) -> Just TBool
                            _                      -> Nothing
typeExpr (NOT e)   m = case typeExpr e m of
                            Just TBool -> Just TBool
                            _          -> Nothing
typeExpr (V v)     m = lookup v m


-- | Checks the parameters of a Cmd.
--   Returns true if they're valid and false if they're not.
typeCmd :: Cmd -> Env Type -> Bool
typeCmd (If x t f) m = case typeExpr x m of
                                       Just TBool -> typeCmd t m && typeCmd f m
                                       _          -> False
typeCmd (While x w)     m = case typeExpr x m of
                                       Just TBool -> typeCmd w m
                                       _          -> False
typeCmd (Assign v e)          m = case (lookup v m, typeExpr e m) of
                                       (Just tv, Just te) -> tv == te
                                       _                  -> False
typeCmd (Block ss)            m = all (\s -> typeCmd s m) ss


-- | Type checking programs. The 'fromList' function is from the
--   Data.Map module. It builds a map from a list of pairs, thus
--   initializing our typing environment.
typeProg :: Prog -> Bool
typeProg (P ds s) = typeCmd s (fromList ds)


--
-- *    Semantics
-- 

-- Basic semantic building block of the language
type Val = Either Int Bool

-- Evaluates an Expression and returns an Int or Bool.
evalExpr :: Expr -> Env Val -> Val
evalExpr (I n) _ = Left n
evalExpr (B b) _ = Right b
evalExpr (ADD l r) m = Left (unpackInt l m + unpackInt r m)
evalExpr (SUB l r) m = Left (unpackInt l m - unpackInt r m)
evalExpr (MUL l r) m = Left (unpackInt l m * unpackInt r m)
evalExpr (DIV l r) m = Left (unpackInt l m `div` unpackInt r m)
evalExpr (EQU l r) m = Right (unpackInt l m == unpackInt r m)
evalExpr (NEQ l r) m = Right (unpackInt l m /= unpackInt r m)
evalExpr (GRT l r) m = Right (unpackInt l m > unpackInt r m)
evalExpr (GTE l r) m = Right (unpackInt l m >= unpackInt r m)
evalExpr (LST l r) m = Right (unpackInt l m < unpackInt r m)
evalExpr (LTE l r) m = Right (unpackInt l m <= unpackInt r m)
evalExpr (NOT x)   m = Right (not (unpackBool x m))
evalExpr (V x)     m = case lookup x m of
                        Just v -> v
                        Nothing -> error "Error: undefined variable"
evalExpr (Def v x)  m = evalExpr x m
evalExpr (Call x y) m = evalExpr x m

-- Unpacks an Int from an Expression.
-- Can be the result of an arithmetic expression,
-- or just a literal Int
unpackInt :: Expr -> Env Val -> Int
unpackInt x m = case evalExpr x m of
                    Left i  -> i
                    Right _ -> error "Error: expected Int, got Bool instead"

-- Unpacks a Bool from an Expression.
-- Can be a comparison or a literal Bool.
unpackBool :: Expr -> Env Val -> Bool
unpackBool x m = case evalExpr x m of
                    Right b -> b 
                    Left _  -> error "Error: expected Bool, got Int instead"

-- Executes a command
evalCmd :: Cmd -> Env Val -> Env Val
evalCmd (Assign v e)          m = insert v (evalExpr e m) m
evalCmd (If x t f)   m = if unpackBool x m
                                  then evalCmd t m
                                  else evalCmd f m
evalCmd (While x w)  m = if unpackBool x m
                                  then evalCmd (While x w) (evalCmd w m)
                                  else m
evalCmd (Block cmds) m = evalCmds cmds m

-- Executes a list of commands
evalCmds :: [Cmd] -> Env Val -> Env Val
evalCmds []     m = m
evalCmds (x:xs) m = evalCmds xs (evalCmd x m)

-- Sets up an environment, initializes Ints to 0 and Bools to false.
-- Runs the program.
evalProg :: Prog -> Env Val
evalProg (P ds c) = evalCmd c m
    where
        m = fromList (map (\(x,t) -> (x, init t)) ds)
        init TInt   = Left 0
        init TBool  = Right False

-- Check for type errors and run the program
start :: Prog -> Maybe (Env Val)
start p = if typeProg p then Just (evalProg p)
                        else Nothing


--
-- *    String Operations
--

-- makes a string upper case
-- example of using upCase
-- lowCase "HELLO world"
-- bad example of using upCase
-- lowCase HELLO world
upCase :: [Char] -> [Char]
upCase = map toUpper

-- makes a string lower case
-- example of using lowCase
-- lowCase "HELLO world"
-- bad example of using lowCase
-- lowCase HELLO world
lowCase :: [Char] -> [Char]
lowCase = map toLower

-- | Concatenate two strings
-- example of using cat_string
-- cat_string "hello" "world"
-- bad example of using cat_string
-- cat_string hello world

cat_string :: String -> String -> String
cat_string a b = a ++ b


--
-- *    Tuple Operations
--
-- example of using tuple_gen:
-- tuple_gen (I 1) (B True)
-- bad example of using tuple_gen:
-- tuple_gen 1 2
tuple_gen :: Expr -> Expr -> (Expr, Expr)
tuple_gen (I x) (I y) = ((I x),(I y))
tuple_gen (B x) (I y) = ((B x),(I y))
tuple_gen (B x) (B y) = ((B x),(B y))
tuple_gen (I x) (B y) = ((I x),(B y))
tuple_gen (T x y) (T x1 y1) = ((T x y),(T x1 y1))
tuple_gen (T x y) (I s) = ((T x y),(I s))
tuple_gen (I s) (T x y)  = ((I s),(T x y))
tuple_gen (T x y) (B s) = ((T x y),(B s))
tuple_gen (B s) (T x y)  = ((B s),(T x y))

-- | Returns the first element in a tuple of two values
-- example of using tuple_first:
-- tuple_first (I 1, I 2)
-- bad example:
-- tuple_first (1, 2)
tuple_first :: (Expr, Expr) -> Expr
tuple_first (x, y) = x

-- | Returns the first element in a tuple of two values
-- example of using tuple_second:
-- tuple_second (B true, B false)
-- bad example:
-- tuple_second (true, false)
tuple_second :: (Expr, Expr) -> Expr
tuple_second (x, y) = y


--
-- *    List Operations
--

-- | Returns the length of a given list
-- example of using len:
-- len [I 1, I 2]
-- bad example:
-- len [1,2,3]
len :: [Expr]  -> Int
len [] = 0
len (x:xs)  = len(xs) +1

-- | Checks to see if a Expr datatype is in a list of Exprs
-- example of using contains_val
-- contains_val [I 1, I 2] (I 1)
-- bad example:
-- contains_val [1, 2, 3] 2

contains_val :: [Expr] -> Expr -> Bool
contains_val [] _ = False
contains_val (x:xs) (I s)
    | (I s) == x = True
    | otherwise = contains_val xs (I s)
contains_val (x:xs) (B s)
    | (B s) == x = True
    | otherwise = contains_val xs (B s)
contains_val (x:xs) (T x1 y)
    | (T x1 y) == x = True
    | otherwise = contains_val xs (T x1 y)

-- | Concatenate two lists of values
-- examples of using cat_vals
-- cat_vals [I 1, I 2] [I 3]
-- cat_vals [I 1, B True] [B False]
-- bad examples:
-- cat_vals [1, 2, 3] 4

cat_vals :: [Expr] -> [Expr] -> [Expr]
cat_vals list_a list_b = list_a ++ list_b

-- | Access a chosen index from a list
-- example of using index:
-- index [I 1, I 2, I 3] 3
-- bad examples:
-- index [1, 2, 3, 4] 3
-- 
index :: [Expr] -> Int -> Expr
index xs i = xs !! i

-- | Appends a value to a list
-- example of using append: 
-- append [1,2,3] [4]
-- examples of using append wrong:
-- append [1,2,3] 4
-- append [1,2,3] ["4"]
append :: [Expr] -> [Expr] -> [Expr]
append [] x = x
append (x:xs) y = x : append xs y