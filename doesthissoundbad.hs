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
type Func = String

-- |    Expressions include literals, arithmetic expressions, comparisons, and variable references
--     
--      Abstract syntax:
--
--      expr ::= int
--             | bool
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
data Expr = I Int
          | B Bool
          | T Expr Expr
          | V Var
          | ADD Expr Expr
          | SUB Expr Expr
          | MUL Expr Expr
          | DIV Expr Expr
          | EQU Expr Expr
          | NEQ Expr Expr
          | GRT Expr Expr
          | GRE Expr Expr
          | LST Expr Expr
          | LTE Expr Expr
          | NOT Expr
    deriving(Eq,Show)

-- |    Commands include if/else conditionals, loops, function definitions and calls, variable assignments, and command blocks
--
--      Abstract syntax:
--      
--      cmd ::= `if` expr cmd `else` cmd
--            | `while` expr cmd
--            | `define` func (var *) { cmd* }
--            | `call` func (expr *)
--            | `assign` var expr
--            | { cmd * }
data Cmd = If Expr Cmd Cmd
         | While Expr Cmd
         | Define Func [Var] [Cmd]
         | Call Func [Expr]
         | Assign Var Expr
         | Block [Cmd]
    deriving(Eq,Show)

-- |    Abstract syntax of types.
--     
--      type  ::=  `int`  |  `bool`
--
data Type = TInt | TBool
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

-- | Typing relation for expressions. We need an environment to lookup
--   the names of variable references (last case). We use a Maybe to
--   represent the fact that typing might fail, for example, if we get
--   a type error or if a variable is not in the environment.
typeExpr :: Expr -> Env Type -> Maybe Type
typeExpr (I _)   _ = Just TInt
typeExpr (ADD l r) m = case (typeExpr l m, typeExpr r m) of
                         (Just TInt, Just TInt) -> Just TInt
                         _                      -> Nothing
typeExpr (LTE l r) m = case (typeExpr l m, typeExpr r m) of
                         (Just TInt, Just TInt) -> Just TBool
                         _                      -> Nothing
typeExpr (NOT e)   m = case typeExpr e m of
                         Just TBool -> Just TBool
                         _          -> Nothing
typeExpr (V v)   m = lookup v m


-- | Type checking statements. NOTe that the return type here is just a
--   Boolean value since a statement doesn't have a type. The Boolean
--   value indicates whether or not the statement is type correct (i.e.
--   this function implements type checking of statements).
-- 
--   Also note that when we type check a while loop, we do not execute
--   the loop several times; we just check that the type of the condition
--   is a Bool, and check that the body type checks. If both of those are
--   true, then we know that the while loop cannot produce a type error
--   without having to consider each iteration. This is where the benefits
--   of static type checking start to become more clear. With dynamic
--   typing we would have to check the types in the loop body on each
--   iteration, whereas with static typing we just check the loop body
--   once.
typeCmd :: Cmd -> Env Type -> Bool
typeCmd (Assign v e)   m = case (lookup v m, typeExpr e m) of
                            (Just tv, Just te) -> tv == te
                            _ -> False
typeCmd (If c st se) m = case typeExpr c m of
                            Just TBool -> typeCmd st m && typeCmd se m
                            _ -> False
typeCmd (While c sb) m = case typeExpr c m of
                            Just TBool -> typeCmd sb m
                            _ -> False
typeCmd (Block ss)   m = all (\s -> typeCmd s m) ss


-- | Type checking programs. The 'fromList' function is from the
--   Data.Map module. It builds a map from a list of pairs, thus
--   initializing our typing environment.
typeProg :: Prog -> Bool
typeProg (P ds s) = typeCmd s (fromList ds)




-- if' :: Bool -> a -> a -> a
-- if' True  x _ = x
-- if' False _ y = y


--
-- *    String Operations
--

-- makes a string upper case
upCase :: [Char] -> [Char]
upCase = map toUpper

-- makes a string lower case
lowCase :: [Char] -> [Char]
lowCase = map toLower

-- | Concatenate two strings

cat_string :: String -> String -> String
cat_string a b = a ++ b

--
-- *    Tuple Operations
--

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
tuple_first :: (Expr, Expr) -> Expr
tuple_first (x, y) = x

-- | Returns the first element in a tuple of two values
tuple_second :: (Expr, Expr) -> Expr
tuple_second (x, y) = y

--
-- *    List Operations
--

-- | Returns the length of a given list
len :: [Expr]  -> Int
len [] = 0
len (x:xs)  = len(xs) +1

-- | Checks to see if a Expr datatype is in a list of Exprs

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

cat_vals :: [Expr] -> [Expr] -> [Expr]
cat_vals list_a list_b = list_a ++ list_b

-- | Access a chosen index from a list
index :: [Expr] -> Int -> Expr
index xs i = xs !! i

-- | Appends a value to a list
append :: [Expr] -> [Expr] -> [Expr]
append [] x = x
append (x:xs) y = x : append xs y