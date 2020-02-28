module DoesThisSoundBad where
import Prelude hiding (Integer)

-- | Abstract syntax of commands.
type Vals = [Value]


data Var = Integer Int
        | Boolean Bool
    deriving (Eq,Show)

data Value = I Int
        | B Bool
        | T Value Value
    deriving (Eq,Show)

data Cmd = Print Var
        | Reassign Var Value
    deriving (Eq,Show)

if' :: Bool -> a -> a -> a
if' True  x _ = x
if' False _ y = y

-- | Tuple generator for two values

tuple_gen :: Value -> Value -> (Value, Value)
tuple_gen (I x) (I y) = ((I x),(I y))
tuple_gen (B x) (I y) = ((B x),(I y))
tuple_gen (B x) (B y) = ((B x),(B y))
tuple_gen (I x) (B y) = ((I x),(B y))
tuple_gen (T x y) (T x1 y1) = ((T x y),(T x1 y1))
tuple_gen (T x y) (I s) = ((T x y),(I s))
tuple_gen (I s) (T x y)  = ((I s),(T x y))
tuple_gen (T x y) (B s) = ((T x y),(B s))
tuple_gen (B s) (T x y)  = ((B s),(T x y))

-- | Concatenate two strings

cat_string :: String -> String -> String
cat_string a b = a ++ b

-- | Concatenate two lists of values

cat_vals :: [Value] -> [Value] -> [Value]
cat_vals list_a list_b = list_a ++ list_b

-- | Checks to see if a Value datatype is in a list of Values

contains_val :: [Value] -> Value -> Bool
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

--| This function returns the length of a given list :)
len :: [Int]  -> Int
len [] = 0
len (x:xs)  = len(xs) +1