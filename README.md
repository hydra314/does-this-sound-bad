# does-this-sound-bad
A simple language created in Haskell by Blake Le, Cameron Kocher, and Robert Yelas. 

Cameron Kocher - kochecam@oregonstate.edu
Robert Yelas - yelasr@oregonstate.edu
Blake Le - lebla@oregonstate.edu

Our language is called does this sound bad, also known as dtsb. It uses an imperative paradigm, and is intended to be simple and easy to use, with uncomplicated syntax. We built it using the language defined in imp.hs as a foundation, and added extra features including strings, tuples, lists, and functions. 

Compile using ghci: *ghci doesthissoundbad.hs*

functions and their example usages (correct expected output is shown, wrong syntax will throw errors)

upCase :: [Char] -> [Char]
    makes a string upper case
    example of using upCase
    upCase "HELLO world"
    output: "HELLO WORLD"

    bad example of using upCase
    upCase HELLO world

lowCase :: [Char] -> [Char]
    makes a string lower case
    example of using lowCase
    lowCase "HELLO world"
    output: "hello world"

    bad example of using lowCase
    lowCase HELLO world

cat_string :: String -> String -> String
    Concatenate two strings
    example of using cat_string
    cat_string "hello" "world"
    output: "helloworld"

    bad example of using cat_string
    cat_string hello world




Tuple Operations

tuple_gen :: Expr -> Expr -> (Expr, Expr)
    example of using tuple_gen:
    tuple_gen (I 1) (B True)
    output: (I 1, B True)

    bad example of using tuple_gen:
    tuple_gen 1 2

tuple_first :: (Expr, Expr) -> Expr
    Returns the first element in a tuple of two values
    example of using tuple_first:
    tuple_first (I 1, I 2)
    oytput: I 1

    bad example:
    tuple_first (1, 2)




tuple_second :: (Expr, Expr) -> Expr
    Returns the first element in a tuple of two values
    example of using tuple_second:
    tuple_second (B true, B false)
    output: B false

    bad example:
    tuple_second (true, false)




List Operations

len :: [Expr]  -> Int
    Returns the length of a given list
    example of using len:
    len [I 1, I 2]
    output: 2

    bad example:
    len [1,2,3]


contains_val :: [Expr] -> Expr -> Bool
    Checks to see if a Expr datatype is in a list of Exprs
    example of using contains_val
    contains_val [I 1, I 2] (I 1)
    output: True

    bad example:
    contains_val [1, 2, 3] 2


cat_vals :: [Expr] -> [Expr] -> [Expr]
    Concatenate two lists of values
    examples of using cat_vals
    cat_vals [I 1, I 2] [I 3]
    output: [I 1, I 2, I 3]
    cat_vals [I 1, B True] [B False]
    output: [I 1, B True, B False]

    bad examples:
    cat_vals [1, 2, 3] 4


index :: [Expr] -> Int -> Expr
    Access a chosen index from a list
    example of using index:
    index [I 1, I 2, I 3] 2
    output: I 3

    bad examples:
    index [1, 2, 3, 4] 3


append :: [Expr] -> [Expr] -> [Expr]
    Appends a value to a list
    example of using append: 
    append [1,2,3] [4]
    output: [1,2,3,4]

    examples of using append wrong:
    append [1,2,3] 4
    append [1,2,3] ["4"]
    


