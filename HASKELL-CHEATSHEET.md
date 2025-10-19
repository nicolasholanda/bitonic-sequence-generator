# Haskell Cheat Sheet

> Quick reference for common Haskell syntax and patterns used in this project.

## 📑 Table of Contents

- [Basic Syntax](#basic-syntax)
- [Types and Type Signatures](#types-and-type-signatures)
- [Functions](#functions)
- [Data Types](#data-types)
- [Pattern Matching](#pattern-matching)
- [Lists](#lists)
- [Operators](#operators)
- [Control Flow](#control-flow)
- [Monads and Do Notation](#monads-and-do-notation)
- [Common Type Classes](#common-type-classes)
- [Error Handling](#error-handling)
- [Project-Specific Patterns](#project-specific-patterns)

---

## 🔤 Basic Syntax

### Comments

```haskell
-- Single line comment

{- 
   Multi-line comment
   Spans multiple lines
-}

{-|
  Haddock documentation comment
  Used for generating API docs
-}
```

### Variables (Immutable)

```haskell
-- Variables are immutable (can't be changed)
x = 42
name = "Alice"
isValid = True

-- No 'let' or 'var' keyword needed at top level
```

### String Literals

```haskell
{-# LANGUAGE OverloadedStrings #-}

-- With OverloadedStrings, these all work:
text1 = "hello"           :: String
text2 = "hello"           :: Text
text3 = "hello"           :: ByteString
```

---

## 📐 Types and Type Signatures

### Basic Types

| Type | Description | Example |
|------|-------------|---------|
| `Int` | Fixed-precision integer | `42` |
| `Integer` | Arbitrary-precision integer | `999999999999999` |
| `Double` | Double-precision float | `3.14` |
| `Bool` | Boolean | `True`, `False` |
| `Char` | Single character | `'a'` |
| `String` | List of characters | `"hello"` (same as `[Char]`) |
| `()` | Unit (like void) | `()` |

### Type Signatures

```haskell
-- Function type signature
functionName :: Type1 -> Type2 -> ReturnType

-- Examples:
add :: Int -> Int -> Int
add x y = x + y

greet :: String -> String
greet name = "Hello, " ++ name

isEven :: Int -> Bool
isEven n = n `mod` 2 == 0
```

### Type Variables (Generics)

```haskell
-- 'a' is a type variable (like <T> in Java)
identity :: a -> a
identity x = x

-- Can use any lowercase letter
first :: a -> b -> a
first x y = x

-- Multiple type variables
pair :: a -> b -> (a, b)
pair x y = (x, y)
```

### Type Constraints

```haskell
-- 'a' must be a type that can be shown (has Show instance)
printIt :: Show a => a -> String
printIt x = "Value: " ++ show x

-- Multiple constraints
compare :: (Ord a, Show a) => a -> a -> String
compare x y = show x ++ " vs " ++ show y
```

---

## 🔧 Functions

### Function Definition

```haskell
-- Single parameter
square x = x * x

-- Multiple parameters
add x y = x + y

-- With type signature (recommended)
multiply :: Int -> Int -> Int
multiply x y = x * y
```

### Function Application

```haskell
-- No parentheses needed for function calls
result = square 5            -- 25
sum = add 3 4                -- 7

-- With parentheses for clarity
result = square (add 2 3)    -- 25
```

### Partial Application (Currying)

```haskell
add :: Int -> Int -> Int
add x y = x + y

-- Create a new function by partially applying
add5 = add 5                 -- add5 :: Int -> Int
result = add5 10             -- 15

-- Same as:
result = add 5 10            -- 15
```

### Anonymous Functions (Lambdas)

```haskell
-- Syntax: \param1 param2 -> body
square = \x -> x * x
add = \x y -> x + y

-- Used inline
map (\x -> x * 2) [1, 2, 3]  -- [2, 4, 6]

-- Multiple parameters
zipWith (\x y -> x + y) [1,2,3] [4,5,6]  -- [5,7,9]
```

### Higher-Order Functions

```haskell
-- Function that takes a function as parameter
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

-- Usage
applyTwice (*2) 3            -- 12  (3 * 2 * 2)
applyTwice (++[1]) [0]       -- [0,1,1]
```

---

## 📦 Data Types

### Simple Data Types (Enums)

```haskell
-- Like enum in Java/TypeScript
data Status = Active | Inactive | Pending

-- With deriving clauses
data Color = Red | Green | Blue
    deriving (Show, Eq)

-- Usage
myStatus = Active
showIt = show Red            -- "Red"
```

### Data Types with Fields (Records)

```haskell
-- Record syntax (like a struct or class)
data Person = Person
    { name :: String
    , age :: Int
    , email :: String
    } deriving (Show)

-- Creating values
alice = Person "Alice" 30 "alice@example.com"

-- Or with named fields
bob = Person { name = "Bob", age = 25, email = "bob@example.com" }

-- Accessing fields
bobsName = name bob          -- "Bob"
bobsAge = age bob            -- 25
```

### Sum Types (Discriminated Unions)

```haskell
-- Type with multiple constructors
data Shape 
    = Circle Double                    -- radius
    | Rectangle Double Double          -- width, height
    | Triangle Double Double Double    -- three sides
    deriving (Show)

-- Creating values
c = Circle 5.0
r = Rectangle 10.0 20.0

-- Pattern matching to handle
area :: Shape -> Double
area (Circle r) = pi * r * r
area (Rectangle w h) = w * h
area (Triangle a b c) = -- Heron's formula
    let s = (a + b + c) / 2
    in sqrt (s * (s-a) * (s-b) * (s-c))
```

### Type Aliases

```haskell
-- Create an alias for an existing type
type Name = String
type Age = Int
type Email = String

-- Usage
processUser :: Name -> Age -> Email -> String
processUser n a e = n ++ " is " ++ show a ++ " years old"
```

### Newtype (Wrapper)

```haskell
-- Like type alias but creates a distinct type
newtype UserId = UserId Int
    deriving (Show, Eq)

-- Must wrap/unwrap explicitly
userId = UserId 42

unwrap :: UserId -> Int
unwrap (UserId id) = id
```

---

## 🎯 Pattern Matching

### Basic Pattern Matching

```haskell
-- Function-level pattern matching
factorial :: Int -> Int
factorial 0 = 1                      -- Base case
factorial n = n * factorial (n - 1)  -- Recursive case

-- Boolean patterns
not :: Bool -> Bool
not True = False
not False = True
```

### Case Expressions

```haskell
-- Pattern match anywhere in code
describeNumber :: Int -> String
describeNumber n = case n of
    0 -> "zero"
    1 -> "one"
    2 -> "two"
    _ -> "many"  -- underscore matches anything
```

### Pattern Matching on Data Types

```haskell
data Maybe a = Nothing | Just a

-- Extract value from Maybe
fromMaybe :: a -> Maybe a -> a
fromMaybe default Nothing = default
fromMaybe _ (Just x) = x

-- Either type
data Either a b = Left a | Right b

-- Handle both cases
handleResult :: Either String Int -> String
handleResult (Left err) = "Error: " ++ err
handleResult (Right val) = "Success: " ++ show val
```

### List Patterns

```haskell
-- Empty list
isEmpty :: [a] -> Bool
isEmpty [] = True
isEmpty _ = False

-- Head and tail
head :: [a] -> a
head (x:xs) = x

-- Multiple elements
first :: [a] -> Maybe a
first [] = Nothing
first (x:_) = Just x

-- Specific patterns
describe :: [Int] -> String
describe [] = "empty"
describe [x] = "singleton"
describe [x,y] = "pair"
describe (x:y:xs) = "list of " ++ show (2 + length xs)
```

### Guards (Conditional Patterns)

```haskell
-- Guards with '|' (like if-else if-else)
grade :: Int -> String
grade score
    | score >= 90 = "A"
    | score >= 80 = "B"
    | score >= 70 = "C"
    | score >= 60 = "D"
    | otherwise = "F"

-- With pattern matching
analyze :: Int -> String
analyze n
    | n < 0 = "negative"
    | n == 0 = "zero"
    | n > 0 = "positive"
```

---

## 📋 Lists

### List Syntax

```haskell
-- List literal
numbers = [1, 2, 3, 4, 5]
letters = ['a', 'b', 'c']
strings = ["hello", "world"]

-- Empty list
empty = []

-- Cons operator (:)
list = 1 : 2 : 3 : []        -- [1,2,3]
list = 1 : [2, 3]            -- [1,2,3]

-- Ranges
range1 = [1..10]             -- [1,2,3,4,5,6,7,8,9,10]
range2 = [1,3..10]           -- [1,3,5,7,9]
range3 = [10,9..1]           -- [10,9,8,7,6,5,4,3,2,1]

-- Infinite lists
naturals = [1..]             -- [1,2,3,4,5,...]
evens = [2,4..]              -- [2,4,6,8,...]
```

### Common List Functions

```haskell
-- Length
length [1,2,3]               -- 3

-- Head and tail
head [1,2,3]                 -- 1
tail [1,2,3]                 -- [2,3]

-- Init and last
init [1,2,3]                 -- [1,2]
last [1,2,3]                 -- 3

-- Take and drop
take 3 [1..10]               -- [1,2,3]
drop 3 [1..10]               -- [4,5,6,7,8,9,10]

-- Concatenation
[1,2] ++ [3,4]               -- [1,2,3,4]
concat [[1,2], [3,4]]        -- [1,2,3,4]

-- Reverse
reverse [1,2,3]              -- [3,2,1]

-- Map (transform each element)
map (*2) [1,2,3]             -- [2,4,6]

-- Filter (keep elements that match)
filter even [1..10]          -- [2,4,6,8,10]

-- Fold (reduce)
foldl (+) 0 [1,2,3]          -- 6
foldr (:) [] [1,2,3]         -- [1,2,3]
```

---

## 🔢 Operators

### Arithmetic Operators

```haskell
5 + 3     -- 8   (addition)
5 - 3     -- 2   (subtraction)
5 * 3     -- 15  (multiplication)
5 / 3     -- 1.666... (division, returns Double)
5 `div` 3 -- 1   (integer division)
5 `mod` 3 -- 2   (modulo)
5 ^ 3     -- 125 (exponentiation)
```

### Comparison Operators

```haskell
5 == 5    -- True  (equality)
5 /= 3    -- True  (not equal)
5 > 3     -- True  (greater than)
5 < 3     -- False (less than)
5 >= 5    -- True  (greater or equal)
5 <= 3    -- False (less or equal)
```

### Logical Operators

```haskell
True && False   -- False (and)
True || False   -- True  (or)
not True        -- False (negation)
```

### Function Application Operators

```haskell
-- $ (apply function, low precedence)
f $ g $ h x     -- Same as: f (g (h x))
sqrt $ 3 + 1    -- Same as: sqrt (3 + 1)

-- . (function composition)
f . g           -- Same as: \x -> f (g x)
(+1) . (*2)     -- Same as: \x -> (x * 2) + 1

-- Example:
map ((*2) . (+1)) [1,2,3]  -- [4,6,8]
```

### List Operators

```haskell
1 : [2,3]       -- [1,2,3]  (cons)
[1,2] ++ [3,4]  -- [1,2,3,4] (concatenation)
[1,2,3] !! 1    -- 2 (index, 0-based)
```

### Custom Infix Operators

```haskell
-- Make function infix with backticks
10 `div` 3      -- Same as: div 10 3
"hello" `elem` ["hi", "hello"]  -- True

-- Define custom operators
(+++) :: [a] -> [a] -> [a]
xs +++ ys = xs ++ ys ++ xs

[1,2] +++ [3,4]  -- [1,2,3,4,1,2]
```

---

## 🔀 Control Flow

### If-Then-Else

```haskell
-- If expression (returns a value)
abs :: Int -> Int
abs n = if n < 0 then -n else n

-- Multi-line
checkAge :: Int -> String
checkAge age = 
    if age < 18 
    then "Minor" 
    else "Adult"
```

### Guards (Recommended over if-else)

```haskell
-- Guards are cleaner for multiple conditions
classify :: Int -> String
classify n
    | n < 0 = "negative"
    | n == 0 = "zero"
    | otherwise = "positive"
```

### Case Expressions

```haskell
-- Pattern matching in expressions
describeMaybe :: Maybe Int -> String
describeMaybe m = case m of
    Nothing -> "no value"
    Just x -> "value: " ++ show x

-- Can use guards in case
describeList :: [a] -> String
describeList xs = case xs of
    [] -> "empty"
    [x] -> "singleton"
    _ | length xs > 10 -> "long list"
      | otherwise -> "short list"
```

### Let and Where

```haskell
-- 'let' bindings (before usage)
circleArea :: Double -> Double
circleArea r = 
    let pi = 3.14159
        square x = x * x
    in pi * square r

-- 'where' bindings (after usage)
cylinderVolume :: Double -> Double -> Double
cylinderVolume r h = baseArea * h
    where baseArea = pi * r * r
          pi = 3.14159
```

---

## 🎭 Monads and Do Notation

### What is a Monad?

```haskell
-- Monad is a type class for types that support:
-- 1. return (wrap a value)
-- 2. >>= (bind, chain operations)

-- Common monads:
-- Maybe   - computations that might fail
-- Either  - computations with errors
-- IO      - input/output operations
-- []      - non-deterministic computations
```

### Maybe Monad

```haskell
-- Without do notation
safeDivide :: Double -> Double -> Maybe Double
safeDivide _ 0 = Nothing
safeDivide x y = Just (x / y)

chain :: Maybe Double
chain = safeDivide 10 2 >>= \x ->
        safeDivide x 2  >>= \y ->
        Just (y + 1)

-- With do notation (same thing, cleaner)
chain :: Maybe Double
chain = do
    x <- safeDivide 10 2
    y <- safeDivide x 2
    return (y + 1)
```

### IO Monad

```haskell
-- IO monad for side effects
main :: IO ()
main = do
    putStrLn "What's your name?"
    name <- getLine
    putStrLn ("Hello, " ++ name)
    return ()
```

### Either Monad

```haskell
-- Either for error handling
divide :: Double -> Double -> Either String Double
divide _ 0 = Left "Division by zero"
divide x y = Right (x / y)

-- Chain with do notation
calculate :: Either String Double
calculate = do
    x <- divide 10 2        -- x = 5
    y <- divide x 2          -- y = 2.5
    z <- divide y 0          -- Error! Returns Left "Division by zero"
    return z
```

### Common Monad Functions

```haskell
-- return (wrap a value in monad)
return 42 :: Maybe Int           -- Just 42
return 42 :: Either String Int   -- Right 42
return 42 :: IO Int              -- IO action returning 42

-- >>= (bind, chain operations)
Just 5 >>= \x -> Just (x * 2)    -- Just 10
Nothing >>= \x -> Just (x * 2)   -- Nothing

-- >> (sequence, ignore result)
putStrLn "Hello" >> putStrLn "World"

-- mapM (map with monadic function)
mapM print [1,2,3]               -- Prints each number

-- sequence (turn list of monads into monad of list)
sequence [Just 1, Just 2, Just 3]  -- Just [1,2,3]
sequence [Just 1, Nothing, Just 3] -- Nothing
```

---

## 🏷️ Common Type Classes

### Eq (Equality)

```haskell
-- Types that can be compared for equality
class Eq a where
    (==) :: a -> a -> Bool
    (/=) :: a -> a -> Bool

-- Usage
5 == 5          -- True
"hello" /= "hi" -- True

-- Deriving
data Color = Red | Green | Blue
    deriving (Eq)

Red == Red      -- True
Red == Blue     -- False
```

### Ord (Ordering)

```haskell
-- Types that can be ordered
class Eq a => Ord a where
    compare :: a -> a -> Ordering
    (<) :: a -> a -> Bool
    (>) :: a -> a -> Bool
    -- ... and more

-- Usage
5 > 3           -- True
compare 5 3     -- GT (Greater Than)
max 5 3         -- 5
min 5 3         -- 3

-- Deriving
data Priority = Low | Medium | High
    deriving (Eq, Ord)  -- Low < Medium < High
```

### Show (String Representation)

```haskell
-- Types that can be converted to String
class Show a where
    show :: a -> String

-- Usage
show 42         -- "42"
show True       -- "True"
show [1,2,3]    -- "[1,2,3]"

-- Deriving
data Person = Person String Int
    deriving (Show)

show (Person "Alice" 30)  -- "Person \"Alice\" 30"
```

### Read (Parse from String)

```haskell
-- Types that can be parsed from String
class Read a where
    read :: String -> a

-- Usage
read "42" :: Int          -- 42
read "True" :: Bool       -- True
read "[1,2,3]" :: [Int]   -- [1,2,3]

-- Deriving
data Color = Red | Green | Blue
    deriving (Read, Show)

read "Red" :: Color       -- Red
```

### Functor (Mappable)

```haskell
-- Types that can be mapped over
class Functor f where
    fmap :: (a -> b) -> f a -> f b

-- Usage
fmap (*2) (Just 5)        -- Just 10
fmap (*2) Nothing         -- Nothing
fmap (*2) [1,2,3]         -- [2,4,6]
fmap (*2) (Right 5)       -- Right 10

-- <$> is infix fmap
(*2) <$> Just 5           -- Just 10
```

---

## ⚠️ Error Handling

### Maybe (Null Safety)

```haskell
-- Maybe represents optional values
data Maybe a = Nothing | Just a

-- Safe head
safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

-- Usage
safeHead [1,2,3]     -- Just 1
safeHead []          -- Nothing

-- Chain with do notation
result :: Maybe Int
result = do
    x <- safeHead [1,2,3]
    y <- safeHead [4,5,6]
    return (x + y)
```

### Either (Error with Details)

```haskell
-- Either represents success or failure with details
data Either a b = Left a | Right b

-- Division with error message
safeDivide :: Int -> Int -> Either String Int
safeDivide _ 0 = Left "Cannot divide by zero"
safeDivide x y = Right (x `div` y)

-- Usage
safeDivide 10 2      -- Right 5
safeDivide 10 0      -- Left "Cannot divide by zero"

-- Pattern matching
case safeDivide 10 2 of
    Right result -> print result
    Left err -> putStrLn $ "Error: " ++ err
```

### Throwing and Catching Exceptions (IO)

```haskell
import Control.Exception

-- Throw an exception
throwError :: IO ()
throwError = throwIO $ userError "Something went wrong"

-- Catch an exception
safeDivideIO :: Int -> Int -> IO (Either String Int)
safeDivideIO x y = 
    (Right <$> evaluate (x `div` y)) 
    `catch` (\(e :: SomeException) -> return $ Left (show e))

-- Usage
main = do
    result <- safeDivideIO 10 0
    case result of
        Right val -> print val
        Left err -> putStrLn $ "Error: " ++ err
```

---

## 🎪 Project-Specific Patterns

### AppM Monad (ReaderT Pattern)

```haskell
-- Our custom monad for dependency injection
type AppM = ReaderT AppEnv IO

-- Get environment
getRedis :: AppM Redis.Connection
getRedis = asks envRedisConn

-- Run IO in AppM
doIO :: IO a -> AppM a
doIO = liftIO

-- Execute AppM action
runApp :: AppEnv -> AppM a -> IO a
runApp env action = runReaderT action env
```

### ActionM (Scotty Web Framework)

```haskell
-- Define route
post "/endpoint" $ do
    -- Parse JSON
    req <- jsonData :: ActionM MyRequest
    
    -- Do work
    result <- liftIO $ doSomething req
    
    -- Return JSON
    json result

-- Error handling
handleError :: ActionM ()
handleError = do
    status status400
    json $ ErrorResponse "Bad request" "INVALID"
```

### Hedis (Redis Operations)

```haskell
-- Get from Redis
cached <- liftIO $ Redis.runRedis conn $ do
    Redis.get (toBS key)

-- Set in Redis
liftIO $ Redis.runRedis conn $ do
    Redis.setex (toBS key) 3600 (toBS value)

-- Pattern matching on result
case cached of
    Right (Just bytes) -> -- found
    Right Nothing -> -- not found
    Left err -> -- error
```

### Aeson (JSON)

```haskell
-- Automatic deriving
data MyData = MyData
    { field1 :: String
    , field2 :: Int
    } deriving (Generic, Show)

instance ToJSON MyData
instance FromJSON MyData

-- Encoding/Decoding
encode myData             -- ByteString
decode bytes :: Maybe MyData
```

---

## 🚀 Quick Reference

### Common Patterns in This Project

| Pattern | Code | Meaning |
|---------|------|---------|
| Get environment | `conn <- asks envRedisConn` | Access dependency |
| Lift IO | `liftIO someIOAction` | Run IO in AppM/ActionM |
| Parse JSON | `req <- jsonData` | Parse request body |
| Return JSON | `json response` | Send JSON response |
| Error case | `Left (ServiceError "msg")` | Return error |
| Success case | `Right value` | Return success |
| Pattern match | `case x of ...` | Handle different cases |
| Guard | `\| condition = result` | Conditional return |
| Do notation | `do { x <- action; return x }` | Sequential operations |

---

## 📝 Remember

1. **Functions are pure** - Same input always gives same output (unless in IO)
2. **Immutability** - Can't change variables, create new values instead
3. **Lazy evaluation** - Values computed only when needed
4. **Type safety** - Compiler catches most bugs before runtime
5. **Pattern matching** - Exhaustive checking ensures all cases handled

---

**Pro Tip**: When stuck, ask the compiler! Type `:type expression` in GHCi to see the type of any expression.

