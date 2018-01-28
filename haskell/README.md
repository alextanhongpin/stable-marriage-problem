# Stable Marriage Problem in Haskell


## Usage

```bash
$ runhaskell Main.hs
```

## Comments

Here are some thoughts while writing the solution:

## Persisting State between recursions

It is a little hard to maintain the state of the application while recursing - using list doesn't solve the problem, because occassionally you need to reset the state of the previous data (when matchmaking a Male to Female, the Female has to first break up with the previous male).

Hence, we use `Data.Map` to persist the data, since you can use it to `lookup` the data from the previous iteration.

## Types Overuse

To make it more readable, we tend to create custom type such as:

```
type Male = String
type Female = String
```

The problem lies when creating the plural form:

```haskell
-- Expected
type Males = [Male]

-- Got
type Males = Map.Map MaleName Male
```

Those are not related at all, so the question is, how do we approach this naming problem? We will come back to this once we are more experienced :).

## Polymorphism

If we have a mixture of `Male` and `Female` type in a list, how do we name it? Do we come up with a generic type such as `Gender`?

Unfortunately, we cannot answer this for now due to lack of knowledge. This could just be a matter of decision.

## Returning order

```haskell
testOrder :: Int -> Int -> Ordering
testOrder a b 
    | a > b = GT
    | a < b = LT
    | a == b = EQ

testOrder 1 2 -- Returns LT
testOrder 2 1 -- Returns GT
testOrder 1 1 -- Returns EQ
```


## Updating Record

```haskell
data Male = Male { choices :: [FemaleName]
                 , status :: Status } deriving (Show)


let m = Male { choices = ["1", "2"], status = Single }
let makeEngage x y = x { status = Engaged y } 
let makeSingle x = x { status = Single }
let reduceChoice x = x { choices = init $ choices x }
print m

let m2 = makeEngage m "2"
print m2

let m3 = makeSingle m2
print m3

let m4 = reduceChoice m3
print m4
```