This file accompanies my talk about finger trees. It contains all code presented, as well as exercises to attempt. 
Most of the content of this talk comes from the paper "Finger trees: a simple general-purpose data structure" by Ralf Hinze and Ross Paterson. 

Finger trees have amortized constant time access to either end, and logarithmic concatenation. The Haskell package Data.Sequence is implemented using finger trees.

We start off recapping our definition of perfect trees:

> data PerfectTree a = Leaf a
>                    | Fork (PerfectTree (a,a))

If we wish to convert this into a tree which always had exactly 2 or 3 branches at each node, we can define:

> data TwoThreeTree a = Zero a
>               | Succ (TwoThreeTree (Node a))

> data Node a = Node2 a a
>             | Node3 a a a
>               deriving Show

Finger trees take this data structure and 'pull it up' from both ends. This is equivalent to creating a zipper at both ends.

***EXERCISE: (Hard) Differentiate the 23Tree data structure to reach the FingerTree definition below.

> data FingerTree a = Empty
>                   | Single a
>                   | Deep [a] (FingerTree (Node a)) [a]
>                     deriving Show

We now define the foldable instance of Finger Trees and Nodes. 
This contrasts with the paper which uses a 'Reducable' typeclass instead.
reducer is the same as foldr with the last two arguments swapped around. It makes a lot of the code look nicer, but hides some of the complexity and for our own familiarity, I have converted everything to use foldr instead.

> instance Foldable Node where
>   foldr f k (Node2 x y) = (f x . f y) k
>   foldr f k (Node3 x y z) = (f x . f y . f z) k

> instance Foldable FingerTree where
>   --foldr :: (a -> b -> b) -> b -> FingerTree a -> b
>   foldr f k Empty = k
>   foldr f k (Single x) = f x k
>   foldr f k (Deep ls ft rs) = foldr f (foldr g (foldr f k rs) ft) ls where
>     --g :: Node a -> b -> b
>     g n k = foldr f k n
>   -- It is easy to see how this definition in particular can be greatly simplified with reducer

Next we define the left append function for finger trees:

> (<|) :: a -> FingerTree a -> FingerTree a
> x <| Empty = Single x
> x <| Single y = Deep [x] Empty [y]
> x <| Deep [a,b,c,d] ft rs = Deep [x,a] ((Node3 b c d) <| ft) rs
> x <| Deep ls ft rs = Deep ([x] ++ ls) ft rs

***EXERCISE: Define |>

> (|>) :: FingerTree a -> a -> FingerTree a
> ft |> x = undefined

Now with folds and appends, converting two and from a list is trivial:

> toList :: FingerTree a -> [a]
> toList = foldr (:) []

> fromList :: [a] -> FingerTree a
> fromList = foldr (<|) Empty

We could define our own view datatype which acted lazily for just viewing the head or tail of the finger tree. You can find this in the paper.

***EXERCISE: Prove fromList.toList = id and toList.fromList = id

It's also nice to have a function which conveniently adds multiple elements to a tree:

> (<||) :: Foldable t => t a -> FingerTree a -> FingerTree a
> xs <|| ft = foldr (<|) ft xs

***EXERCISE: Define ||>

> (||>) :: Foldable t => FingerTree a -> t a -> FingerTree a
> ft ||> xs = undefined

***EXERCISE: Prove fromList.toList = id and toList.fromList = id

Lastly we look at concatenation:

> (|><|) :: FingerTree a -> FingerTree a -> FingerTree a
> xs |><| ys = app3 xs [] ys

We use the app3 function (don't know why it's called app3, but I couldn't think of a better name) as a way of pushing the right and left fingers of the two trees together.

> app3 :: FingerTree a -> [a] -> FingerTree a -> FingerTree a 
> app3 xs ts Empty = xs ||> ts
> app3 Empty ts ys = ts <|| ys
> app3 xs ts (Single y) = (xs ||> ts) |> y
> app3 (Single x) ts ys = x <| (ts <|| ys)
> app3 (Deep ls ft rs) ts (Deep ls' ft' rs') = Deep ls (app3 ft (nodes (rs ++ ts ++ ls')) ft') rs'

As the finger tree has parameter Node a, we need to construct the middle bits into new nodes:

> nodes :: [a] -> [Node a]
> nodes [a,b] = [Node2 a b]
> nodes [a,b,c] = [Node3 a b c]
> nodes (a:b:c:xs) = Node3 a b c : nodes xs

***EXERCISE: Using lists in the Deep constructor for the left and right fingers is inefficient.
Convert all the functions to use the more efficient Digit data type defined below:

> data Digit a = One a
>              | Two a a
>              | Three a a a 
>              | Four a a a a

Some of the definitions may get a bit complex, and you may find it easier to use template haskell to generate those definitions. 

This is everything that I covered in the talk, the paper proves some of the bounds and goes on to talk about measurements (adding values to nodes eg. for weighting/ordering them) and to show some typical applications of them.
