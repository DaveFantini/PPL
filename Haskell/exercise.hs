-- define a data structure to define structures working both as lists and as binary trees

data Listree a = Nil | Cons a (Listree a) | Branch (Listree a) (Listree a) deriving (Eq, Show)

-- make Listree an instance of Functor
-- fmap :: Functor f => (a -> b) -> f a -> f b
instance Functor Listree where
    fmap f Nil = Nil
    fmap f (Cons n next) = Cons (f n) (fmap f next)
    fmap f (Branch left right) = Branch (fmap f left) (fmap f right)

-- make Listree an instance of Foldable
-- foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b
instance Foldable Listree where
    foldr f b Nil = b
    foldr f b (Cons n next) = f n (foldr f b next)
    foldr f b (Branch left right) = foldr f (foldr f b left) right
    
-- make Listree instance of Applicative
-- (<*>) :: Applicative f => f (a -> b) -> f a -> f b:

(<++>) :: Listree a -> Listree a -> Listree a
x <++> Nil = x
Nil <++> x = x
(Cons n next) <++> z = (Cons n (next <++> z))
(Branch left right) <++> z = (Branch left (right <++> z))

myconcat :: Listree (Listree a) -> Listree a
myconcat listree = foldr (<++>) Nil listree

myconcatMap :: (a -> (Listree b)) -> Listree a -> Listree b
myconcatMap f listree = myconcat $ fmap f listree

instance Applicative Listree where
    pure n = Cons n Nil
    list1 <*> list2 = myconcatMap (\f -> fmap f list2) list1 



-- Define a ternary tree data structure, called Ttree, in which every node contain both a value and a color, which can be either
--  yellow or blue. You can assume that a tree cannot be empty.
--  Node 1 B (Node 2 Y (Leaf 3 Y)(Leaf 4 Y)(Leaf 5 Y)) (Leaf 6 B) (Node 7 Y (Leaf 8 Y)(Leaf 9 B)(Leaf 10 Y))
data Color = Y | B deriving (Show, Eq)

data Ttree a = Leaf a Color | Node a Color (Ttree a) (Ttree a) (Ttree a) deriving (Show, Eq)

-- make Tree an instance of Functor
instance Functor Ttree where
    fmap f (Leaf val col) = (Leaf (f val) col)
    fmap f (Node val col left mid right) = Node (f val) col (fmap f left) (fmap f mid) (fmap f right)

instance Foldable Ttree where
    foldr f b (Leaf val col) = f val b 
    foldr f b (Node val col left mid right) = f val (foldr f (foldr f (foldr f b right) mid) left)
{-
    foldr f b (Node val col left mid right) =
        let v1 = foldr f b right
            v2 = foldr f v1 mid
            v3 = foldr f v2 left
        in f val v3
-}

-- define a function yellowSubTrees, which returns a list containing all the maximal subtrees
-- of a given Ttree that are all made of yellow nodes
yellowSubTree :: Ttree a -> [(Ttree a)]
yellowSubTree (Leaf val B) = []
yellowSubTree (Leaf val Y) = [(Leaf val Y)]
yellowSubTree (Node val Y left mid right) =
    let l = yellowSubTree left 
        m = yellowSubTree mid 
        r = yellowSubTree right
    in if and [(not $ null l), (not $ null m), (not $ null r)]
        then [(Node val Y left mid right)]
        else (l ++ m ++ r)
        
yellowSubTree (Node val B left mid right) =
        let l = yellowSubTree left 
            m = yellowSubTree mid 
            r = yellowSubTree right
        in l ++ m ++ r




-- A “dual list”, or Dupl, is a pair of independent lists. 
-- 1) Define a datatype for Dupl. Can it derive Show and/or Eq? If not, make Dupl an instance of both of them.
data Dupl a = Dupl [a] [a] deriving (Show, Eq)

-- make Dupl an instance of Functor
instance Functor Dupl where
    fmap f (Dupl l1 l2) = Dupl (fmap f l1) (fmap f l2)

tfoldr :: (a -> b -> b) -> b -> (Dupl a) -> b
tfoldr f b (Dupl l1 l2) = foldr f b (l1 ++ l2)

instance Foldable Dupl where
    foldr = tfoldr

instance Applicative Dupl where
    pure x = Dupl [x] []
    (Dupl l1 l2) <*> (Dupl l3 l4) = Dupl (l1 <*> l3) (l2 <*> l4)
    -- (Dupl l1 l2) <*> (Dupl l3 l4) = Dupl (concatMap (\f -> map f l3) l1) (concatMap (\f1 -> map f1 l4) l2)







data Blob a = Blob a (a -> a)

instance Show a => Show (Blob a) where
    show (Blob x f) = "Blob " ++ (show (f x))


-- Can Blob automatically derive Eq? Explain how, why, and, if the answer is negative, make it an instance of Eq.
-- Blob cannot derive Eq since contains functions (a->a) and equivalence between functions is not defined

instance Eq a => Eq (Blob a) where
    (Blob n1 f1) == (Blob n2 f2) = (n1 == n2) && ((f1 n1) == (f2 n2))

-- make Blob ans instance of Functor
instance Functor Blob where
    fmap f (Blob n fb) = Blob (f (fb n)) id

instance Foldable Blob where
    foldr f b (Blob n fb) = f (fb n) b

-- the first Blob of the operator should be ----> Blob (a -> b) ((a -> b) -> (a -> b))
-- Blob a (a -> a)
instance Applicative Blob where
    pure x = Blob x id
    (Blob n1 f1) <*> (Blob n2 f2) = Blob (((f1 n1) . f2) n2) id



-- when positive represents the fact that the stream is periodic, while it is not periodic if
-- negative (0 is left unspecified
data LolStream x = LolStream Int [x]

-- Define a function lol2lolstream which takes a finite list of finite lists [h1, h2, … hn], and returns
-- LolStream (|h1| + |h2| + … + |hn|) (h1 ++ h2 ++ … ++ hn ++ h1 ++ h2 ++ …)
generateinfList :: [[a]] -> [a]
generateinfList list = gen list list where
    gen (x:xs) l2 = if null xs
        then x ++ gen l2 l2
        else x ++ gen xs l2   

lol2lolstream :: [[a]] -> LolStream a
lol2lolstream ll = LolStream (foldr (+) 0 (map (\h -> (length h)) ll)) (generateinfList ll)

instance Show a => Show (LolStream a) where
    show (LolStream n lis) = "LolStream " ++ show n ++ " " ++ (show (take 20 lis)) ++ "..."

instance Eq a => Eq (LolStream a) where
    (LolStream n1 l1) == (LolStream n2 l2) = if (n1 /= n2)
        then False
        else if n1 > 0 
            then (take n1 l1) == (take n2 l2) 
            else (take 100 l1) == (take 100 l2)


instance Functor LolStream where
    fmap f (LolStream n lis) = LolStream n (map f lis)
    
lolfoldr :: (a -> b -> b) -> b -> (LolStream a) -> b
lolfoldr f b (LolStream n lis) = foldr f b (take n lis)

instance Foldable LolStream where
    foldr = lolfoldr

mypure :: Enum a => a -> LolStream a
mypure x = LolStream 1 [x,x..]

instance Applicative LolStream where
    pure x = lol2lolstream [[x]] 
    (LolStream n1 lis1) <*> (LolStream n2 lis2) = (LolStream (n1*n2) (concatMap (\h -> map h lis2) lis1))

-- instance Monad LolStream where
--    (LolStream n lis) >>= f = lol2lolstream [(take n lis) >>= \x -> (take n (f x))]



-- Define a data structure, called Lt, for generic list of lists, where each list has a fixed length and such
-- number is stored in the data structure.
data Lt a = Lt Int [[a]]


-- Define a function, called checkLt, that takes an Lt and returns true if it is valid (i.e. every list in it have
-- the correct size), false otherwise.
checklt :: Lt a -> Bool
checklt (Lt n ls) = foldr (&&) True (map (\x -> (length x) == n) ls)


-- Define a function, called checklist, that takes a list t and an Lt, checks if all the sublists of t are in the
-- given Lt, and uses Maybe to return the list of sublists of t that are not present in Lt.
--  sublists must be contiguous, e.g. the sublists of size 2 of [1,2,3] are [1,2], [2,3]
sublistcont :: [a] -> Int -> [[a]]
sublistcont lis n = if (length lis) < n 
    then []
    else [take n lis] ++ sublistcont (tail lis) n

liscontain :: Eq a => a -> [a] -> Bool
liscontain n lis = foldr (||) False (map (\el -> el == n) lis) 

checklist ::Eq a => [a] -> Lt a -> Maybe [[a]]
checklist lis (Lt n ltlis) = if (length lis) < n 
    then let res = filter (\x -> x /= lis) ltlis
    in
        if null res 
            then Nothing
            else Just res

    else let sub = sublistcont lis n
             res2 = filter (\x -> not (liscontain x ltlis)) sub
             in 
                if null res2
                    then Nothing 
                    else Just res2

-- Make Lt an instance of Functor
instance Functor Lt where
    fmap f (Lt n lis) = (Lt n (map (\x -> map f x) lis))




-- Define a Graph data-type, for directed graphs. Nodes hold some generic data, while edges have no data associated
data Gnode a b = Gnode {
    identifier :: a,
    datum :: b,
    adjacent :: [a]
} deriving (Show, Eq)

data Graph a b = Graph [Gnode a b] deriving (Show)

-- Define a graph_lookup function, to get the data associated with a node in the graph (or nothing if the node is not present).
graph_lookup :: Eq a => a -> Graph a b -> Maybe b
graph_lookup i (Graph g) = if null node 
    then Nothing
    else Just (datum $ head node)
    where node = filter (\x -> (identifier x) == i) g
    
-- Define an adjacents function, to check if two nodes are adjacent or not
adjacentcheck :: Eq a => Gnode a b -> Gnode a b -> Bool
adjacentcheck (Gnode i d al) (Gnode i2 d2 al2) = (elem i al2) || (elem i2 al)

-- Make graph an instance of Functor
instance Functor (Gnode a) where
    fmap f (Gnode i d a) = Gnode i (f d) a

instance Functor (Graph a) where
    fmap f (Graph nodes) = (Graph (fmap (\x -> fmap f x) nodes)) 