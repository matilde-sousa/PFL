import Data.List (sort)
import Data.List (lookup)
import Data.List (nub)

data Set a = Empty
            | Node a (Set a) (Set a)
    deriving (Show)

empty :: Set a
empty = Empty

member :: Ord a => a -> Set a -> Bool
member _ Empty = False
member x (Node y left right)
    | x==y = True
    | x<y = member x left
    | otherwise = member x right

insert :: Ord a => a -> Set a -> Set a
insert x Empty = Node x Empty Empty
insert x (Node y left right)
    | x == y = Node y left right
    | x < y = Node y (insert x left) right
    | otherwise = Node y left (insert x right)

fromList :: Ord a => [a] -> Set a
fromList xs = build (sort xs)
    where
    build [] = Empty
    build ys = 
        let
            k = length ys `div` 2
            (left, rightWithRoot) = splitAt k ys
            (x:right) = rightWithRoot
            in
                Node x (build left) (build right)

-- 5.4 
-- a)
size :: Set a -> Int
size Empty = 0
size (Node _ left right) = 1 + size left + size right

-- b)
height :: Set a -> Int
height Empty = 0
-- height = 1 + height of Node val left right's tallest child
height (Node _ left right) = 1 + max (height left) (height right)

-- c)

-- Inserting a sorted list into a simple binary search tree (BST) 
-- creates a completely unbalanced, 
-- "degenerate" tree with O(N) height, 
-- which is as slow to search as a plain list.
set1 :: Set Integer
set1 = foldr insert empty [1..1000]

--Building a set from a list by balancing it 
-- (e.g., picking the median as the root) 
-- creates a short, bushy tree with O(logN) height,
-- which is extremely fast to search.
set2 :: Set Integer
set2 = fromList [1..1000]

-- TODO: 5.5

-- 5.6
type Name = Char -- ’x’, ’y’, ’z’, etc.
type Env = [(Name, Bool)]
data Prop = Const Bool
    | Var Name
    | Not Prop
    | And Prop Prop
    | Or Prop Prop
    | Imply Prop Prop
  deriving (Show)

eval :: Env -> Prop -> Bool
eval env (Const b) = b
eval env (Var x) = case lookup x env of
    Just b -> b
    Nothing -> error "undefined variable"
eval env (Not p) = not (eval env p)
eval env (And p q) = eval env p && eval env q
eval env (Or p q) = eval env p || eval env q
eval env (Imply p q) = not (eval env p) || eval env q

-- 5.7

vars :: Prop -> [Name]
vars (Const _) = []
vars (Var x) = [x]
vars (Not p) = vars p
vars (And p q) = vars p ++ vars q
vars (Or p q)    = vars p ++ vars q
vars (Imply p q) = vars p ++ vars q

-- 5.8

booleans :: Int -> [[Bool]]
booleans 0 = [[]]
booleans n = [b : rest | b <- [False, True], rest <- booleans (n-1)]


-- 5.9
environments :: [Name] -> [Env]
environments names = map (zip names) (booleans (length names))

-- 5.10

table :: Prop -> [(Env,Bool)]
-- map over the list allEnvs
-- for each env in the list, create a pair (env, ...)
-- where the second part is the result of evaluating the proposition with that env
table prop = map (\env -> (env, eval env prop)) allEnvs
    where
        allVars = vars prop
        uniqueVars = nub allVars
        allEnvs = environments uniqueVars

-- 5.11
satisfies :: Prop -> [Env]
satisfies prop = map fst (truths)
    where
        truths = filter snd (table prop)