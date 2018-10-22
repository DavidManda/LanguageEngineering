> {-# LANGUAGE FlexibleInstances #-}


> import Data.List

> data Graph = Empty
>              | Vertex Int
>              | Overlay Graph Graph
>              | Connect Graph Graph
>               deriving Show

> vertices :: Graph -> [Int]
> vertices Empty = []
> vertices (Vertex x) = [x]
> vertices (Overlay g1 g2) = union (vertices g1) (vertices g2)

> roots :: Graph -> [Int]
> roots Empty = []
> roots (Vertex x) = [x]
> roots (Overlay g1 g2) = union (roots g1) (roots g2)
> roots (Connect g1 g2) = (roots g1) \\ (roots g2)

> class (Show g ) => Graphy g where
>   empty :: g
>   vertex :: Int -> g
>   overlay :: g -> g-> g
>   connect :: g -> g-> g

> instance Graphy Graph where
>   empty = Empty
>   vertex x = Vertex x
>   overlay x y = Overlay x y
>   connect x y = Connect x y
