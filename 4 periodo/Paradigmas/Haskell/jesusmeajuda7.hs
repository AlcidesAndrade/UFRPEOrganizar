{-data Shape = Circle Float
	| Rectangle Float Float

area :: Shape -> Int
area Circle x = (pi * x^2)
area Rectangle b h = b*h-}

data Expr = Lit Int | Add Expr Expr| Sub Expr Expr

data List t = Nil | Const t (List t) deriving (Show)

data Tree t = NilT
 | Node t (Tree t) (Tree t)

eval :: Expr -> Int
eval (Lit n) = n
eval (Add e1 e2) = (eval e1) + (eval e2)
eval (Sub e1 e2) = (eval e1) - (eval e2)

showExpr :: Expr -> String
showExpr (Lit n) = show n
showExpr (Add e1 e2) = showExpr(e1) ++ " + " ++ showExpr(e2)
showExpr (Sub e1 e2) = showExpr(e1) ++ " - " ++ showExpr(e2)

toList :: List t -> [t]
toList Nil = []
toList (Const l z) = l: toList z

fromList :: [t] -> List t
fromList [] = Nil
fromList (x:xs) = (Const x (fromList xs))

depth :: Tree t -> Int
depth NilT = -1
depth (Node n esq dir) = 1 + max (depth esq) (depth dir)