maximo :: (Ord a,Num a) => a -> a -> a
maximo x y      | x>y      = x
                |otherwise = y


maximoLista :: (Ord a,Num a) => [a] -> a
maximoLista [] = 0
maximoLista (x:xs) = maximo x (maximoLista xs)