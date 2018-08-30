ordena2::(Integer, Integer)->(Integer,Integer)
ordena2 (x,y) | x<y = (x,y)
              |otherwise = (y,x) 

ordena3::(Integer,Integer,Integer)->(Integer,Integer,Integer)
ordena3 (x,y,z) | x<aux1 = (x,aux1,aux2)
                | y<aux3 = (y,aux3,aux4)
                | z<aux5 = (z,aux5,aux6)
                where
                    (aux1,aux2)=ordena2(y,z)
                    (aux3,aux4)=ordena2(x,z)
                    (aux5,aux6)=ordena2(x,y)

maximo :: (Ord a,Num a) => a -> a -> a
maximo x y      | x>y      = x
                |otherwise = y

maximoLista :: (Ord a,Num a) => [a] -> a
maximoLista [] = 0
maximoLista (x:xs) = maximo x (maximoLista xs)


maximoYresto :: (Ord a,Num a) => [a] -> (a,[a])
maximoYresto x = (aux1, filter (/=aux1) x)
  where aux1=maximoLista x

