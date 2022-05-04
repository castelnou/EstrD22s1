module Set1 (
	Set,
	emptyS,
	addS,
	belongs,
	sizeS,
	removeS,
	unionS,
	setToList
 ) where
 
data Set a = ConsS [a] deriving Show

--Crea un conjunto vacío.
emptyS :: Set a
emptyS = ConsS []

--Dados un elemento y un conjunto, agrega el elemento al conjunto.
addS :: Eq a => a -> Set a -> Set a
addS x (ConsS xs) =
	if elem x xs
	   then ConsS xs
	   else ConsS (x:xs)

--Dados un elemento y un conjunto indica si el elemento pertenece al conjunto.
belongs :: Eq a => a -> Set a -> Bool
belongs x (ConsS xs) = elem x xs

--Devuelve la cantidad de elementos distintos de un conjunto.
sizeS :: Eq a => Set a -> Int
sizeS (ConsS xs) = length xs


-- Costo: lineal
removeS :: Eq a => a -> Set a -> Set a
removeS x (ConsS xs) = ConsS (sacarUno x xs)

-- Costo: lineal
sacarUno :: Eq a => a -> [a] -> [a]
sacarUno x [] = []
sacarUno x (y:ys) =
	if x == y
	   then ys
	   else y : sacarUno x ys

--Dados dos conjuntos devuelve un conjunto con todos los elementos de ambos. conjuntos.	
-- Costo: cuadratica
unionS :: Eq a => Set a -> Set a -> Set a
unionS (ConsS xs) (ConsS ys) = ConsS (unirListas xs ys)

-- Costo: cuadratica
unirListas :: Eq a => [a] -> [a] -> [a]
unirListas [] ys = ys
unirListas (x:xs) ys =
	if elem x ys
	   then unirListas xs ys
	   else x : unirListas xs ys

--Dado un conjunto devuelve una lista con todos los elementos distintos del conjunto.	   
setToList :: Eq a => Set a -> [a]
setToList (ConsS xs) = elemSinRepetir xs

elemSinRepetir :: Eq a => [a] -> [a]
elemSinRepetir [] = []
elemSinRepetir  (x:xs) = 
	if elem x xs
	   then elemSinRepetir xs
	   else x : ( elemSinRepetir xs )

	   