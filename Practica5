import Set1

--Como usuario del tipo abstracto Set implementar las siguientes funciones:

--2 SET

data Tree a = EmptyT | NodeT  a (Tree a) (Tree a)

--Dados una lista y un conjunto, devuelve una lista con todos los elementos que pertenecen
--al conjunto.
losQuePertenecen :: Eq a => [a] -> Set a -> [a]
losQuePertenecen [] _ = []
losQuePertenecen (x:xs) set = 
		if belongs x set
		then x : ( losQuePertenecen  xs set )
		else losQuePertenecen  xs set

--Quita todos los elementos repetidos de la lista dada utilizando un conjunto como es-
--tructura auxiliar.
sinRepetidos :: Eq a => [a] -> [a]
sinRepetidos xs = setToList (toSet xs)

toSet :: Eq a => [a] -> Set a
toSet [] = emptyS
toSet (x:xs) = addS x (toSet xs)

--Dado un arbol de conjuntos devuelve un conjunto con la union de todos los conjuntos
--del arbol.
unirTodos :: Eq a => Tree (Set a) -> Set a
unirTodos  EmptyT = emptyS
unirTodos (NodeT set t1 t2) = unionS set(unionS(unirTodos t1)(unirTodos t2))
		   
			   
s1 :: Set Int
s1 = toSet [1,1,2,2,3,4,4,5,6,7,8,9,10,10,10,10]
