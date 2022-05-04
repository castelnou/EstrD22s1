import Stack1
			   
--4 STACK

--Dada una lista devuelve una pila sin alterar el orden de los elementos.
apilar :: [a] -> Stack a
apilar [] = emptyS
apilar (x:xs) = push x (apilar xs)

--Dada una pila devuelve una lista sin alterar el orden de los elementos
desapilar :: Stack a -> [a]
desapilar s =
  if (isEmptyS s)
    then []
    else (top s) : (desapilar  (pop s))

--Dada una posicion v�lida en la stack y un elemento, ubica dicho elemento en dicha
--posici�n (se desapilan elementos hasta dicha posici�n y se inserta en ese lugar).
insertarEnPos :: Int -> a -> Stack a -> Stack a
insertarEnPos 0 x s = push x s
insertarEnPos n x s =
  if(isEmptyS s)
    then error "No hay posicion"
    else push (top s) (insertarEnPos (n-1) x (pop s))			   
			   
			   