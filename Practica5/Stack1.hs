module Stack1 (
			Stack, 
			emptyS, 
			isEmptyS, 
			push, 
			top, 
			pop, 
			lenS
) where

data Stack a = S [a]

--Crea una pila vacía.
emptyS :: Stack a
emptyS = S []

--Dada una pila indica si está vacía
isEmptyS :: Stack a -> Bool
isEmptyS (S xs) = null xs

--Dados un elemento y una pila, agrega el elemento a la pila.
push :: a -> Stack a -> Stack a
push x (S xs) = S (x:xs)

--Dada un pila devuelve el elemento del tope de la pila.
top :: Stack a -> a
top (S xs) = head xs

--Dada una pila devuelve la pila sin el primer elemento.
pop :: Stack a -> Stack a
pop (S xs) = S (tail xs)

--Dada una pila devuelve la pila sin el primer elemento.
lenS :: Stack a -> Int
lenS (S xs) = length xs