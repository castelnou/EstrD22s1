module Queue1 (
	Queue,
	queue,
	emptyQ,
	isEmptyQ,
	firstQ,
	dequeue
 ) where

data Queue a = Q [a] deriving Show

--Crea una cola vacía.
emptyQ :: Queue a
emptyQ = Q []

--Dada una cola indica si la cola está vacía.
isEmptyQ :: Queue a -> Bool
isEmptyQ  (Q xs) = null xs

--Dados un elemento y una cola, agrega ese elemento a la cola.
queue :: a -> Queue a -> Queue a
queue x ( Q xs ) =  Q ( x:xs ) 

-- Dada una cola devuelve el primer elemento de la cola.
firstQ :: Queue a -> a
firstQ (Q xs) = primero xs

-- Dada una cola la devuelve sin su primer elemento.
dequeue :: Queue a -> Queue a
dequeue (Q xs) = Q (sinPrimero xs)

primero:: [a] -> a
primero   []   = error "Lista Vacia"
primero (x:xs) = x 

sinPrimero:: [a]-> [a]
sinPrimero   []  = []
sinPrimero (x:xs)= xs