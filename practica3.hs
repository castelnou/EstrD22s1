data Color = Azul | Rojo deriving Show
data Celda = Bolita Color Celda | CeldaVacia 

cel1 = Bolita Azul ( Bolita Rojo ( Bolita Rojo CeldaVacia ) )

--Dados un color y una celda, indica la cantidad de bolitas de ese color. Nota: pensar si ya
--existe una operación sobre listas que ayude a resolver el problema.
esIgual :: Color -> Color -> Bool
esIgual Rojo Rojo = True
esIgual Azul Azul = True
esIgual _ _ = False


unoSi :: Bool -> Int
unoSi True  = 1
unoSi False = 0

nroBolitas :: Color -> Celda -> Int
nroBolitas _ CeldaVacia = 0
nroBolitas c ( Bolita col cel ) = unoSi ( esIgual c col ) +  nroBolitas c cel
--	if esIgual c col
--	then 1 + nroBolitas c cel
--	else nroBolitas c cel
	
--Dado un color y una celda, agrega una bolita de dicho color a la celda.	
poner :: Color -> Celda -> Celda
poner c cel = Bolita c cel
	
--Dado un color y una celda, quita una bolita de dicho color de la celda. Nota: a diferencia de
--Gobstones, esta función es total.	
sacar :: Color -> Celda -> Celda
sacar col CeldaVacia = CeldaVacia
sacar col ( Bolita c cel) = 
	if esIgual col c 
	then cel
	else Bolita c ( sacar col cel )

--Dado un número n, un color c, y una celda, agrega n bolitas de color c a la celda.	
ponerN :: Int -> Color -> Celda -> Celda
ponerN 0 col cel = cel
ponerN num col cel  = Bolita col ( ponerN ( num - 1 ) col cel )


data Objeto = Cacharro | Tesoro deriving Show
data Camino = Fin | Cofre [Objeto] Camino | Nada Camino deriving Show


cam1 = Cofre [ Tesoro ] (Nada (Nada (Nada (Cofre [Tesoro] Fin)))) 

esTesoro :: Objeto -> Bool
esTesoro Tesoro = True
esTesoro o = False

tieneTesoro :: [Objeto] -> Bool
tieneTesoro [] = False
tieneTesoro (x:xs) = esTesoro x || tieneTesoro xs

--Indica si hay un cofre con un tesoro en el camino.
hayTesoro :: Camino -> Bool
hayTesoro Fin = False
hayTesoro ( Nada cam ) = hayTesoro cam
hayTesoro ( Cofre obj cam ) = tieneTesoro obj || hayTesoro cam

--Indica la cantidad de pasos que hay que recorrer hasta llegar al primer cofre con un tesoro.
--Si un cofre con un tesoro está al principio del camino, la cantidad de pasos a recorrer es 0.
--Precondición: tiene que haber al menos un tesoro.
pasosHastaTesoro :: Camino -> Int
pasosHastaTesoro Fin = 0
pasosHastaTesoro ( Nada cam ) = 1 + pasosHastaTesoro cam
pasosHastaTesoro ( Cofre obj cam ) = 
	if tieneTesoro obj 
	then 0 
	else pasosHastaTesoro cam

--Indica si hay un tesoro en una cierta cantidad exacta de pasos. Por ejemplo, si el número de
--pasos es 5, indica si hay un tesoro en 5 pasos.	
hayTesoroEn :: Int -> Camino -> Bool
hayTesoroEn _ Fin = False
hayTesoroEn n cam = hayTesoroEnCelda ( avanzarNPasos n cam )

avanzarNPasos :: Int -> Camino -> Camino
avanzarNPasos _ Fin = Fin
avanzarNPasos 0 cam = cam
avanzarNPasos n cam = avanzarNPasos ( n - 1 ) ( avanzarUnPaso cam )

avanzarUnPaso :: Camino -> Camino
avanzarUnPaso Fin = Fin
avanzarUnPaso (Nada cam) = cam
avanzarUnPaso ( Cofre obj cam ) = cam	

hayTesoroEnCelda :: Camino -> Bool
hayTesoroEnCelda ( Cofre obj cam ) = tieneTesoro obj
hayTesoroEnCelda _ = False

--Indica si hay al menos “n” tesoros en el camino
alMenosNTesoros :: Int -> Camino -> Bool
alMenosNTesoros 0 _ = True
alMenosNTesoros _ Fin = False
alMenosNTesoros n (Nada cam) = alMenosNTesoros n cam
alMenosNTesoros n ( Cofre obj cam ) = n > cantidadTesoros obj || alMenosNTesoros ( n - cantidadTesoros obj ) cam
									  
cantidadTesoros :: [Objeto] -> Int
cantidadTesoros [] = 0
cantidadTesoros (x:xs) = unoSi ( esTesoro x ) + cantidadTesoros xs



--Dado un rango de pasos, indica la cantidad de tesoros que hay en ese rango. Por ejemplo, si
--el rango es 3 y 5, indica la cantidad de tesoros que hay entre hacer 3 pasos y hacer 5. Están
--incluidos tanto 3 como 5 en el resultado.(desafío) 
cantTesorosEntre :: Int -> Int -> Camino -> Int
cantTesorosEntre n1 n2 cam = cantTesorosHastaN (n2-n1) (avanzarHasta n1 cam)

avanzarHasta :: Int -> Camino -> Int
avanzarHasta 0 c = c
avanzarHasta n Fin = Fin
avanzarHasta n (Nada c) = avanzarHasta (n-1) c
avanzarHasta n (Cofre os c) = avanzarHasta (n-1) c

cantTesorosHasta :: Int -> Camino -> Int
cantTesorosHasta 0 c = 0
cantTesorosHasta n Fin = 0
cantTesorosHasta n (Nada c) = cantTesorosHasta (n-1) c
cantTesorosHasta n (Cofre os c) = cantidadTesoros os + cantTesorosHasta (n-1) c

--2.1. Árboles binarios
--Dada esta definición para árboles binarios
data Tree a = EmptyT | NodeT a (Tree a) (Tree a)

--1.Dado un árbol binario de enteros devuelve la suma entre sus elementos.
sumarT :: Tree Int -> Int
sumarT EmptyT _ = 0
sumarT (NodeT num ti td) = num + sumarT ti + sumarT td


--2.Dado un árbol binario devuelve su cantidad de elementos, es decir, el tamaño del árbol (size
--en inglés).
sizeT :: Tree a -> Int
sizeT EmptyT = 0
sizeT (NodeT _ ti td) = 1 + sizeT ti + sizeT td


--3.Dado un árbol de enteros devuelve un árbol con el doble de cada número.
mapDobleT :: Tree Int -> Tree Int
mapDobleT EmptyT = EmptyT
mapDobleT (NodeT num ti td) = NodeT (n*2) (mapDobleT ti) (mapDobleT td)

--4.Dados un elemento y un árbol binario devuelve True si existe un elemento igual a ese en el
--árbol.
perteneceT :: Eq a => a -> Tree a -> Bool
perteneceT _ EmptyT = False
perteneceT n (NodeT num ti td) = n == num || perteneceT n ti || perteneceT n td


--5.Dados un elemento e y un árbol binario devuelve la cantidad de elementos del árbol que son
--iguales a e.
aparicionesT :: Eq a => a -> Tree a -> Int
aparicionesT _ EmptyT = 0
aparicionesT n (NodeT num ti td) = unoSi (n == num) + aparicionesT n ti + aparicionesT n td

--6.Dado un árbol devuelve los elementos que se encuentran en sus hojas
leaves :: Tree a -> [a]
leaves EmptyT = []
leaves (NodeT num EmptyT EmptyT) = [num]
leaves (NodeT num ti td) = leaves ti ++ leaves td 

--7. Dado un árbol devuelve su altura.
--Nota: la altura de un árbol (height en inglés), también llamada profundidad, es la cantidad
--de niveles del árbol1
--. La altura para EmptyT es 0, y para una hoja es 1.
heightT :: Tree a -> Int
heightT EmptyT = 0
heightT (NodeT num ti td) = 1 + (heightT ti) + (heightT td)

--8. Dado un árbol devuelve el árbol resultante de intercambiar el hijo izquierdo con el derecho,
--en cada nodo del árbol.
mirrorT :: Tree a -> Tree a
mirrorT EmptyT = EmptyT 
mirrorT (NodeT num ti td) = (NodeT num (mirrorT td ) (mirrorT ti))


--9. Dado un árbol devuelve una lista que representa el resultado de recorrerlo en modo in-order.
--Nota: En el modo in-order primero se procesan los elementos del hijo izquierdo, luego la raiz
--y luego los elementos del hijo derecho.
toList :: Tree a -> [a]
toList EmptyT = []
toList (NodeT num ti td ) = toList ti ++ [num] ++ toList td


--10.Dados un número n y un árbol devuelve una lista con los nodos de nivel n. El nivel de un
--nodo es la distancia que hay de la raíz hasta él. La distancia de la raiz a sí misma es 0, y la
--distancia de la raiz a uno de sus hijos es 1. Nota: El primer nivel de un árbol (su raíz) es 0.
levelN :: Int -> Tree a -> [a]
levelN _ EmptyT = []
levelN 0 (NodeT num _ _  ) = [num]
levelN n (NodeT num ti td) = levelN (n-1) ti ++ levelN (n-1) td


--11.Dado un árbol devuelve una lista de listas en la que cada elemento representa un nivel de
--dicho árbol.
listPerLevel :: Tree a -> [[a]]
listPerLevel t = listPerLevelAux t (heightT t)

listPerLevelAux :: Tree a -> Int -> [[a]]
listPerLevelAux EmptyT _ = []
listPerLevelAux t 0 = [levelN 0 t]
listPerLevelAux t p = listPerLevelAux t (p-1) ++ [levelN p t]  

--12.Devuelve los elementos de la rama más larga del árbol
ramaMasLarga :: Tree a -> [a]
ramaMasLarga EmptyT = []
ramaMasLarga (NodeT num ti td) = masLarga (ramaMasLarga ti) (ramaMasLarga td)

masLarga :: [a] -> [a] -> [a]
masLarga ti td = if long l1 > long l2 
				 then ti
				 else td 

long :: [a] -> Int
long []  = 0
long (x:xs) = 1 + long xs

--13.Dado un árbol devuelve todos los caminos, es decir, los caminos desde la raiz hasta las hojas.
todosLosCaminos :: Tree a  -> [[a]]
todosLosCaminos EmptyT = []
todosLosCaminos (NodeT num ti td)   = [num] : agregarCamino num (todosLosCaminos ti) ++ agregarCamino num (todosLosCaminos td)

agregarCamino :: a -> [[a]] -> [[a]]
agregarCamino x [] = []
agregarCamino x (xs:xss) = (x:xs) : agregarCamino x xss

-- 2.2. Expresiones Aritméticas
-- El tipo algebraico ExpA modela expresiones aritméticas de la siguiente manera:
data ExpA = Valor Int | Sum ExpA ExpA | Prod ExpA ExpA | Neg ExpA 


--1 Dada una expresión aritmética devuelve el resultado evaluarla
eval :: ExpA -> Int
eval (Valor n) = n
eval (Sum e1 e2) = eval e1 + eval e2
eval (Prod e1 e2) = eval e1 * eval e2
eval (Neg expA) = eval e1 * (-1)

--2 Dada una expresión aritmética, la simplifica según los siguientes criterios (descritos utilizando
--notación matemática convencional):

simplificar :: ExpA -> ExpA 
simplificar (Valor e    )  = Valor e
simplificar (Sum   e1 e2)  = simplificarSuma (simplificar e1) (simplificar e2) 
simplificar (Prod  e1 e2)  = simplificarProducto (simplificar e1) (simplificar e2) 
simplificar (Neg   e    )  = simplificarNegativo (simplificar e)


simplificarSuma :: ExpA -> ExpA -> ExpA
simplificarSuma (Valor 0) e = e
simplificarSuma e (Valor 0 )  = e
simplificarSuma e1 e2 = Sum e1 e2 

simplificarProducto :: ExpA -> ExpA -> ExpA 
simplificarProducto (Valor 0) _ = Valor 0
simplificarProducto _ (Valor 0) = Valor 0
simplificarProducto (Valor 1) e = e
simplificarProducto e (Valor 1) = e
simplificarProducto e1 e2 = Prod e1 e2    

simplificarNegativo :: ExpA -> ExpA
simplificarNegativo (Neg x) = x
simplificarNegativo x = Neg x


			
			
