--1. Pizzas

data Pizza = Prepizza | Capa Ingrediente Pizza deriving Show
data Ingrediente = Salsa | Queso | Jamon | Aceitunas Int deriving Show

pizza1 = Capa Jamon ( Capa Queso Prepizza ) 
pizza2 = Capa Salsa ( Capa Queso Prepizza ) 
pizza3 = Capa ( Aceitunas 2 ) ( Capa ( Aceitunas 4) Prepizza ) 

--1.Dada una pizza devuelve la cantidad de ingredientes
cantidadDeCapas :: Pizza -> Int
cantidadDeCapas Prepizza = 0
cantidadDeCapas (Capa _ p) = 1 + cantidadDeCapas p

--2.Dada una lista de ingredientes construye una pizza
armarPizza :: [Ingrediente] -> Pizza
armarPizza [] = Prepizza
armarPizza (x:xs) = Capa x (armarPizza xs)

--3.Le saca los ingredientes que sean jamón a la pizza
sacarJamon :: Pizza -> Pizza
sacarJamon Prepizza = Prepizza
sacarJamon (Capa ing p) = if esJamon ing
						  then sacarJamon p
						  else Capa ing ( sacarJamon p )

esJamon :: Ingrediente -> Bool
esJamon Jamon = True
esJamon _     = False

esQueso :: Ingrediente -> Bool
esQueso Queso = True
esQueso _     = False

esSalsa :: Ingrediente -> Bool
esSalsa Salsa = True
esSalsa _     = False

esAceituna :: Ingrediente -> Bool
esAceituna (Aceitunas _) = True
esAceituna _ = False

--4.Dice si una pizza tiene salsa y queso
tieneSoloSalsaYQueso :: Pizza -> Bool
tieneSoloSalsaYQueso Prepizza = True
tieneSoloSalsaYQueso (Capa ing p) =  (esQueso ing || esSalsa ing ) && tieneSoloSalsaYQueso p

--5.Recorre cada ingrediente y si es aceitunas duplica su cantidad
duplicarAceitunas :: Pizza -> Pizza
duplicarAceitunas Prepizza = Prepizza
duplicarAceitunas (Capa ing p) = if esAceituna ing
								 then Capa ( duplicar ing ) ( duplicarAceitunas p )
								 else Capa ing ( duplicarAceitunas p)
								 
duplicar :: Ingrediente -> Ingrediente
duplicar (Aceitunas cant) = Aceitunas ( cant * 2 )
duplicar ing = ing


--6.Dada una lista de pizzas devuelve un par donde la primera componente es la cantidad de
--ingredientes de la pizza, y la respectiva pizza como segunda componente.
cantCapasPorPizza :: [Pizza] -> [(Int, Pizza)]
cantCapasPorPizza [] = []
cantCapasPorPizza (x:xs) =  ( cantidadIngredientes x , x ) :  cantCapasPorPizza xs

cantidadIngredientes :: Pizza -> Int
cantidadIngredientes Prepizza = 0
cantidadIngredientes (Capa ing p) = cantidad ing + cantidadIngredientes p

cantidad :: Ingrediente -> Int
cantidad (Aceitunas num ) = num
cantidad _ = 1


-- 2. Mapa de tesoros (con bifurcaciones)

data Dir = Izq | Der
data Objeto = Tesoro | Chatarra
data Cofre = Cofre [Objeto]
data Mapa = Fin Cofre | Bifurcacion Cofre Mapa Mapa

mapa1 :: Mapa
mapa1 = Fin (Cofre [Chatarra])

mapa2 :: Mapa
mapa2 = 
  Bifurcacion (Cofre [Chatarra])
              (Fin (Cofre [Tesoro]))
              (Fin (Cofre [Tesoro]))

mapa3 :: Mapa
mapa3 = Bifurcacion (Cofre [Chatarra]) 
                    mapa1
                    mapa1

mapa4 :: Mapa
mapa4 = Bifurcacion (Cofre [Tesoro])
                    mapa3
                    mapa3


--1 Indica si hay un tesoro en alguna parte del mapa.
hayTesoro :: Mapa -> Bool
hayTesoro ( Fin c ) = tieneTesoro c
hayTesoro (Bifurcacion c mi md) = tieneTesoro c || hayTesoro mi || hayTesoro md

tieneTesoro :: Cofre -> Bool
tieneTesoro (Cofre xs) = algunoEsTesoro xs

algunoEsTesoro :: [Objeto] -> Bool
algunoEsTesoro [] = False
algunoEsTesoro (x:xs) = esTesoro x || algunoEsTesoro xs

esTesoro :: Objeto -> Bool
esTesoro Tesoro = True
esTesoro _ = False

--2 Indica si al final del camino hay un tesoro. Nota: el final de un camino se representa con una
--lista vacía de direcciones.
hayTesoroEn :: [Dir] -> Mapa -> Bool
hayTesoroEn [] map = hayTesoroEnFin map
hayTesoroEn (x:xs) map = hayTesoroEn xs ( moverEnDir x map )

hayTesoroEnFin :: Mapa -> Bool
hayTesoroEnFin ( Fin c ) = tieneTesoro c
hayTesoroEnFin (Bifurcacion c mi md) = tieneTesoro c 

moverEnDir :: Dir -> Mapa -> Mapa
moverEnDir _ (Fin c) = error "No tengo donde moverme"
moverEnDir d (Bifurcacion c mi md) = if esIzq d
									 then mi
									 else md
									 
esIzq :: Dir -> Bool
esIzq Izq = True
esIzq _   = False

--3 Indica el camino al tesoro. Precondición: existe un tesoro y es único.
caminoAlTesoro :: Mapa -> [Dir]
caminoAlTesoro (Fin c) = []
caminoAlTesoro (Bifurcacion c mi md) = if tieneTesoro c 
										then []
										else if hayTesoro mi
											then Izq : caminoAlTesoro mi 
											else Der : caminoAlTesoro md

--4 Indica el camino de la rama más larga.
caminoDeLaRamaMasLarga :: Mapa -> [Dir] 
caminoDeLaRamaMasLarga (Fin cofre) = []
caminoDeLaRamaMasLarga (Bifurcacion _ mi md) =  if heightM mi > heightM md 
                                                    then Izq : caminoDeLaRamaMasLarga mi 
                                                    else Der : caminoDeLaRamaMasLarga md   

heightM :: Mapa -> Int
heightM (Fin _) = 0
heightM (Bifurcacion _ mi md) = 1 + max (heightM mi) (heightM md)

-- 5 Devuelve los tesoros separados por nivel en el árbol.
tesorosPorNivel :: Mapa -> [[Objeto]]
tesorosPorNivel (Fin c) = tesorosEnCofre c : []
tesorosPorNivel (Bifurcacion c mi md) = 
    tesorosEnCofre c : juntarPorNivel (tesorosPorNivel mi) (tesorosPorNivel md)

tesorosEnCofre :: Cofre -> [Objeto]
tesorosEnCofre (Cofre xs) = objetos xs

objetos :: [Objeto] -> [Objeto]
objetos [] = []
objetos (x:xs) = if esTesoro x
		then x : objetos xs
		else objetos xs

juntarPorNivel :: [[Objeto]] -> [[Objeto]] -> [[Objeto]]
juntarPorNivel [] yss = yss
juntarPorNivel xss [] = xss
juntarPorNivel (xs:xss) (ys:yss) = (xs ++ ys) : juntarPorNivel xss yss

--6 Devuelve todos lo caminos en el mapa.
todosLosCaminos :: Mapa -> [[Dir]]
todosLosCaminos (Fin c) = [[]]
todosLosCaminos (Bifurcacion c mi md) = 
	[] : agregarATodos Izq (todosLosCaminos mi) ++ agregarATodos Der (todosLosCaminos md)

agregarATodos :: Dir -> [[Dir]] -> [[Dir]]
agregarATodos d [] = []
agregarATodos d (xs:xss) = (d:xs) : agregarATodos d xss


data Componente = LanzaTorpedos | Motor Int | Almacen [Barril]
data Barril = Comida | Oxigeno | Torpedo | Combustible
data Sector = S SectorId [Componente] [Tripulante]
type SectorId = String
type Tripulante = String
data Tree a = EmptyT | NodeT a (Tree a) (Tree a)
data Nave = N (Tree Sector)



-- 1) Devuelve todos los sectores de la nave.
sectores :: Nave -> [SectorId]
sectores (N t) = sectoresT t

sectoresT :: Tree Sector -> [SectorId]
sectoresT EmptyT  = []
sectoresT (NodeT s si sd) =
	sectorId s : sectoresT si ++ sectoresT sd

sectorId :: Sector -> String
sectorId (S i _ _) = i

-- 2) Propósito: Devuelve la suma de poder de propulsión de todos los motores de la nave. Nota:
-- el poder de propulsión es el número que acompaña al constructor de motores.
poderDePropulsion :: Nave -> Int
poderDePropulsion (N t) = poderDePropulsionT t

poderDePropulsionT :: Tree Sector -> Int
poderDePropulsionT EmptyT = 0
poderDePropulsionT (NodeT s si sd) = 
   propulsionS s + poderDePropulsionT si + poderDePropulsionT sd         

propulsionS :: Sector -> Int
propulsionS (S _ cs _) = propulsionC cs

propulsionC :: [Componente] -> Int
propulsionC [] = 0
propulsionC (c:cs) = propulsion c + propulsionC cs
        
propulsion :: Componente -> Int
propulsion (Motor n) = n
propulsion _ = 0

--3) Propósito: Devuelve todos los barriles de la nave.
barriles :: Nave -> [Barril]
barriles (N t) = barrilesT t

barrilesT :: Tree Sector -> [Barril]
barrilesT EmptyT = []
barrilesT (NodeT s si sd) = barrilesS s ++ barrilesT si ++ barrilesT sd

barrilesS :: Sector -> [Barril]
barrilesS (S _ cs _) = barrilesC cs

barrilesC :: [Componente] -> [Barril]
barrilesC []     = []
barrilesC (c:cs) = barril c ++ barrilesC cs

barril :: Componente -> [Barril]
barril (Almacen bs) = bs
barril _ = []

-- 4) Propósito: Añade una lista de componentes a un sector de la nave.
--  Nota: ese sector puede no existir, en cuyo caso no añade componentes
agregarASector :: [Componente] -> SectorId -> Nave -> Nave
agregarASector cs i (N t) = N (agregarASectorT cs i t)

agregarASectorT :: [Componente] -> SectorId -> Tree Sector -> Tree Sector
agregarASectorT cs i EmptyT  = EmptyT
agregarASectorT cs i (NodeT s si sd) = 
	if esSectorConId s i
	    then NodeT (agregarASectorS cs s) si sd
	    else NodeT s (agregarASectorT cs i si) (agregarASectorT cs i sd)

agregarASectorS :: [Componente] -> Sector -> Sector
agregarASectorS [] s = s
agregarASectorS cs1 (S i cs2 tr) = (S i (cs1 ++ cs2) tr)

esSectorConId :: Sector -> SectorId -> Bool
esSectorConId (S i _ _) sid = i == sid

-- 5) Incorpora un tripulante a una lista de sectores de la nave.
-- Precondición: Todos los id de la lista existen en la nave.
asignarTripulanteA :: Tripulante -> [SectorId] -> Nave -> Nave
asignarTripulanteA t ids (N tr) = N (asignarTripulanteAT t ids tr)

asignarTripulanteAT :: Tripulante -> [SectorId] -> Tree Sector -> Tree Sector
asignarTripulanteAT _ _   EmptyT = EmptyT
asignarTripulanteAT t ids (NodeT s si sd) = 
    NodeT (asignarTripulanteAS t ids s) (asignarTripulanteAT t ids si) (asignarTripulanteAT t ids sd) 
    
    
asignarTripulanteAS :: Tripulante -> [SectorId] -> Sector -> Sector    
asignarTripulanteAS t [] s = s
asignarTripulanteAS t (id:ids) s =
    if esSectorConId s id
        then asignarTripulante t s
	    else asignarTripulanteAS t ids s

asignarTripulante :: Tripulante -> Sector -> Sector
asignarTripulante t (S i cs ts) = S i cs (t:ts) 

-- 6) Devuelve los sectores en donde aparece un tripulante dado.
sectoresAsignados :: Tripulante -> Nave -> [SectorId]
sectoresAsignados tr (N t) = sectoresAsignadosT tr t

sectoresAsignadosT :: Tripulante -> Tree Sector -> [SectorId]
sectoresAsignadosT t EmptyT  = []
sectoresAsignadosT t (NodeT s si sd) = 
	singularSi (sectorId s) (esAsignadoA t s) ++ sectoresAsignadosT t si ++ sectoresAsignadosT t sd

esAsignadoA :: Tripulante -> Sector -> Bool
esAsignadoA t (S _ _ ts) = elem t ts

singularSi :: a -> Bool -> [a]
singularSi x True  = [x]
singularSi x False = [] 


-- 4. MANADA DE LOBOS

type Presa = String -- nombre de presa
type Territorio = String -- nombre de territorio
type Nombre = String -- nombre de lobo
data Lobo = Cazador Nombre [Presa] Lobo Lobo Lobo | Explorador Nombre [Territorio] Lobo Lobo | Cria Nombre
data Manada = M Lobo

-- 1) Construir un valor de tipo Manada que posea 1 cazador, 2 exploradores y que el resto sean
--    crías. Resolver las siguientes funciones utilizando recursión estructural sobre la estructura
--    que corresponda en cada caso.

manada = M (Cazador "perro1" ["1","2","3","4"] 
               (Explorador "perro2" [] 
	               (Cria "perro4") 
	               (Cria "perro5"))
	           (Explorador "perro3" []
	               (Cria "perro6")
	               (Cria "perro7"))
	           (Cria "perro8"))

-- 2) dada una manada, indica si la cantidad de alimento cazado es mayor a la cantidad de crías.
buenaCaza :: Manada -> Bool 
buenaCaza (M l) = cantDePresas l > cantDeCrias l

-- dado un lobo, describe la cantidad de presas que cazaron. 
cantDePresas :: Lobo -> Int
cantDePresas (Cria n) = 0
cantDePresas (Explorador n ts l1 l2) = cantDePresas l1 + cantDePresas l2 
cantDePresas (Cazador n ps l1 l2 l3) = 
    length ps + cantDePresas l1 + cantDePresas l2 + cantDePresas l3

cantDeCrias :: Lobo -> Int
cantDeCrias (Cria n) = 1
cantDeCrias (Explorador n ts l1 l2) = cantDeCrias l1 + cantDeCrias l2
cantDeCrias (Cazador n ps l1 l2 l3) = cantDeCrias l1 + cantDeCrias l2 + cantDeCrias l3

-- 3) dada una manada, devuelve el nombre del lobo con más presas cazadas, junto con su cantidad de
--presas. Nota: se considera que los exploradores y crías tienen cero presas cazadas, y que 
--podrían formar parte del resultado si es que no existen cazadores con más de cero presas
elAlfa :: Manada -> (Nombre, Int)
elAlfa (M l) = elAlfaL l

elAlfaL :: Lobo -> (Nombre, Int)
elAlfaL (Cria n) = (n,0)
elAlfaL (Explorador n ts l1 l2) = mayorCazador (n,0) (mayorCazador  (elAlfaL l1) (elAlfaL l2))
elAlfaL (Cazador n ps l1 l2 l3) = 
	mayorCazador (n,length ps) (mayorCazador (mayorCazador  (elAlfaL l1) (elAlfaL l2)) (elAlfaL l3))

mayorCazador :: (Nombre, Int) -> (Nombre, Int) -> (Nombre, Int)
mayorCazador c1 c2 = 
	if snd c1 > snd c2
		then c1
		else c2

-- 4) dado un territorio y una manada, devuelve los nombres de los exploradores que
--pasaron por dicho territorio.
losQueExploraron :: Territorio -> Manada -> [Nombre]
losQueExploraron t (M l) = losQueExploraronL t l

losQueExploraronL :: Territorio -> Lobo -> [Nombre]
losQueExploraronL t (Cria n) = []
losQueExploraronL t (Explorador n ts l1 l2) = singularSi n (elem t ts) ++ losQueExploraronL t l1 ++ losQueExploraronL t l2
losQueExploraronL t (Cazador n ps l1 l2 l3) = 
    losQueExploraronL t l1 ++ losQueExploraronL t l2 ++ losQueExploraronL t l3


-- 5) dada una manada, denota la lista de los pares cuyo primer elemento es un territorio y cuyo segundo 
--    elemento es la lista de los nombres de los exploradores que exploraron  dicho territorio. Los 
--    territorios no deben repetirse.
exploradoresPorTerritorio :: Manada -> [(Territorio, [Nombre])]
exploradoresPorTerritorio (M l) = exploradoresPorTerritorioAux l

exploradoresPorTerritorioAux :: Lobo -> [(Territorio, [Nombre])]
exploradoresPorTerritorioAux (Cria _)  = []
exploradoresPorTerritorioAux (Explorador n ts l1 l2) = agregarLoboA n ts ( exploradoresPorTerritorioAux l1 ++ 
																		  exploradoresPorTerritorioAux l2 )
exploradoresPorTerritorioAux (Cazador _ _ l1 l2 l3) = exploradoresPorTerritorioAux l1 ++ 
													  exploradoresPorTerritorioAux l2 ++ 
													  exploradoresPorTerritorioAux l3

agregarLoboA :: Nombre -> [Territorio] -> [(Territorio, [Nombre])] -> [(Territorio, [Nombre])]
agregarLoboA _ [] xs = xs
agregarLoboA n (t:ts) xs = agregarLobo n t (agregarLoboA n ts xs)

agregarLobo :: Nombre -> Territorio -> [(Territorio, [Nombre])] -> [(Territorio, [Nombre])]
agregarLobo n t []     = (t, n : []) : []
agregarLobo n t (x:xs) = agregarExploradorA n t x : (agregarLobo n t xs)

agregarExploradorA :: Nombre -> Territorio -> (Territorio, [Nombre]) -> (Territorio, [Nombre])
agregarExploradorA n t tup = if t == fst tup
							 then agregarNom n tup
							 else tup
							
agregarNom :: Nombre -> (Territorio, [Nombre]) -> (Territorio, [Nombre])
agregarNom n (t,ns) = (t, n : ns)

-- 6) dado un nombre de cazador y una manada, indica el nombre de todos los cazadores que tienen
--como subordinado al cazador dado (directa o indirectamente). 
--Precondición: hay un cazador con dicho nombre y es único.
superioresDelCazador :: Nombre -> Manada -> [Nombre]
superioresDelCazador n (M l) = superioresDe n l

superioresDe :: Nombre -> Lobo -> [Nombre]
superioresDe n (Cria n') = []
superioresDe n (Explorador n' ts l1 l2) = if esSuperiorDe n l1 || esSuperiorDe n l2
                                            then n' : ((superioresDe n l1) ++ (superioresDe n l2))
                                            else []
superioresDe n (Cazador n' cs l1 l2 l3) = if esSuperiorDe n l1 || esSuperiorDe n l2 || esSuperiorDe n l3
                                            then n' : ((superioresDe n l1) ++ (superioresDe n l2) ++ (superioresDe n l3))
                                            else []

esSuperiorDe :: Nombre -> Lobo -> Bool
esSuperiorDe n (Cria n') = n == n'
esSuperiorDe n (Explorador n' ts l1 l2) = n == n' || esSuperiorDe n l1  || esSuperiorDe n l2
esSuperiorDe n (Cazador n' cs l1 l2 l3) = n == n' || esSuperiorDe n l1 || esSuperiorDe n l2 || esSuperiorDe n l3


