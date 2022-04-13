--1. Recursión sobre listas

--1 Dada una lista de enteros devuelve la suma de todos sus elementos.
sumatoria :: [Int] -> Int
sumatoria [] = 0
sumatoria (x:xs) = x + sumatoria xs

--2 Dada una lista de elementos de algún tipo devuelve el largo de esa lista, es decir, la cantidad
-- de elementos que posee.
longitud :: [a] -> Int
longitud [] = 0
longitud (x:xs) = 1 + longitud xs


--Dado un número entero devuelve su siguiente
sucesor :: Int -> Int
sucesor x = x + 1

--3 Dada una lista de enteros, devuelve la lista de los sucesores de cada entero.
sucesores :: [Int] -> [Int]
sucesores [] = []
sucesores (x:xs) = sucesor x : sucesores xs

--4 Dada una lista de booleanos devuelve True si todos sus elementos son True.
conjuncion :: [Bool] -> Bool
conjuncion [] = True
conjuncion (x:xs) = x && conjuncion xs

--5 Dada una lista de booleanos devuelve True si alguno de sus elementos es True.
disyuncion :: [Bool] -> Bool
disyuncion [] = False
disyuncion (x:xs) = x || disyuncion xs

--6 Dada una lista de listas, devuelve una única lista con todos sus elementos.
aplanar :: [[a]] -> [a]
aplanar [] = []
--aplanar (x:xs) = x ++ aplanar xs // Se reemplaza por la función
aplanar (x:xs) = concatenar x ( aplanar xs )


--7 Dados un elemento e y una lista xs devuelve True si existe un elemento en xs que sea igual
-- a e.
pertenece :: Eq a => a -> [a] -> Bool
pertenece x [] = False
pertenece e (x:xs) = 
	if e == x
	then True
	else pertenece e xs

--8 Dados un elemento e y una lista xs cuenta la cantidad de apariciones de e en xs.
apariciones :: Eq a => a -> [a] -> Int
apariciones e [] = 0
apariciones e (x:xs) = 
	if e == x
	then 1 + apariciones e xs
	else apariciones e xs

--9 Dados un número n y una lista xs, devuelve todos los elementos de xs que son menores a n.	
losMenoresA :: Int -> [Int] -> [Int]
losMenoresA n [] = []
losMenoresA n (x:xs) = 
	if x < n
	then x : losMenoresA n xs
	else losMenoresA n xs
	
--10. Dados un número n y una lista de listas, devuelve la lista de aquellas listas que tienen más
--de n elementos.	
lasDeLongitudMayorA :: Int -> [[a]] -> [[a]]
lasDeLongitudMayorA n [] = []
lasDeLongitudMayorA n (x:xs) = 
	if longitud x > n
	then x: lasDeLongitudMayorA n xs
	else lasDeLongitudMayorA n xs
	
--11. Dados una lista y un elemento, devuelve una lista con ese elemento agregado al final de la
--lista.
agregarAlFinal :: [a] -> a -> [a]
agregarAlFinal [] n = [n]
agregarAlFinal (x:xs) n = x : agregarAlFinal xs n
	
--12. Dadas dos listas devuelve la lista con todos los elementos de la primera lista y todos los
--elementos de la segunda a continuación. Definida en Haskell como ++.
concatenar :: [a] -> [a] -> [a]
concatenar [] xs2 = xs2
concatenar (x1:xs1) xs2 = x1 : concatenar xs1 xs2

--13.Dada una lista devuelve la lista con los mismos elementos de atrás para adelante. Definida
--en Haskell como reverse.
reversa :: [a] -> [a]
reversa [] = []
reversa (x:xs) = agregarAlFinal ( reversa xs ) x

--14. Dadas dos listas de enteros, devuelve una lista donde el elemento en la posición n es el
--máximo entre el elemento n de la primera lista y de la segunda lista, teniendo en cuenta que
--las listas no necesariamente tienen la misma longitud.	
zipMaximos :: [Int] -> [Int] -> [Int]
zipMaximos xs1 [] = xs1
zipMaximos [] xs2 = xs2
zipMaximos (x1:xs1) (x2:xs2) = ( maxDelPar (x1,x2) ) : zipMaximos xs1 xs2

maxDelPar :: (Int,Int) -> Int
maxDelPar ( x, y ) = max x y


--15. Dada una lista devuelve el mínimo
elMinimo :: Ord a => [a] -> a
elMinimo (x:[]) = x 
elMinimo (x:xs) = min x ( elMinimo xs )

elMinimo2 :: [Int] -> Int
elMinimo2 (x:[]) = x
elMinimo2 (x:xs) = min x ( elMinimo xs )

elMaximo :: [Int] -> Int
elMaximo (x:[]) = x
elMaximo (x:xs) = max x ( elMaximo xs )

soloMayorAN :: Int -> [Int] -> [Int]
soloMayorAN n [] = []
soloMayorAN n (x:xs) = 
	if x > n
	then x : soloMayorAN n xs
	else soloMayorAN n xs
	
	
porDos :: [Int] -> [Int]
porDos [] = []	
porDos (x:xs) = x*2 : porDos xs


lasDeLongitudMenoresA :: Int -> [[a]] -> [[a]]
lasDeLongitudMenoresA n [] = []
lasDeLongitudMenoresA n (x:xs) =
	if longitud x < n
	then x : lasDeLongitudMenoresA n xs
	else lasDeLongitudMenoresA n xs
	

--2. Recursión sobre números	
--1 Dado un número n se devuelve la multiplicación de este número y todos sus anteriores hasta
--llegar a 0. Si n es 0 devuelve 1. La función es parcial si n es negativo.	
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial ( n - 1 )

--2. Dado un número n devuelve una lista cuyos elementos sean los números comprendidos entre
--n y 1 (incluidos). Si el número es inferior a 1, devuelve la lista vacía.
cuentaRegresiva :: Int -> [Int]
cuentaRegresiva 0 = []
cuentaRegresiva n = n : cuentaRegresiva ( n - 1 )

--3. Dado un número n y un elemento e devuelve una lista en la que el elemento e repite n veces.
repetir :: Int -> a -> [a]
repetir 0 e = []
repetir n e = e : repetir ( n - 1 ) e


data Persona = ConsPersona String Int  deriving Show

pers1 = ConsPersona "Miguel" 10
pers2 = ConsPersona "Juan" 30
pers3 = ConsPersona "Pepe" 20

--Dados una edad y una lista de personas devuelve a las personas mayores a esa edad.
mayoresA :: Int -> [Persona] -> [Persona]
mayoresA _ [] = []
mayoresA i (p:ps) = if esMayorA (edadPersona p) i 
					then p : ( mayoresA i ps )
					else mayoresA i ps	

				
--Dada una lista de personas devuelve el promedio de edad entre esas personas. Precondición: la lista al menos posee una persona.
promedioEdad :: [Persona] -> Int
promedioEdad [] = error "Lista Vacia"
promedioEdad p = div ( sumarEdades p ) (cantidadPersonas p)
		
sumarEdades :: [Persona] -> Int
sumarEdades [] = 0
sumarEdades (x:xs) = edadPersona x + sumarEdades xs		

cantidadPersonas :: [Persona] -> Int
cantidadPersonas [] = 0
cantidadPersonas (x:xs) = 1 + cantidadPersonas xs
				
esMayorA :: Int -> Int -> Bool
esMayorA x y = x > y

nombre :: Persona -> String
nombre (ConsPersona n e) = n

edadPersona :: Persona -> Int
edadPersona (ConsPersona n e) = e

nombres :: [Persona] -> [String]
nombres [] = []
nombres (x:xs) = nombre x : nombres xs

--Dada una lista de personas devuelve la persona más vieja de la lista. Precondición: la
--lista al menos posee una persona.
elMasViejo :: [Persona] -> Persona
elMasViejo (x:[]) =  x 
elMasViejo (x:xs) = if esMayorA ( edadPersona x ) ( edadPersona ( elMasViejo xs ) )
				  then x
				  else elMasViejo xs


data TipoDePokemon = Agua | Fuego | Planta
data Pokemon = ConsPokemon TipoDePokemon Int
data Entrenador = ConsEntrenador String [Pokemon]
			
poke1 = ConsPokemon Agua 50
poke2 = ConsPokemon Fuego 30
poke3 = ConsPokemon Planta 80

entrenador0 = ConsEntrenador "Vacio" []
entrenador1 = ConsEntrenador "Ash" [poke1, poke2, poke3]
entrenador2 = ConsEntrenador "Brook" [poke2, poke2]
			
unoSiEsTrue :: Bool -> Int
unoSiEsTrue True = 1
unoSiEsTrue False = 0

esDeMismoTipo :: TipoDePokemon -> TipoDePokemon -> Bool
esDeMismoTipo Agua Agua = True
esDeMismoTipo Fuego Fuego = True
esDeMismoTipo Planta Planta = True
esDeMismoTipo _ _ = False			
			
--Devuelve la cantidad de Pokémon que posee el entrenador.				  
cantPokemon :: Entrenador -> Int
cantPokemon (ConsEntrenador n ps) = longitud ps

--Devuelve la cantidad de Pokémon de determinado tipo que posee el entrenador.
cantPokemonDe :: TipoDePokemon -> Entrenador -> Int
cantPokemonDe t (ConsEntrenador n ps) = cuentaMistmoTipo t ps

tipoPokemon :: Pokemon -> TipoDePokemon
tipoPokemon ( ConsPokemon t e ) = t

cuentaMistmoTipo :: TipoDePokemon -> [Pokemon] -> Int
cuentaMistmoTipo _ [] = 0
cuentaMistmoTipo t (x:xs) = unoSiEsTrue ( esDeMismoTipo t (tipoPokemon x)) + cuentaMistmoTipo t xs
--Dados dos entrenadores, indica la cantidad de Pokemon de cierto tipo, que le ganarían
--a los Pokemon del segundo entrenador.
losQueLeGanan :: TipoDePokemon -> Entrenador -> Entrenador -> Int
losQueLeGanan t ent1 ent2 = pokemonesQueGanan ( pokemonesDelTipo  t  (pokemonesEntrenador ent1) ) (pokemonesEntrenador ent2)


pokemonesDelTipo :: TipoDePokemon -> [Pokemon] -> [Pokemon] 
pokemonesDelTipo _ [] = []
pokemonesDelTipo t (x:xs) = if esDeMismoTipo t ( tipoPokemon x )
					then x : pokemonesDelTipo t xs
					else pokemonesDelTipo t xs

pokemonesQueGanan :: [Pokemon] -> [Pokemon] -> Int
pokemonesQueGanan [] _ = 0
pokemonesQueGanan (x:xs) yss = unoSiEsTrue ( superaATodos x yss) +  pokemonesQueGanan xs yss
				

superaATodos :: Pokemon -> [Pokemon] -> Bool
superaATodos _ [] =  True
superaATodos poke (x:xs) = superaA poke x && (superaATodos poke xs)


pokemonesEntrenador :: Entrenador -> [Pokemon]
pokemonesEntrenador (ConsEntrenador n ps) = ps

superaA :: Pokemon -> Pokemon -> Bool
superaA poke1 poke2 = esGanador (tipoPokemon poke1) ( tipoPokemon poke2)
--Dados dos pokémon indica si el primero, en base al tipo, es superior al segundo. Agua
--supera a fuego, fuego a planta y planta a agua. Y cualquier otro caso es falso.
esGanador :: TipoDePokemon -> TipoDePokemon -> Bool
esGanador Agua Fuego = True
esGanador Planta Agua = True
esGanador Fuego Planta = True
esGanador x y = False

--Dado un entrenador, devuelve True si posee al menos un Pokémon de cada tipo posible.
esMaestroPokemon :: Entrenador -> Bool
esMaestroPokemon ent = tieneTipo Agua (pokemonesEntrenador ent) && 
						tieneTipo Fuego (pokemonesEntrenador ent) && 
						tieneTipo Planta (pokemonesEntrenador ent)

tieneTipo :: TipoDePokemon -> [Pokemon] ->Bool
tieneTipo _ [] = False
tieneTipo t (x:xs) = esDeMismoTipo t ( tipoPokemon x) || tieneTipo t xs
 

 
data Seniority = Junior | SemiSenior | Senior 
data Proyecto = ConsProyecto String deriving (Eq, Show)
data Rol = Developer Seniority Proyecto | Management Seniority Proyecto 
data Empresa = ConsEmpresa [Rol] 
	
proy1 = ConsProyecto "Proy1"
proy2 = ConsProyecto "Proy2"
rol1 = Developer Senior	proy1
rol2 = Developer Senior	proy2
emp1 = ConsEmpresa [rol1,rol2]


--Dada una empresa denota la lista de proyectos en los que trabaja, sin elementos repetidos.
proyectos :: Empresa -> [Proyecto]
proyectos emp = proyectosDelRol (rolesDeEmpresa emp)

rolesDeEmpresa :: Empresa -> [Rol]
rolesDeEmpresa (ConsEmpresa xss) = xss

proyectosDelRol :: [Rol] -> [Proyecto]
proyectosDelRol [] = []
proyectosDelRol (x:xs) = if existeProyecto ( obtenerProyecto x ) ( proyectosDelRol xs)
						 then proyectosDelRol xs
					     else obtenerProyecto x : proyectosDelRol xs
	
existeProyecto :: Proyecto -> [Proyecto] -> Bool
existeProyecto _ [] = False
existeProyecto p (x:xs) = if p == x
						  then True
						  else existeProyecto p xs

	
obtenerProyecto :: Rol -> Proyecto
obtenerProyecto (Developer _ p) = p
obtenerProyecto (Management _ p) = p

--Dada una empresa indica la cantidad de desarrolladores senior que posee, que pertecen
--además a los proyectos dados por parámetro.
losDevSenior :: Empresa -> [Proyecto] -> Int
losDevSenior emp xss = pertenecenAUnProyecto ( desarolladoresPorTipo (rolesDeEmpresa emp) Senior) xss 

desarolladoresPorTipo :: [Rol] -> Seniority -> [Rol]
desarolladoresPorTipo [] _ = []
desarolladoresPorTipo (x:xs) s = if esDelMismoRango x s
								 then x : desarolladoresPorTipo xs s
								 else desarolladoresPorTipo xs s
								 
esDelMismoRango :: Rol -> Seniority -> Bool
esDelMismoRango (Developer Junior _) Junior = True
esDelMismoRango (Developer SemiSenior _) SemiSenior = True
esDelMismoRango (Developer Senior _) Senior = True
esDelMismoRango _ _ = False

pertenecenAUnProyecto :: [Rol] -> [Proyecto] -> Int
pertenecenAUnProyecto [] _ = 0
pertenecenAUnProyecto (x:xs) yss = if existeProyecto (obtenerProyecto x) yss
								   then 1 + pertenecenAUnProyecto xs yss
								   else pertenecenAUnProyecto xs yss
								   
								 
--Indica la cantidad de empleados que trabajan en alguno de los proyectos dados.
cantQueTrabajanEn :: [Proyecto] -> Empresa -> Int
cantQueTrabajanEn yss emp = pertenecenAUnProyecto (rolesDeEmpresa emp) yss

--Devuelve una lista de pares que representa a los proyectos (sin repetir) junto con su
--cantidad de personas involucradas.
asignadosPorProyecto :: Empresa -> [(Proyecto, Int)]
asignadosPorProyecto emp = involucrados emp ( proyectosDelRol ( rolesDeEmpresa emp ) )

involucrados :: Empresa -> [Proyecto] -> [(Proyecto, Int)]
involucrados _ []  = []
involucrados emp (x:xs)  = (x, cantQueTrabajanEn [x] emp ) : involucrados emp xs


	
 
