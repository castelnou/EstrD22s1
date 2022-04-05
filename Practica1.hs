-- 1 Numeros enteros. 
-- 1.1 Defina las siguientes funciones:
--a) sucesor :: Int -> Int
-- Dado un n�mero devuelve su sucesor

sucesor :: Int -> Int
sucesor x = x + 1


--b) sumar :: Int -> Int -> Int
-- Dados dos n�meros devuelve su suma utilizando la operaci�n +.

sumar :: Int -> Int -> Int
sumar x y  = x  + y

--c) divisionYResto :: Int -> Int -> (Int, Int)
--Dado dos n�meros, devuelve un par donde la primera componente es la divisi�n del
--primero por el segundo, y la segunda componente es el resto de dicha divisi�n. Nota:
--para obtener el resto de la divisi�n utilizar la funci�n mod :: Int -> Int -> Int,
--provista por Haskell.

divisionYResto :: Int -> Int -> (Int, Int)
divisionYResto x y = ( div x y, mod x y )

--d) maxDelPar :: (Int,Int) -> Int
--Dado un par de n�meros devuelve el mayor de estos.

maxDelPar :: (Int,Int) -> Int
maxDelPar ( x, y ) = max x y


--2. Tipos enumerativos
--1. Definir el tipo de dato Dir, con las alternativas Norte, Sur, Este y Oeste. Luego implementar
--las siguientes funciones:
data Dir = Norte | Sur | Este | Oeste 

--a) Dada una direcci�n devuelve su opuesta.
opuesto :: Dir -> Dir
opuesto Norte = Sur
opuesto Sur   = Norte
opuesto Este  = Oeste
opuesto Oeste = Este

--b) Dadas dos direcciones, indica si son la misma. Nota: utilizar pattern matching y no ==.
iguales :: Dir -> Dir -> Bool
iguales Sur Sur = True
iguales Norte Norte = True
iguales Este Este = True
iguales Oeste Oeste = True
iguales x y = False


--c) Dada una direcci�n devuelve su siguiente, en sentido horario, y suponiendo que no existe
--la siguiente direcci�n a Oeste. 
-- Precondici�n: El Oeste no tiene siguiente
-- Parcial ya que no esta definida para el Oeste, y da error.

siguiente :: Dir -> Dir
siguiente Norte = Este
siguiente Este = Sur
siguiente Sur = Oeste


--2. Definir el tipo de dato DiaDeSemana, con las alternativas Lunes, Martes, Mi�rcoles, Jueves,
--Viernes, Sabado y Domingo. Supongamos que el primer d�a de la semana es lunes, y el �ltimo
--es domingo. Luego implementar las siguientes funciones:

data DiaDeSemana = Lunes | Martes | Miercoles | Jueves | Viernes | Sabado | Domingo 

--a) Devuelve un par donde la primera componente es el primer d�a de la semana, y la
--segunda componente es el �ltimo d�a de la semana.

primeroYUltimoDia :: (DiaDeSemana, DiaDeSemana)
primeroYUltimoDia = (Lunes, Domingo)

--b) Dado un dia de la semana indica si comienza con la letra M.
empiezaConM :: DiaDeSemana -> Bool
empiezaConM Lunes = False
empiezaConM Martes = True
empiezaConM Miercoles = True
empiezaConM Jueves = False 
empiezaConM Viernes = False
empiezaConM Sabado = False
empiezaConM Domingo = False

--c) Dado dos dias de semana, indica si el primero viene despu�s que el segundo.
vieneDespues :: DiaDeSemana -> DiaDeSemana -> Bool
vieneDespues Martes Lunes  	  = True
vieneDespues Miercoles Martes  = True
vieneDespues Jueves Miercoles = True
vieneDespues Viernes Jueves = True
vieneDespues Sabado Viernes = True
vieneDespues Domingo Sabado = True
vieneDespues Lunes Domingo = True
vieneDespues x y = False


--d) Dado un dia de la semana indica si no es ni el primer ni el ultimo dia.
estaEnElMedio :: DiaDeSemana -> Bool
estaEnElMedio Martes 	= True
estaEnElMedio Miercoles = True
estaEnElMedio Jueves 	= True
estaEnElMedio Viernes 	= True
estaEnElMedio Sabado 	= True
estaEnElMedio x			= False

--a) Dado un booleano, si es True devuelve False, y si es False devuelve True.
--En Haskell ya est� definida como not.
negar :: Bool -> Bool
negar True = False
negar False = True

--b) Dados dos booleanos, si el primero es True y el segundo es False, devuelve False, sino
--devuelve True.
--Nota: no viene implementada en Haskell.
implica :: Bool -> Bool -> Bool
implica True False = False
implica x y = True

--c) Dados dos booleanos si ambos son True devuelve True, sino devuelve False.
--En Haskell ya est� definida como &&.

and :: Bool -> Bool -> Bool
and True True = True
and x y = False

--d) Dados dos booleanos si alguno de ellos es True devuelve True, sino devuelve False.
-- En Haskell ya est� definida como ||.
or :: Bool -> Bool -> Bool
or True False = True
or False True = True
or True True = True
or x y = False


--3. Registros
--3.1 Personas
--Definir el tipo de dato Persona, como un nombre y la edad de la persona. Realizar las
--siguientes funciones:
data Persona = ConsP String Int 

persona1 = ConsP "Juan" 20
persona2 = ConsP "Carlos" 15
persona3 = ConsP "Pepe" 55

--Devuelve el nombre de una persona
nombre :: Persona -> String
nombre ( ConsP n e ) = n

--Devuelve la edad de una persona
edad :: Persona -> Int
edad ( ConsP n e ) = e 

--Aumenta en uno la edad de la persona.
crecer :: Persona -> Persona
crecer p = ConsP ( nombre p ) ( edad p + 1 )
--crecer ( ConsP n e ) = ConsP n ( e + 1 ) ESTO ESTA MAL


--Dados un nombre y una persona, devuelve una persona con la edad de la persona y el
--nuevo nombre.
cambioDeNombre :: String -> Persona -> Persona
cambioDeNombre newName p = ConsP newName ( edad p )
--cambioDeNombre newName ( ConsP n e ) = ConsP newName e ESTO ESTA MAL

--Dadas dos personas indica si la primera es mayor que la segunda.
esMayorQueLaOtra :: Persona -> Persona -> Bool
esMayorQueLaOtra p1 p2 = edad p1 > edad p2
--esMayorQueLaOtra ( ConsP nl el ) ( ConsP nr er ) = el > er ESTO ESTA MAL
	
		
--Dadas dos personas devuelve a la persona que sea mayor.
laQueEsMayor :: Persona -> Persona -> Persona
laQueEsMayor personaL personaR = 
	if esMayorQueLaOtra personaL personaR
		then personaL
		else personaR



-- 3.2 Pokemones
--Definir los tipos de datos P okemon, como un T ipoDeP okemon (agua, fuego o planta) y un
--porcentaje de energ�a; y Entrenador, como un nombre y dos pokemones. Luego definir las
--siguientes funciones:

data TipoDePokemon = Fuego | Agua | Planta
 
data Pokemon = ConsPoke TipoDePokemon Int 

data Entrenador = ConsEntr String Pokemon Pokemon 

poke1 = ConsPoke Agua 50 
poke2 = ConsPoke Planta 40 
entrenador1 = ConsEntr "ent1" poke1 poke2
entrenador2 = ConsEntr "ent2" poke2 poke2

--Dados dos pok�mon indica si el primero, en base al tipo, es superior al segundo. Agua
--supera a fuego, fuego a planta y planta a agua. Y cualquier otro caso es falso.
esGanador :: TipoDePokemon -> TipoDePokemon -> Bool
esGanador Agua Fuego = True
esGanador Planta Agua = True
esGanador Fuego Planta = True
esGanador x y = False

--Auxiliar para obtener el tipo de pokemon dado un pokemon
tipoPokemon :: Pokemon -> TipoDePokemon
tipoPokemon ( ConsPoke t e ) = t

superaA :: Pokemon -> Pokemon -> Bool
superaA poke1 poke2 = esGanador (tipoPokemon poke1) ( tipoPokemon poke2)
--superaA ( ConsPoke n p a t ) ( ConsPoke n2 p2 a2 t2 ) = esGanador t t2 ESTO ESTA MAL


--Devuelve la cantidad de pok�mon de determinado tipo que posee el entrenador.
sumaUnoSiEsMismoTipo :: TipoDePokemon -> TipoDePokemon -> Int
sumaUnoSiEsMismoTipo Agua Agua = 1
sumaUnoSiEsMismoTipo Fuego Fuego = 1
sumaUnoSiEsMismoTipo Planta Planta = 1
sumaUnoSiEsMismoTipo x y = 0


cantidadDePokemonesDe :: TipoDePokemon -> Entrenador -> Int
cantidadDePokemonesDe t ( ConsEntr n poke1 poke2 ) =    sumaUnoSiEsMismoTipo t ( tipoPokemon poke1 )  +  sumaUnoSiEsMismoTipo t ( tipoPokemon poke2 ) 

--Dado un par de entrenadores, devuelve a sus pokemones en una lista.
pokemonesEntrenador :: Entrenador -> [Pokemon]
pokemonesEntrenador ( ConsEntr n1 pokeL pokeR ) = [pokeL,pokeR] 

juntarPokemones :: (Entrenador, Entrenador) -> [Pokemon]
juntarPokemones ( entrenadorL, entrenadorR ) = pokemonesEntrenador entrenadorL ++ pokemonesEntrenador entrenadorR


--4. Funciones polim�rficas
--1.Defina las siguientes funciones polim�rficas: 
--a) Dado un elemento de alg�n tipo devuelve ese mismo elemento. 
loMismo :: a -> a
loMismo x = x

--b) Dado un elemento de alg�n tipo devuelve el n�mero 7.
siempreSiete :: a -> Int
siempreSiete x = 7

--c) Dadas una tupla, invierte sus componentes.
swap :: (a,b) -> (b, a)
swap ( x , y ) = ( y , x )


--2. Dada una lista de elementos, si es vac�a devuelve True, sino devuelve False.
-- Definida en Haskell como null.
estaVacia :: [a] -> Bool
estaVacia [] = True
estaVacia [x] = False

--3. Dada una lista devuelve su primer elemento.
--Definida en Haskell como head.
--Nota: tener en cuenta que el constructor de listas es :
elPrimero :: [a] -> a
elPrimero (x:_) =  x

--Dada una lista devuelve esa lista menos el primer elemento.
sinElPrimero :: [a] -> [a]
sinElPrimero [] = []
sinElPrimero (x:xs) = xs

--Dada una lista devuelve un par, donde la primera componente es el primer elemento de la
--lista, y la segunda componente es esa lista pero sin el primero.
--No es esta defindo que sucede con el primer elemento si la lista es vacia
splitHead :: [a] -> (a, [a])
splitHead xs = (elPrimero xs, sinElPrimero xs)

elUltimo :: [a] -> a
elUltimo [x]                =  x
elUltimo (_:xs)             =  elUltimo xs
















