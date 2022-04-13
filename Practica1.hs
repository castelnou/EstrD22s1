-- 1 Numeros enteros. 
-- 1.1 Defina las siguientes funciones:
--a) sucesor :: Int -> Int
-- Dado un número devuelve su sucesor

sucesor :: Int -> Int
sucesor x = x + 1


--b) sumar :: Int -> Int -> Int
-- Dados dos números devuelve su suma utilizando la operación +.

sumar :: Int -> Int -> Int
sumar x y  = x  + y

--c) divisionYResto :: Int -> Int -> (Int, Int)
--Dado dos números, devuelve un par donde la primera componente es la división del
--primero por el segundo, y la segunda componente es el resto de dicha división. Nota:
--para obtener el resto de la división utilizar la función mod :: Int -> Int -> Int,
--provista por Haskell.

divisionYResto :: Int -> Int -> (Int, Int)
divisionYResto x y = ( div x y, mod x y )

--d) maxDelPar :: (Int,Int) -> Int
--Dado un par de números devuelve el mayor de estos.

maxDelPar :: (Int,Int) -> Int
maxDelPar ( x, y ) = max x y


--2. Tipos enumerativos
--1. Definir el tipo de dato Dir, con las alternativas Norte, Sur, Este y Oeste. Luego implementar
--las siguientes funciones:
data Dir = Norte | Sur | Este | Oeste 

--a) Dada una dirección devuelve su opuesta.
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


--c) Dada una dirección devuelve su siguiente, en sentido horario, y suponiendo que no existe
--la siguiente dirección a Oeste. 
-- Precondición: El Oeste no tiene siguiente
-- Parcial ya que no esta definida para el Oeste, y da error.

siguiente :: Dir -> Dir
siguiente Norte = Este
siguiente Este = Sur
siguiente Sur = Oeste


--2. Definir el tipo de dato DiaDeSemana, con las alternativas Lunes, Martes, Miércoles, Jueves,
--Viernes, Sabado y Domingo. Supongamos que el primer día de la semana es lunes, y el último
--es domingo. Luego implementar las siguientes funciones:

data DiaDeSemana = Lunes | Martes | Miercoles | Jueves | Viernes | Sabado | Domingo 

--a) Devuelve un par donde la primera componente es el primer día de la semana, y la
--segunda componente es el último día de la semana.

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

--c) Dado dos dias de semana, indica si el primero viene después que el segundo.

--Como no se puede utilizar el deriving Ord, Se crea una funión que devuelva el indice del día
-- de la semana. De esa manera si puedo comparar con ">"
--Dado un dia de la semana indica que orden tiene, siendo Lunes el primer dia
numeroDelDia :: DiaDeSemana -> Int
numeroDelDia Lunes = 1
numeroDelDia Martes = 2
numeroDelDia Miercoles = 3
numeroDelDia Jueves = 4
numeroDelDia Viernes = 5
numeroDelDia Sabado = 6
numeroDelDia Domingo = 7

vieneDespues ::  DiaDeSemana ->  DiaDeSemana -> Bool
vieneDespues dia1 dia2  = numeroDelDia(dia1) > numeroDelDia(dia2)

--d) Dado un dia de la semana indica si no es ni el primer ni el ultimo dia.
estaEnElMedio :: DiaDeSemana -> Bool
estaEnElMedio Martes 	= True
estaEnElMedio Miercoles = True
estaEnElMedio Jueves 	= True
estaEnElMedio Viernes 	= True
estaEnElMedio Sabado 	= True
estaEnElMedio x			= False

--a) Dado un booleano, si es True devuelve False, y si es False devuelve True.
--En Haskell ya está definida como not.
negar :: Bool -> Bool
negar True = False
negar False = True

--b) Dados dos booleanos, si el primero es True y el segundo es False, devuelve False, sino
--devuelve True.
--Nota: no viene implementada en Haskell.
implica :: Bool -> Bool -> Bool
implica True False = False
implica x y = True

--c) Dados dos booleanos si ambos son True devuelve True, sino devuelve False. Esta
--función debe ser tal que yTambien False (error "Mal") devuelva False.
--En Haskell ya está definida como \&\&.
--yTambien :: Bool -> Bool -> Bool
--yTambien False _ = False
--yTambien _ True = True
yTambien :: Bool -> Bool -> Bool
yTambien True b = b 
yTambien _ _ = False



--d) Dados dos booleanos si alguno de ellos es True devuelve True, sino devuelve False.
--Esta función debe ser tal que oBien True (error "Mal") devuelva True.
--En Haskell ya está deefinida como ||.
-- Lo cambio por la logica vista en clase
oBien :: Bool -> Bool -> Bool
oBien True _ = True
oBien _ b = b
--or True False = True
--or False True = True
--or True True = True
--or x y = False


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
--porcentaje de energía; y Entrenador, como un nombre y dos pokemones. Luego definir las
--siguientes funciones:

data TipoDePokemon = Fuego | Agua | Planta
 
data Pokemon = ConsPoke TipoDePokemon Int 

data Entrenador = ConsEntr String Pokemon Pokemon 

poke1 = ConsPoke Agua 50 
poke2 = ConsPoke Planta 40 
entrenador1 = ConsEntr "ent1" poke1 poke2
entrenador2 = ConsEntr "ent2" poke2 poke2

--Dados dos pokémon indica si el primero, en base al tipo, es superior al segundo. Agua
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


--Devuelve la cantidad de pokémon de determinado tipo que posee el entrenador.
--sumaUnoSiEsMismoTipo :: TipoDePokemon -> TipoDePokemon -> Int
--sumaUnoSiEsMismoTipo Agua Agua = 1
--sumaUnoSiEsMismoTipo Fuego Fuego = 1
--sumaUnoSiEsMismoTipo Planta Planta = 1
--sumaUnoSiEsMismoTipo x y = 0

--Devuelve 1 si es True, sino 0
unoSiEsTrue :: Bool -> Int
unoSiEsTrue True = 1
unoSiEsTrue False = 0

--Devuelve True si dado 2 tipos de pokemon son iguales
esDeMismoTipo :: TipoDePokemon -> TipoDePokemon -> Bool
esDeMismoTipo Agua Agua = True
esDeMismoTipo Fuego Fuego = True
esDeMismoTipo Planta Planta = True
esDeMismoTipo _ _ = False	

--Se reemplaza sumaUnoSiEsMismoTipo por dos funciones con tareas mas especificas
cantidadDePokemonesDe :: TipoDePokemon -> Entrenador -> Int
--cantidadDePokemonesDe t ( ConsEntr n poke1 poke2 ) =    sumaUnoSiEsMismoTipo t ( tipoPokemon poke1 )  +  sumaUnoSiEsMismoTipo t ( tipoPokemon poke2 ) 
cantidadDePokemonesDe t ( ConsEntr n poke1 poke2 ) =    unoSiEsTrue ( esDeMismoTipo  t ( tipoPokemon poke1 )  )
													+  unoSiEsTrue ( esDeMismoTipo  t ( tipoPokemon poke2 ) )

--Dado un par de entrenadores, devuelve a sus pokemones en una lista.
pokemonesEntrenador :: Entrenador -> [Pokemon]
pokemonesEntrenador ( ConsEntr n1 pokeL pokeR ) = [pokeL,pokeR] 

juntarPokemones :: (Entrenador, Entrenador) -> [Pokemon]
juntarPokemones ( entrenadorL, entrenadorR ) = pokemonesEntrenador entrenadorL ++ pokemonesEntrenador entrenadorR


--4. Funciones polimórficas
--1.Defina las siguientes funciones polimórficas: 
--a) Dado un elemento de algún tipo devuelve ese mismo elemento. 
loMismo :: a -> a
loMismo x = x

--b) Dado un elemento de algún tipo devuelve el número 7.
siempreSiete :: a -> Int
siempreSiete x = 7

--c) Dadas una tupla, invierte sus componentes.
swap :: (a,b) -> (b, a)
swap ( x , y ) = ( y , x )


--2. Dada una lista de elementos, si es vacía devuelve True, sino devuelve False.
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
--sinElPrimero [] = []
sinElPrimero [] = error "No hay elementos"
sinElPrimero (x:xs) = xs

--Dada una lista devuelve un par, donde la primera componente es el primer elemento de la
--lista, y la segunda componente es esa lista pero sin el primero.
--No es esta defindo que sucede con el primer elemento si la lista es vacia
splitHead :: [a] -> (a, [a])
splitHead xs = (elPrimero xs, sinElPrimero xs)

elUltimo :: [a] -> a
elUltimo [x]                =  x
elUltimo (_:xs)             =  elUltimo xs
