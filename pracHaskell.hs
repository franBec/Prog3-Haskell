--Practica de Haskell by Pollito

--Al momento de compilar esto, ghci va a llenarse de warnings por los tabs que uso
--pero es que sin tabs el codigo resulta ilegible

--las lineas "nombreDeFuncion :: definicion" no son necesarias para una correcta compilacion
--pero bueno, se ven bonitas y es buena practica de programacion funcional... creo

--EN los ejercicios de folding, acc significa "accumulator", es donde voy
--acumulando la respuesta

--Exitos!

module PracHaskell where --Esta linea no es realmente necesaria

-- ####################################################
-- EJERCICIO 4
-- ####################################################

--EJ 4.1 no recursivo
ej41 :: Num a => [b] -> a
ej41 x = sum(map(const 1)x)

--EJ 4.1 recursivo
ej41r :: Num a => [b] -> a
ej41r [] = 0
ej41r (x:xs) = 1 + ej41r xs

--EJ 4.1 recursivo con acumulador
--long es definida como una funcion local auxiliar
ej41r' :: Num t => [a] -> t
ej41r' x = long x 0 where
	long [] n = n
	long (x:xs) n = long xs (n+1)

--EJ 4.1 con foldr
ej41fr :: (Foldable t, Num a1) => t a2 -> a1
ej41fr lista = foldr(\ _ acc -> (+) 1 acc) 0 lista

--EJ 4.1 con foldl
ej41fl :: (Foldable t, Num a1) => t a2 -> a1
ej41fl lista = foldl(\ acc _ -> (+) 1 acc) 0 lista

-- ####################################################

--EJ 4.2 no recursivo
ej42 :: Eq a => [a] -> a -> Int
ej42 lista x = length(filter(==x)lista)

--EJ 4.2 recursivo
--equals es definida como una funcion local auxiliar
ej42r :: (Num p, Eq t) => [t] -> t -> p
ej42r [] x = 0
ej42r (y:ys) x = equals y x + ej42r ys x where
	equals x y 	|x==y 		= 1
				|otherwise 	= 0

--EJ 4.2 recursivo con acumulador
--flashee un where anidado pensando "nah esto no funciona", pero si funca por lo que:
	--apariciones es definida como una funcion local auxiliar
		--equals es definida como una funcion local a la funcion local apariciones
ej42r' :: (Num t, Eq a) => [a] -> a -> t
ej42r' lista x = apariciones lista x 0 where
	apariciones [] x n = n
	apariciones (y:ys) x n = apariciones (ys) (x) ((equals (y) x) + n) where
		equals x y 	|x==y 		= 1
					|otherwise 	= 0

--EJ 4.2 con foldr
--equals es definida como una funcion local auxiliar
ej42fr :: (Foldable t, Eq a, Num b) => t a -> a -> b
ej42fr lista e = foldr (equals e) 0 lista where
	equals e x acc	|x==e		= acc+1
					|otherwise	= acc

--EJ 4.2 con foldl
----equals es definida como una funcion local auxiliar
ej42fl :: (Foldable t, Eq a, Num b) => t a -> a -> b
ej42fl lista e = foldl (equals e) 0 lista where
	equals e acc x	|x==e		= acc+1
					|otherwise	= acc

-- ####################################################

--EJ 4.3 no recursivo
ej43 :: Eq a => [a] -> a -> Bool
ej43 lista x = length(filter(==x)lista) > 0

--EJ 4.3 recursivo
ej43r :: Eq t => [t] -> t -> Bool
ej43r [] x = False
ej43r (y:ys) x 	|x==y 		= True
				|otherwise 	= ej43r ys x

--EJ 4.3 recursivo con acumulador
--existe es definida como una funcion local auxiliar
ej43r' :: Eq t => [t] -> t -> Bool
ej43r' lista x = existe lista x False where
	existe [] x bool = bool
	existe (y:ys) x bool	|x==y 		= existe [] x True
							|otherwise 	= existe ys x False

--EJ 4.3 con foldr
--existe es definida como una funcion local auxiliar
ej43fr :: (Foldable t, Eq a) => t a -> a -> Bool
ej43fr lista x = foldr (existe x) False lista where
	existe x e acc	|x==e		= acc || True
					|otherwise 	= acc || False

--EJ 4.3 con foldl
--existe es definida como una funcion local auxiliar
ej43fl :: (Foldable t, Eq a) => t a -> a -> Bool
ej43fl lista x = foldl (existe x) False lista where
	existe x acc e	|x==e		= acc || True
					|otherwise 	= acc || False 

-- ####################################################

--EJ 4.4 no recursivo -> [1,2,3,1,2,3]
ej44 :: [a] -> [a]
ej44 x = x++x

--no se como hacerlo recursivo sin acumulador

--EJ 4.4 recursivo con acumulador -> [3,2,1,1,2,3]
--faux es definida como una funcion local auxiliar
ej44r :: [a] -> [a]
ej44r x = faux x x where
	faux x [] = x
	faux x (x':xs') = faux (x':x) (xs')

--EJ 4.4 con foldr -> [1,2,3,3,2,1]
ej44fr :: [a] -> [a]
ej44fr lista = foldr (\x acc -> acc ++ [x]) lista lista

--EJ 4.4 con foldl -> [1,2,3,1,2,3]
ej44fl :: [a] -> [a]
ej44fl lista = foldl (\acc x -> acc ++ [x]) lista lista

-- ####################################################

--EJ 4.5
ej45 :: (Foldable t1, Foldable t2, Num p) => t1 a1 -> t2 a2 -> p
ej45 a b 	|(length a) > (length b)	= 1
			|(length a) < (length b)	= 2
			|otherwise					= 0

--EJ 4.5 recursivo
ej45r :: Num n => [a1] -> [a2] -> n
ej45r [] [] = 0
ej45r a [] = 1
ej45r [] b = 2
ej45r (x:xs) (y:ys) = ej45r xs ys

--Realizar este ejercicio con acumulador no tiene sentido
--pues no hay que hacer ninguna cuenta en la vuelta arriba
--simplemente se comunica un 0, 1, o 2
--como no hay cuenta a realizar, no hay resultado a acumular

--no tengo idea como hacer folding de dos estructuras a la vez

-- ####################################################

--EJ 4.6 recursivo
ej46r :: Num a => [a] -> a
ej46r [] = 0
ej46r [xs] = xs
ej46r (x:xs) = x * ej46r xs

--EJ 4.6 recursivo con acumulador
--mult es definida como una funcion local auxiliar
ej46r' :: Num t => [t] -> t
ej46r' x = mult x 1 where
	mult [] r = 0
	mult [xs] r = r*xs
	mult (x:xs) r = mult xs (r*x)

--EJ 4.6 con foldr
ej46fr :: (Foldable t, Num n) => t n -> n
ej46fr x 	|(length x) == 0	= 	0
			|otherwise 			=	foldr (*) 1 x

--EJ 4.6 con foldl
ej46fl :: (Foldable t, Num n) => t n -> n
ej46fl x 	|(length x) == 0	= 	0
			|otherwise 			=	foldl (*) 1 x

-- ####################################################

--EJ 4.7 recursivo
ej47r :: (Eq a, Num a) => [a] -> a
ej47r [x, y]	|y==1 = x
				|otherwise = x + ej47r [x, (y-1)]

--EJ 4.7 recursivo con acumulador
--multSuc es definida como una funcion local auxiliar
ej47r' :: (Eq t, Num t) => [t] -> t
ej47r' [x, y] = multSuc [x, y] x where
	multSuc [x, 1] n = n
	multSuc [x, y] n = multSuc [x, (y-1)] n+x

--Realizar este ejercicio con folding no le veo mucho sentido
--ya que la estructura a foldear contiene solamente dos elementos
--sin embargo como para hacer algo, voy a crear una estructura aux
--que contenga tantos 'y' como 'x', para luego foldear eso

-- ejemplo de estructura aux a foldear
-- [5,3] = take 5 (repeat 3) = [3,3,3,3,3]

--EJ 4.7 con foldr
ej47fr :: [Int] -> Int
ej47fr [x,y] = foldr (+) 0 (take x (repeat y))

--EJ 4.7 con foldl
ej47fl :: [Int] -> Int
ej47fl [x,y] = foldl (+) 0 (take x (repeat y))

-- ####################################################

--EJ 4.8 recursivo
ej48r :: [a] -> [a]
ej48r [] = []
ej48r (x:xs) = ej48r xs ++ [x]

--EJ 4.8 recursivo con acumulador
--reverseList es definida como una funcion local auxiliar
ej48r' :: [a] -> [a]
ej48r' x = reverseList x [] where
	reverseList [] r = r
	reverseList (x:xs) r = reverseList xs r++[x]

--EJ 4.8 con foldr
ej48fr :: Foldable t => t a -> [a]
ej48fr x = foldr (\x acc -> acc ++ [x]) [] x

--EJ 4.8 con foldl
ej48fl :: [a] -> [a]
ej48fl x = foldl (\acc x -> acc ++ [x]) [] (reverse x)

-- ####################################################

--EJ 4.9

--Ayesa: Te anduvo el programa Franco?
--Franco: Si si ahora lo hice andar
--Ayesa: No Franco yo lo hice andar :) :) :)

--EJ 4.9 no recursivo
ej49 :: [[a]] -> [[a]]
ej49 x = map reverse x

--EJ 4.9 recursivo
ej49r :: [[a]] -> [[a]]
ej49r [] = []
ej49r (x:xs) = (reverse x) : (ej49r xs)

--EJ 4.9 recursivo con acumulador
ej49r' :: [[a]] -> [[a]]
ej49r' x = rev x [] where
	rev [] r = r
	rev (x:xs) r = rev (xs) (r ++ [reverse x])

--EJ 4.9 con foldr
ej49fr :: Foldable t => t [a] -> [[a]]
ej49fr lista = foldr (\x acc -> (reverse x):acc) [] lista

--EJ 4.9 con foldl
ej49fl :: Foldable t => t [a] -> [[a]]
ej49fl lista = foldl (\acc x -> acc ++ [reverse x]) [] lista

-- ####################################################
-- EJERCICIO 5
-- ####################################################

--EJ 5.2 Salida 1 = [1,2,3,1,2,3]

--con acumulador
--addAtEnd es definida como una funcion local auxiliar
ej521 :: [a] -> [a]
ej521 x = addAtEnd x x where
	addAtEnd x [] = x
	addAtEnd x (y:ys) = addAtEnd (x ++ [y]) ys

--con foldr
ej521fr :: [a] -> [a]
ej521fr lista = foldr (\x acc -> acc ++ [x]) lista (reverse lista)

--con foldl
ej521fl :: [a] -> [a]
ej521fl lista = foldl (\acc x -> acc ++ [x]) lista lista

-- ####################################################

--EJ 5.2 Salida 2 = [1,2,3,3,2,1]

--con acumulador
--mirror es definida como una funcion local auxiliar
ej522 :: [a] -> [a]
ej522 x = mirror x x where
	mirror x [] = x
	mirror x x' = mirror (x ++ [last x']) (take (length x' - 1) x')

--con foldr
ej522fr :: [a] -> [a]
ej522fr lista = foldr (\x acc -> acc ++ [x]) lista lista

--con foldl
ej522fl :: [a] -> [a]
ej522fl lista = foldl (\acc x -> acc ++ [x]) lista (reverse lista)

-- ####################################################

--EJ 5.2 Salida 3 = [1,1,2,2,3,3]

--con acumulador
--usa un mergesort que encontre en https://riptutorial.com/haskell/example/7552/merge-sort
ej523 :: Ord a => [a] -> [a]
ej523 x = merge x x where
	merge :: Ord a => [a] -> [a] -> [a]
	merge [] ys = ys
	merge xs [] = xs
	merge (x : xs) (y : ys)	|x <= y = x : merge xs (y:ys)
							|otherwise = y : merge (x:xs) ys

--DATAZO: Si al hacer un folding es importante conservar orden estricto
--(creciente o decreciente), entonces tener como constante una lista vacia

--con foldr
ej523fr :: Foldable t => t a -> [a]
ej523fr lista = foldr (\x acc -> [x,x] ++ acc) [] lista

--con foldl
ej523fl :: Foldable t => t a -> [a]
ej523fl lista = foldl (\acc x -> acc ++ [x,x]) [] lista

-- ####################################################

--EJ 5.3 recursivo
ej53 :: (Eq t, Num t) => t -> [t]
ej53 1 = [2]
ej53 n = ej53 (n-1) ++ [n*2]

--EJ 5.3 recursivo con acumulador
--firstEven es definida como una funcion local auxiliar
ej53' :: (Eq t, Num t) => t -> [t]
ej53' x = firstEven [] x where
	firstEven a 0 = a
	firstEven a x = firstEven ((2*x):a) (x-1)

--EJ 5.3 con foldr
ej53fr :: (Num a, Enum a) => a -> [a]
ej53fr n = foldr (\x acc -> (2*x):acc) [] [1..n]

--EJ 5.3 con foldl
ej53fl :: (Num a, Enum a) => a -> [a]
ej53fl n = foldl (\acc x -> acc ++ [2*x]) [] [1..n]

-- ####################################################

--EJ 5.4 recursivo con acumulador

--tablaMult es definida como una funcion local auxiliar
--recursiva con 3 parametros: [], x, n
ej54 :: (Eq t, Num t) => t -> t -> [t]
ej54 x n = tablaMult [] x n where
	tablaMult lista x 0 = lista
	tablaMult lista x n = tablaMult ((x*n):lista) x (n-1)

--recursiva con 2 parametros: [x], n
ej54' :: Int -> Int -> [Int]
ej54' x n = tablaMult [x] n where
	tablaMult lista 0 = take n lista
	tablaMult lista n = tablaMult (((last lista)*n):lista) (n-1)

--EJ 5.4 con foldr
ej54fr :: (Num a, Enum a) => a -> a -> [a]
ej54fr n1 n2 = foldr faux [] [1..n2] where
	faux x acc = (n1*x):acc

--EJ 5.4 con foldl
ej54fl :: (Num a, Enum a) => a -> a -> [a]
ej54fl n1 n2 = foldl faux [] [1..n2] where
	faux acc x = acc ++ [n1*x]


-- ####################################################
-- COSAS HECHAS POR AYESA EN TEORIA
-- ####################################################

--FACTORIAL SIN ACUMULADOR
factorial :: (Eq p, Num p) => p -> p
factorial 0 = 1
factorial n = n * factorial(n-1)

--FACTORIAL CON ACUMULADOR
-- el usuario llamara factorial n y esta usara una funcion local auxiliar llamada factAcum
factorial' :: (Eq t, Num t) => t -> t
factorial' n = factAcum n 1 where
	factAcum 1 acum = acum
	factAcum n acum = factAcum (n-1) n*acum

-- EJEMPLOS FOLDR

--cuenta:  retorna length l
cuenta :: (Foldable t, Num b) => t p -> b
cuenta l = foldr aux 0 l where
	aux _ xs = (+) 1 xs -- _ es un argumento anonimo que no usare

cuenta' :: (Foldable t, Num a1) => t a2 -> a1
cuenta' l = foldr (\ _ xs -> (+) 1 xs) 0 l

--duplica: [1,2,3] -> [1,1,2,2,3,3]
duplica :: Foldable t => t a -> [a]
duplica l = foldr (\ x xs -> [x,x] ++ xs) [] l

--duplica': [1,2,3] -> [1,2,3,3,2,1]
duplica' :: Foldable t => t a -> [a]
duplica' l = foldr (\ x xs -> x:xs ++ [x]) [] l

--invertir una lista
reverseF l = foldr faux [] l where
	faux e lista = lista ++ [e]

--invertir lista de lista
foldinvsublis :: Foldable t => t [a] -> [[a]]
foldinvsublis li = foldr (\ x xs -> (reverse x) : xs) [] li

--take n elementos de una lista
--con foldr
mitakefr :: Int -> [a] -> [a]
mitakefr n lista = foldr (aux n) [] (reverse lista) where
	aux n e acum	|(length acum)==n 	= acum
					|otherwise			= acum++[e]

--con foldl
mitakefl :: Foldable t => Int -> t a -> [a]
mitakefl n lista = foldl (aux n) [] lista where
	aux n acum e	|(length acum)==n	= acum
					|otherwise			= acum++[e]


-- ####################################################
-- COSAS RANDOM QUE SE ME OCURREN Y QUIERO VER SI FUNCIONA
-- ####################################################

--suma currificada
sumaC = \x -> (\y -> x+y)

-- suma con foldl
sumafl xs = foldl (\x y -> x+y) 0 xs

-- ####################################################
-- EJERCICIO SORPRESA DE AYESA
-- ####################################################

-- interc 1 2 [2,3,1,1,4] = [2,3,2,2,4]

--con foldlr
intercfr :: (Foldable t, Eq a) => a -> a -> t a -> [a]
intercfr e1 e2 lista = foldr faux [] lista where
	faux elemento acc	|e1==elemento 	= e2:acc
						|otherwise		= elemento:acc

--con foldl
intercfl :: Eq a => a -> a -> [a] -> [a]
intercfl e1 e2 lista = foldl faux [] (reverse lista) where
	faux acc elemento	|e1==elemento 	= e2:acc
						|otherwise		= elemento:acc
-- otro con foldl
intercfl' :: (Foldable t, Eq a) => a -> a -> t a -> [a]
intercfl' e1 e2 lista = foldl faux [] lista where
	faux acc elemento	|e1==elemento 	= acc++[e2]
						|otherwise		= acc++[elemento]