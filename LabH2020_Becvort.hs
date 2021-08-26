----------------------------------------------------------------
-- Module : LabH2020_Becvort
-- Developer : Becvort Franco
--
-- Programacion III - Laboratorio Haskell 2020
----------------------------------------------------------------

module LabH2020_Becvort where

-- ####################################################

--Ejercicio 1

	--Recursivo
	mapR :: (t -> a) -> [t] -> [a]
	mapR _ [] = []
	mapR f (x:xs) = (f x):(mapR f xs)

	--Recursivo con acumulador
	mapRA :: (t -> a) -> [t] -> [a]
	mapRA f (x:xs) = faux f (x:xs) [] where
		faux _ [] acc = acc
		faux f (x:xs) acc = faux f xs (acc++[f x])

	--Foldl
	mapfl :: Foldable t1 => (t2 -> a) -> t1 t2 -> [a]
	mapfl f lista = foldl (\acc x -> acc++[f x]) [] lista

	--Foldr
	mapfr :: Foldable t1 => (t2 -> a) -> t1 t2 -> [a]
	mapfr f lista = foldr (\x acc -> (f x):acc) [] lista

-- ####################################################

--Ejercicio 2

	--Recursivo
	filterR :: (a -> Bool) -> [a] -> [a]
	filterR _ [] = []
	filterR cond (x:xs)	|cond x		= x : filterR cond xs
						|otherwise	= filterR cond xs

	--Recursivo con acumulador
	filterRA :: (a -> Bool) -> [a] -> [a]
	filterRA cond (x:xs) = faux cond (x:xs) [] where
		faux _ [] acc = acc
		faux cond (x:xs) acc	|cond x 	= faux cond xs (acc++[x])
								|otherwise 	= faux cond xs acc

	--Foldl
	filterfl :: Foldable t => (a -> Bool) -> t a -> [a]
	filterfl cond lista = foldl (\acc x -> if cond x then acc++[x] else acc) [] lista

	--Foldr
	filterfr :: Foldable t => (a -> Bool) -> t a -> [a]
	filterfr cond lista = foldr (\x acc -> if cond x then x:acc else acc) [] lista

-- ####################################################

--Ejercicio 3

	--Recursivo
	dropR :: (Eq t, Num t) => t -> [a] -> [a]
	dropR _ [] = []
	dropR 0 lista = lista
	dropR n (x:xs) = dropR (n-1) xs

	--Recursivo con acumulador
	dropRA :: (Eq t, Num t) => t -> [a] -> [a]
	dropRA n lista = faux n lista lista where
		faux _ [] _ = []
		faux 0 lista acc = acc
		faux n (x:xs) acc = faux (n-1) xs xs

	--Foldl
	dropfl :: Int -> [a] -> [a]
	dropfl n lista = foldl (\acc x -> if length acc == length lista - n then acc else x:acc) [] (reverse lista)

	--Foldr
	dropfr :: Foldable t => Int -> t a -> [a]
	dropfr n lista = foldr (\x acc -> if length acc == length lista - n then acc else x:acc) [] lista

-- ####################################################

--Ejercicio 4

	----------------------------------------------------------------
	-- MODELO SISTEMA DE STOCK DE ALMACEN
	-----------------------------------------------------------------
	type Cod_Item = Int -- Codigo Interno del producto
	type Item = String -- Descripcion del producto
	type Marca = String -- Marca
	type Rubro = String -- Rubro
	type Cod_Proveedor = Int -- Codigo Interno del proveedor
	type U_Med = String -- Unidad de Medida: 1LT,800GRM, 1500CM3, etc
	type Cant_Existente = Int -- cantidad de productos en deposito (E)
	type V_Min = Int -- valor en existencia recomendado para reposicion (EMin)
	type V_Max = Int -- valor maximo de acopio en deposito (EMax)
	type Precio_U = Float -- precio o valor de compra unitario
	type P_Ganancia = Int -- Porcentaje de ganancia sobre el precio de compra
	type Nombre = String
	type Direccion = String
	type Telefono = String
	-- tupla con datos de 1 item de Stock
	type Item_Stock = (
	 Cod_Item,
	 Item,
	 Marca,
	 Rubro,
	 Cod_Proveedor,
	 U_Med,
	 Cant_Existente,
	 V_Min,
	 V_Max,
	 Precio_U,
	 P_Ganancia
	 )
	-- tupla con datos de 1 proveedor
	type Proveedor = (
	 Cod_Proveedor,
	 Nombre,
	 Direccion,
	 Telefono
	 )
	-- Tablas BD
	type T_Stock = [Item_Stock] --Tabla con el Stock de un comercio
	type T_Proveedor = [ Proveedor] --Tabla con los proveedores de un comercio
	--funciones de extracción
	codItem (cod,item,marca,rubro,prov,umed,cant,cmin,cmax,preciou,pgan)= cod
	item (cod,item,marca,rubro,prov,umed,cant,cmin,cmax,preciou,pgan)= item
	-- y así siguiendo……(completar el codigo si es necesario)
	precioU (cod,item,marca,rubro,prov,umed,cant,cmin,cmax,preciou,pgan)= preciou
	-- y así siguiendo……(completar el codigo si es necesario)
	pganancia (cod,item,marca,rubro,prov,umed,cant,cmin,cmax,preciou,pgan)= pgan
	-- datos predefinidos (Ejemplo)
	tabla1S:: T_Stock
	tabla1S= [
	 (100,"ARROZ GRANO GRANDE","CONDOR","Alimentos",20,"1LT",8000,500,10000,20,30),
	 (107,"ARROZ GRANO GRANDE","GALLO","Alimentos",20,"1KG",6000,200,8000,25,30),
	 (200,"ACEITE DE GIRASOL","NATURA","Alimentos",20,"1LT",9800,600,10000,40,30),
	 (200,"ACEITE DE GIRASOL","COCINERO","Alimentos",20,"1LT",900,500,10000,30,30),
	 (410,"AGUA MINERAL S/GAS BAJO SODIO","SER","Alimentos",31,"1.5LT",20,50,3000,10,35),
	 (412,"AGUA SABORIZADA LIMA LIMON","SER","Alimentos",31,"2LT",1570,50,3000,15, 35),
	 (478,"ALFAJOR CHOCOLATE TITA","TERRABUSI","Alimentos",31,"36GR",900,200,5000,4, 30),
	 (479,"ALFAJOR CHOCOLATE RODESIA","TERRABUSI","Alimentos",31,"40GR",9,200,3500, 4,30),
	 (708,"LECHE DESC. PASTEURIZADA","SERENISIMA","Alimentos",31,"1TL",230,100,1200,20,30),
	 (767,"ARVEJAS SECAS REMOJADAS", "NOEL","Alimentos",20,"300GR",1203,500,3000,10,30),
	 (801,"ANTITRANSPIRANTE ROLL ON","ETIQUET","PERFUMERIA",20,"60gr",30,45,2000,25,30) ]
	tabla1P :: T_Proveedor
	tabla1P= [ (20,"Juan Perez","Belgrano 1827, San Luis, 5700, Argentina","2664-786543"),
	 (31,"Jose Lopez","Junin 444, Mendoza,5500, Argentina","261-3452677")]

-- ANALISIS DE LAS EXPRESIONES

-- map item tabla1S retorna todos los string item de la tabla
-- filter ((==200).codItem)tabla1S retorna todos Item_Stock de la tabla cuyo codItem sea igual a 200

-- Funcion solicitada
	increPU tStock porcentaje = map (((1+porcentaje/100)*).precioU) tStock