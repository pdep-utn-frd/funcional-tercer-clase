--
-- Clase 3
-- 

-- data Disco = DiscoRigido {rpm :: Float, almacenamiento :: Float} | SSD {almacenamiento :: Float}
data Computadora = UnaComputadora {
    cantidadDeRam :: Float, -- medido en gigabytes
    procesador :: String,
    cantidadDeDisco :: Float -- medido en gigabytes
} deriving (Show, Eq, Ord)

miPC :: Computadora
miPC = UnaComputadora {cantidadDeRam = 1.0, procesador = "Intel", cantidadDeDisco = 10.0}

otraPC = UnaComputadora {cantidadDeRam = 8.0, procesador = "Ryzen 5 1600", cantidadDeDisco = 1024}

puntuarCPU :: String -> Float
puntuarCPU "Ryzen 5 1600" = 0
puntuarCPU "Intel" = 5800

puntuarPC1 :: Computadora -> Float
puntuarPC1 (UnaComputadora ram cpu cantDisco) = 1.5 * ram + fromIntegral (length cpu) + cantDisco

puntuarPC2 :: Computadora -> Float
puntuarPC2 (UnaComputadora _ cpu cantDisco) = puntuarCPU cpu + cantDisco * 2

esMejorPC :: (Computadora -> Float) -> Computadora -> Computadora -> Computadora
esMejorPC criterio compu1 compu2
    | criterio compu1 > criterio compu2 = compu1
    | criterio compu1 == criterio compu2 = compu1
    | otherwise = compu2

-- esMejorPCIntel :: Computadora -> Computadora -> Computadora
-- esMejorPCIntel compu1 compu2
--     | puntuarPC2 compu1 > puntuarPC2 compu2 = compu1
--     | puntuarPC2 compu1 == puntuarPC2 compu2 = compu1
--     | otherwise = compu2

-- Funciones de orden superior
-- 
listaDeNombres :: [String]
listaDeNombres = ["Hannah", "Ivan", "Bernardo", "Lucas" ]

tieneMasDe5Letras :: String -> Bool
tieneMasDe5Letras palabra = length palabra > 5


agregarMemoria :: Float -> Computadora -> Computadora
agregarMemoria cantidad (UnaComputadora ram cpu disco) = (UnaComputadora (ram + cantidad) cpu disco)


miMap :: (a -> b) -> [a] -> [b]
miMap _ [] = []
miMap funcion (cabeza:cola) = (funcion cabeza : miMap funcion cola) 

miFilter :: (a -> Bool) -> [a] -> [a]
miFilter condicion [] = []
miFilter condicion (x:xs)
    | condicion x = (x : miFilter condicion xs)
    | otherwise = miFilter condicion xs

agregarResultadosCon operacionLogica valorInicial condicion [] = valorInicial
agregarResultadosCon operacionLogica valorInicial condicion (x:xs) 
    = operacionLogica (condicion x) (agregarResultadosCon operacionLogica valorInicial condicion xs)


miAll2 condicion lista = agregarResultadosCon (&&) True condicion lista

miAll :: (a -> Bool) -> [a] -> Bool
miAll condicion [] = True
miAll condicion (x:xs) = condicion x && miAll condicion xs

miAny2 condicion lista = agregarResultadosCon (||) False condicion lista

miAny :: (a -> Bool) -> [a] -> Bool
miAny _ [] = False
miAny condicion (x:xs) = condicion x || miAny condicion xs

identidad x = x

sumatoria :: Num t => [t] -> t
sumatoria lista = agregarResultadosCon (+) 0 identidad lista

productoria lista = agregarResultadosCon (*) 1 identidad lista