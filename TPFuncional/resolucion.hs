module Ejemplo where
import Text.Show.Functions()

---------PUNTO 1----------

type Ruedas = Float
type Chasis = Float

data Auto = UnAuto { 
                    marca :: String,
                    modelo :: String,
                    desgaste :: (Ruedas , Chasis),
                    velocidadMaxima :: Float,
                    tiempoDeCarrera :: Float,
                    apodos :: [String]
                } deriving (Show, Eq)

ferrari :: Auto
ferrari = UnAuto "Ferrari" "F50" (0 , 0) 65 0 ["La nave", "El fierro", "Ferrucho"]

lamborghini :: Auto
lamborghini = UnAuto "Lamborghini" "Diablo" (4 , 7) 73 0 ["Lambo", "La bestia"]

fiat :: Auto
fiat = UnAuto "Fiat" "600" (27 , 33) 44 0 ["La Bocha","La bolita", "Fitito"]

peugeot :: Auto
peugeot = UnAuto "Peugeot" "504" (0 , 0) 40 0 ["El rey del desierto"]

----------PUNTO 2 ----------
---a---

desgasteChasis :: Auto -> Float
desgasteChasis unAuto = snd . desgaste $ unAuto

desgasteRuedas :: Auto -> Float
desgasteRuedas unAuto = fst . desgaste $ unAuto

chasisEstaMasDesgastadoQue :: Float -> Auto -> Bool
chasisEstaMasDesgastadoQue unNumero unAuto = desgasteChasis unAuto > unNumero

ruedasEstanMasDesgastadasQue :: Float -> Auto -> Bool
ruedasEstanMasDesgastadasQue unNumero unAuto = desgasteRuedas unAuto > unNumero

chasisEstaMenosDesgastadoQue :: Float -> Auto -> Bool
chasisEstaMenosDesgastadoQue unNumero unAuto = desgasteChasis unAuto < unNumero

ruedasEstanMenosDesgastadasQue :: Float -> Auto -> Bool
ruedasEstanMenosDesgastadasQue unNumero unAuto = desgasteRuedas unAuto < unNumero

chasisNoEstaDesgastado :: Auto -> Bool
chasisNoEstaDesgastado unAuto = desgasteChasis unAuto == 0

ruedasNoEstanDesgastadas :: Auto -> Bool
ruedasNoEstanDesgastadas unAuto = desgasteRuedas unAuto == 0

estaComoNuevo :: Auto -> Bool
estaComoNuevo unAuto = chasisNoEstaDesgastado unAuto && ruedasNoEstanDesgastadas unAuto

estaEnBuenEstado :: Auto -> Bool
estaEnBuenEstado (UnAuto "Peugeot" _ _ _ _ _) = False
estaEnBuenEstado unAuto 
  | tiempoDeCarrera unAuto < 100 = chasisEstaMenosDesgastadoQue 20 unAuto
  | otherwise = chasisEstaMenosDesgastadoQue 40 unAuto && ruedasEstanMenosDesgastadasQue 60 unAuto

{-
CASOS DE PRUEBA

*Ejemplo> estaEnBuenEstado (UnAuto "Peugeot" "504" (0 , 0) 40 0 ["El rey del desierto"])
False
---
*Ejemplo> estaEnBuenEstado (UnAuto "Lamborghini" "Diablo" (4 , 7) 73 99 ["Lambo", "La bestia"])
True
---
*Ejemplo> estaEnBuenEstado (UnAuto "Fiat" "600" (27 , 33) 44 99 ["La Bocha","La bolita", "Fitito"])       
False
---
*Ejemplo> estaEnBuenEstado (UnAuto "Ferrari" "F50" (50 , 30) 65 130 ["La nave", "El fierro", "Ferrucho"])
True
---
*Ejemplo> estaEnBuenEstado (UnAuto "Ferrari" "F50" (50 , 45) 65 15 ["La nave", "El fierro", "Ferrucho"])
False
---
*Ejemplo> estaEnBuenEstado (UnAuto "Ferrari" "F50" (70 , 30) 65 150 ["La nave", "El fierro", "Ferrucho"])
False
-}

---b---
tomarLetrasDelApodo :: Int -> Auto -> String
tomarLetrasDelApodo unNumero unAuto = take unNumero ((head.apodos) unAuto)

noDaMas :: Auto -> Bool
noDaMas unAuto = tomarLetrasDelApodo 3 unAuto == "La " &&  chasisEstaMasDesgastadoQue 80 unAuto || ruedasEstanMasDesgastadasQue 80 unAuto 

{-
CASOS DE PRUEBAS

*Ejemplo> noDaMas (UnAuto "Ferrari" "F50" (20 , 90) 65 0 ["La nave", "El fierro", "Ferrucho"])
True
---
*Ejemplo> noDaMas (UnAuto "Ferrari" "F50" (0 , 20) 65 0 ["La nave", "El fierro", "Ferrucho"])
False
---
*Ejemplo> noDaMas (UnAuto "Lamborghini" "Diablo" (90 , 20) 73 0 ["Lambo", "La bestia"])
True
---
*Ejemplo> noDaMas (UnAuto "Lamborghini" "Diablo" (4 , 7) 73 0 ["Lambo", "La bestia"])
False
-}

--- c ---
apodosTotalesDe :: Auto -> Int
apodosTotalesDe unAuto = length . apodos $ unAuto

cantidadParDeApodos :: Auto -> Bool
cantidadParDeApodos unAuto = (even.apodosTotalesDe) unAuto

esUnChiche :: Auto -> Bool
esUnChiche unAuto 
    |cantidadParDeApodos unAuto = chasisEstaMenosDesgastadoQue 20 unAuto
    |otherwise = chasisEstaMenosDesgastadoQue 50 unAuto
{-
CASO DE PRUEBA
*Ejemplo> esUnChiche (UnAuto "Lamborghini" "Diablo" (4 , 7) 73 0 ["Lambo", "La bestia"])
True
---
*Ejemplo> esUnChiche (UnAuto "Lamborghini" "Diablo" (90 , 20) 73 0 ["Lambo", "La bestia"])
False
---
*Ejemplo> esUnChiche (UnAuto "Ferrari" "F50" (20 , 90) 65 0 ["La nave", "El fierro", "Ferrucho"])
False
---
*Ejemplo> esUnChiche (UnAuto "Ferrari" "F50" (0 , 0) 65 0 ["La nave", "El fierro", "Ferrucho"])
True

-}
--- d ---
esUnaJoya :: Auto -> Bool
esUnaJoya unAuto = estaComoNuevo unAuto && apodosTotalesDe unAuto <= 1

{- 
CASOS DE PRUEBAS

*Ejemplo> esUnaJoya (UnAuto "Ferrari" "F50" (0 , 0) 65 0 ["La nave", "El fierro", "Ferrucho"])
False
---
*Ejemplo> esUnaJoya (UnAuto "Peugeot" "504" (0 , 0) 40 0 ["El rey del desierto"])             
True
-}

--- e ---
cantidadDeCaracteresDelModelo :: Auto -> Int
cantidadDeCaracteresDelModelo unAuto = length . modelo $ unAuto

nivelDeChetez :: Auto -> Int
nivelDeChetez unAuto = 20 * apodosTotalesDe unAuto * cantidadDeCaracteresDelModelo unAuto

{-
CASO DE PRUEBA
*Ejemplo> nivelDeChetez (UnAuto "Ferrari" "F50" (0 , 0) 65 0 ["La nave", "El fierro", "Ferrucho"]) 
180
-}

--- f ---
primerApodo :: Auto -> String
primerApodo unAuto = head . apodos $ unAuto

capacidadSupercalifragilisticaespialidosa :: Auto -> Int
capacidadSupercalifragilisticaespialidosa unAuto = length . primerApodo $ unAuto

{-
CASO DE PRUEBA
*Ejemplo> capacidadSupercalifragilisticaespialidosa  (UnAuto "Ferrari" "F50" (0 , 0) 65 0 ["La nave", "El fierro", "Ferrucho"])
7
-}

--- g ---
decimaParteDelDesgasteDeLasRuedas :: Auto -> Float
decimaParteDelDesgasteDeLasRuedas unAuto =  (/10) . desgasteRuedas $ unAuto

cantidadDeRiesgo :: Auto -> Float
cantidadDeRiesgo unAuto
    | not (estaEnBuenEstado unAuto) = decimaParteDelDesgasteDeLasRuedas unAuto * velocidadMaxima unAuto * 2 
    | otherwise = decimaParteDelDesgasteDeLasRuedas unAuto * velocidadMaxima unAuto

{-
CASOS DE PRUEBAS
*Ejemplo> cantidadDeRiesgo (UnAuto "Lamborghini" "Diablo" (4 , 7) 73 0 ["Lambo", "La bestia"])
29.2
---
*Ejemplo> cantidadDeRiesgo (UnAuto "Fiat" "600" (27 , 33) 44 0 ["La Bocha","La bolita", "Fitito"]) 
237.6
-}

----------PUNTO 3----------
---a---
type Taller = Auto -> Auto

repararUnAuto :: Taller
repararUnAuto unAuto = unAuto {desgaste = (0, (desgasteChasis unAuto) * 0.15) }

{-
CASOS DE PRUEBAS 
*Ejemplo> repararUnAuto (UnAuto "Fiat" "600" (27 , 33) 44 0 ["La Bocha","La bolita", "Fitito"])
UnAuto {marca = "Fiat", modelo = "600", desgaste = (0.0,4.9500003), velocidadMaxima = 44.0, tiempoDeCarrera = 0, apodos = ["La Bocha","La bolita","Fitito"]}
---
*Ejemplo> repararUnAuto (UnAuto "Ferrari" "F50" (0 , 0) 65 0 ["La nave", "El fierro", "Ferrucho"])
UnAuto {marca = "Ferrari", modelo = "F50", desgaste = (0.0,0.0), velocidadMaxima = 65.0, tiempoDeCarrera = 0, apodos = ["La nave","El fierro","Ferrucho"]}
-}

---b---
aplicarPenalidad :: Float -> Taller
aplicarPenalidad segundos unAuto = modificarTiempoDeCarrera (+ segundos) unAuto

modificarTiempoDeCarrera :: (Float -> Float)-> Taller
modificarTiempoDeCarrera unaFuncion unAuto = unAuto {tiempoDeCarrera = unaFuncion.tiempoDeCarrera $ unAuto}

{-
CASOS DE PRUEBAS
*Ejemplo> aplicarPenalidad 20 (UnAuto "Ferrari" "F50" (0 , 0) 65 10 ["La nave", "El fierro", "Ferrucho"])
UnAuto {marca = "Ferrari", modelo = "F50", desgaste = (0.0,0.0), velocidadMaxima = 65.0, tiempoDeCarrera = 30, apodos = ["La nave","El fierro","Ferrucho"]}
---
*Ejemplo> aplicarPenalidad 0 (UnAuto "Ferrari" "F50" (0 , 0) 65 10 ["La nave", "El fierro", "Ferrucho"])  
UnAuto {marca = "Ferrari", modelo = "F50", desgaste = (0.0,0.0), velocidadMaxima = 65.0, tiempoDeCarrera = 10, apodos = ["La nave","El fierro","Ferrucho"]}
-}

---c---
ponerNitro :: Taller
ponerNitro unAuto = unAuto {velocidadMaxima = (velocidadMaxima unAuto) * 1.2}

{-
CASOS DE PRUEBAS
*Ejemplo> ponerNitro (UnAuto "Fiat" "600" (27 , 33) 44 0 ["La Bocha","La bolita", "Fitito"])
UnAuto {marca = "Fiat", modelo = "600", desgaste = (27.0,33.0), velocidadMaxima = 52.800003, tiempoDeCarrera = 0, apodos = ["La Bocha","La bolita","Fitito"]}
---
*Ejemplo> ponerNitro (UnAuto "Fiat" "600" (27 , 33) 0 0 ["La Bocha","La bolita", "Fitito"]) 
UnAuto {marca = "Fiat", modelo = "600", desgaste = (27.0,33.0), velocidadMaxima = 0.0, tiempoDeCarrera = 0, apodos = ["La Bocha","La bolita","Fitito"]}
-}

---d---
bautizar :: String -> Taller
bautizar nuevoApodo unAuto = unAuto {apodos = nuevoApodo : apodos unAuto}

{-
CASOS DE PRUEBAS
*Ejemplo> bautizar "El Diablo" (UnAuto "Lamborghini" "Diablo" (4 , 7) 73 0 ["Lambo", "La bestia"])
UnAuto {marca = "Lamborghini", modelo = "Diablo", desgaste = (4.0,7.0), velocidadMaxima = 73.0, tiempoDeCarrera = 0, apodos = ["El Diablo","Lambo","La bestia"]}
---
*Ejemplo> bautizar "El Diablo" (UnAuto "Lamborghini" "Diablo" (4 , 7) 73 0 [])                    
UnAuto {marca = "Lamborghini", modelo = "Diablo", desgaste = (4.0,7.0), velocidadMaxima = 73.0, tiempoDeCarrera = 0, apodos = ["El Diablo"]}
-}

---e---
llevarUnAutoADesarmar :: String -> String -> Taller
llevarUnAutoADesarmar nuevaMarca nuevoModelo unAuto = unAuto {marca = nuevaMarca, modelo = nuevoModelo, apodos = ["Nunca taxi"]}

{-
CASOS DE PRUEBAS
*Ejemplo> llevarUnAutoADesarmar "Tesla" "X" (UnAuto "Fiat" "600" (27 , 33) 44 0 ["La Bocha","La bolita", "Fitito"])
UnAuto {marca = "Tesla", modelo = "X", desgaste = (27.0,33.0), velocidadMaxima = 44.0, tiempoDeCarrera = 0, apodos = ["Nunca taxi"]}
-}

----------PUNTO 4----------
---a---
type Tramo = Auto -> Auto

data Pista = UnaPista {
        nombre :: String,
        pais :: String,
        precioBase :: Float,
        tramos :: [Tramo]
    } deriving (Show)

desgastarRuedas :: Float -> Tramo
desgastarRuedas desgasteAgregado unAuto = modificarDesgasteRuedas (+ desgasteAgregado) unAuto

desgastarChasis :: Float -> Tramo
desgastarChasis desgasteAgregado unAuto = modificarDesgasteChasis (+ desgasteAgregado) unAuto

modificarDesgasteRuedas :: (Float -> Float) -> Tramo
modificarDesgasteRuedas unaFuncion unAuto = unAuto {desgaste = (unaFuncion . desgasteRuedas $ unAuto, desgasteChasis unAuto)}

modificarDesgasteChasis :: (Float -> Float) -> Tramo
modificarDesgasteChasis unaFuncion unAuto = unAuto {desgaste = (desgasteRuedas unAuto, unaFuncion . desgasteChasis $ unAuto)}

aumentarTiempoDeCarrera :: Float -> Tramo --OJO, esta funcion es identica a la de aplicarPenalidad
aumentarTiempoDeCarrera tiempoAgregado unAuto = modificarTiempoDeCarrera (+ tiempoAgregado) unAuto

desgasteQueGeneraLaCurva :: Float -> Float -> Tramo
desgasteQueGeneraLaCurva longitud angulo unAuto = desgastarRuedas (3 * longitud / angulo) unAuto

aumentaTiempoDeCarreraEnLaCurva ::  Float -> Tramo
aumentaTiempoDeCarreraEnLaCurva longitud unAuto = aumentarTiempoDeCarrera (longitud / (velocidadMaxima unAuto / 2)) unAuto

curva :: Float -> Float -> Tramo  
curva angulo longitud unAuto = (desgasteQueGeneraLaCurva longitud angulo) . (aumentaTiempoDeCarreraEnLaCurva longitud) $ unAuto

curvaPeligrosa :: Tramo
curvaPeligrosa unAuto = curva 60 300 unAuto

curvaTranca :: Tramo
curvaTranca unAuto = curva 110 550 unAuto

{-
CASOS DE PRUEBAS 

*Ejemplo> curvaPeligrosa (UnAuto "Ferrari" "F50" (0 , 0) 65 0 ["La nave", "El fierro", "Ferrucho"])
UnAuto {marca = "Ferrari", modelo = "F50", desgaste = (15.0,0.0), velocidadMaxima = 65.0, tiempoDeCarrera = 9.230769, apodos = ["La nave","El fierro","Ferrucho"]}
---
*Ejemplo> curvaPeligrosa (UnAuto "Peugeot" "504" (0 , 0) 40 0 ["El rey del desierto"])
UnAuto {marca = "Peugeot", modelo = "504", desgaste = (15.0,0.0), velocidadMaxima = 40.0, tiempoDeCarrera = 15.0, apodos = ["El rey del desierto"]}
---
*Ejemplo> curvaTranca (UnAuto "Ferrari" "F50" (0 , 0) 65 0 ["La nave", "El fierro", "Ferrucho"])   
UnAuto {marca = "Ferrari", modelo = "F50", desgaste = (15.0,0.0), velocidadMaxima = 65.0, tiempoDeCarrera = 16.923077, apodos = ["La nave","El fierro","Ferrucho"]}
---
*Ejemplo> curvaTranca (UnAuto "Peugeot" "504" (0 , 0) 40 0 ["El rey del desierto"])   
UnAuto {marca = "Peugeot", modelo = "504", desgaste = (15.0,0.0), velocidadMaxima = 40.0, tiempoDeCarrera = 27.5, apodos = ["El rey del desierto"]}
-}

---b---
desgasteQueGeneraLaRecta :: Float -> Tramo
desgasteQueGeneraLaRecta longitud unAuto = desgastarChasis (longitud / 100) unAuto

aumentaTiempoDeCarreraEnLaRecta ::  Float -> Tramo
aumentaTiempoDeCarreraEnLaRecta longitud unAuto = aumentarTiempoDeCarrera (longitud / velocidadMaxima unAuto) unAuto

recto :: Float -> Tramo
recto longitud unAuto = (desgasteQueGeneraLaRecta longitud) . (aumentaTiempoDeCarreraEnLaRecta longitud) $ unAuto

tramoRectoClassic :: Tramo
tramoRectoClassic unAuto = recto 715 unAuto

tramito :: Tramo
tramito unAuto = recto 260 unAuto

{-
CASOS DE PRUEBAS 

*Ejemplo> tramoRectoClassic (UnAuto "Ferrari" "F50" (0 , 0) 65 0 ["La nave", "El fierro", "Ferrucho"])
UnAuto {marca = "Ferrari", modelo = "F50", desgaste = (0.0,7.15), velocidadMaxima = 65.0, tiempoDeCarrera = 11.0, apodos = ["La nave","El fierro","Ferrucho"]}
---
*Ejemplo> tramito (UnAuto "Ferrari" "F50" (0 , 0) 65 0 ["La nave", "El fierro", "Ferrucho"])          
UnAuto {marca = "Ferrari", modelo = "F50", desgaste = (0.0,2.6), velocidadMaxima = 65.0, tiempoDeCarrera = 4.0, apodos = ["La nave","El fierro","Ferrucho"]}
-}

---c---
desgasteQueGeneraElZigZag :: Float -> Tramo
desgasteQueGeneraElZigZag cambiosDeDireccion unAuto = desgastarChasis 5 . desgastarRuedas (velocidadMaxima unAuto * cambiosDeDireccion / 10) $ unAuto

aumentaTiempoDeCarreraEnElZigZag ::  Float -> Tramo
aumentaTiempoDeCarreraEnElZigZag cambiosDeDireccion unAuto = aumentarTiempoDeCarrera (cambiosDeDireccion * 3) unAuto

zigzag :: Float -> Tramo
zigzag cambiosDeDireccion unAuto = (aumentaTiempoDeCarreraEnElZigZag cambiosDeDireccion) . (desgasteQueGeneraElZigZag cambiosDeDireccion) $ unAuto

zigZagLoco :: Tramo
zigZagLoco unAuto = zigzag 5 unAuto

casiCurva :: Tramo
casiCurva unAuto = zigzag 1 unAuto

{-
CASOS DE PRUEBAS 

*Ejemplo> zigZagLoco (UnAuto "Ferrari" "F50" (0 , 0) 65 0 ["La nave", "El fierro", "Ferrucho"])
UnAuto {marca = "Ferrari", modelo = "F50", desgaste = (32.5,5.0), velocidadMaxima = 65.0, tiempoDeCarrera = 15.0, apodos = ["La nave","El fierro","Ferrucho"]}
---
*Ejemplo> casiCurva (UnAuto "Ferrari" "F50" (0 , 0) 65 0 ["La nave", "El fierro", "Ferrucho"]) 
UnAuto {marca = "Ferrari", modelo = "F50", desgaste = (6.5,5.0), velocidadMaxima = 65.0, tiempoDeCarrera = 3.0, apodos = ["La nave","El fierro","Ferrucho"]}
-}

---d---
desgasteQueGeneraElRulo :: Float -> Tramo
desgasteQueGeneraElRulo diametro unAuto = desgastarRuedas (diametro * 1.5) unAuto

aumentaTiempoDeCarreraEnElRulo ::  Float -> Tramo
aumentaTiempoDeCarreraEnElRulo diametro unAuto = aumentarTiempoDeCarrera (5 * diametro / velocidadMaxima unAuto) unAuto

ruloEnElAire :: Float -> Tramo
ruloEnElAire diametro unAuto = (desgasteQueGeneraElRulo diametro) . (aumentaTiempoDeCarreraEnElRulo diametro) $ unAuto

ruloClasico :: Tramo
ruloClasico unAuto = ruloEnElAire 13 unAuto

deseoDeMuerte :: Tramo
deseoDeMuerte unAuto = ruloEnElAire 26 unAuto

{-
CASOS DE PRUEBAS 

*Ejemplo> ruloClasico (UnAuto "Ferrari" "F50" (0 , 0) 65 0 ["La nave", "El fierro", "Ferrucho"]) 
UnAuto {marca = "Ferrari", modelo = "F50", desgaste = (19.5,0.0), velocidadMaxima = 65.0, tiempoDeCarrera = 1.0, apodos = ["La nave","El fierro","Ferrucho"]}
---
*Ejemplo> deseoDeMuerte (UnAuto "Ferrari" "F50" (0 , 0) 65 0 ["La nave", "El fierro", "Ferrucho"]) 
UnAuto {marca = "Ferrari", modelo = "F50", desgaste = (39.0,0.0), velocidadMaxima = 65.0, tiempoDeCarrera = 2.0, apodos = ["La nave","El fierro","Ferrucho"]}
-}

----------PUNTO 5----------
---a---
nivelDeJoyezIndividual :: Auto -> Int
nivelDeJoyezIndividual unAuto
    | esUnaJoya unAuto && tiempoDeCarrera unAuto < 50 = 1
    | esUnaJoya unAuto = 2
    | otherwise = 0

nivelDeJoyez :: [Auto] -> Int
nivelDeJoyez autos = sum . map nivelDeJoyezIndividual $ autos

{-
CASO DE PRUEBA

*Ejemplo> nivelDeJoyez [ferrari, UnAuto "Peugeot" "504" (0 , 0) 40 50 ["El rey del desierto"], UnAuto "Peugeot" "504" (0 , 0) 40 49 ["El rey del desierto"]]
3
-}

---b---
paraEntendidos :: [Auto] -> Bool
paraEntendidos autos = all esUnAutoEntendido autos  

esUnAutoEntendido :: Auto -> Bool
esUnAutoEntendido unAuto = estaEnBuenEstado unAuto && tiempoDeCarrera unAuto <= 200

{-
CASO DE PRUEBA

*Ejemplo> paraEntendidos [UnAuto "Ferrari" "F50" (0 , 0) 65 201 ["La nave", "El fierro", "Ferrucho"], UnAuto "Ferrari" "F50" (0 , 0) 65 200 ["La nave", "El fierro", "Ferrucho"]]
False
---
*Ejemplo> paraEntendidos [UnAuto "Ferrari" "F50" (0 , 0) 65 200 ["La nave", "El fierro", "Ferrucho"], UnAuto "Peugeot" "504" (0 , 0) 40 0 ["El rey del desierto"]]
False
---
*Ejemplo> paraEntendidos [UnAuto "Ferrari" "F50" (0 , 0) 65 200 ["La nave", "El fierro", "Ferrucho"], UnAuto "Lamborghini" "Diablo" (4 , 7) 73 200 ["Lambo", "La bestia"]]       
True
-}

----------ENTREGA 2----------

----------PUNTO 1----------
---a---
data Equipo = UnEquipo {
    nombreDeEquipo :: String,
    autosDelEquipo  :: [Auto],
    presupuestoDelEquipo :: Float
} deriving (Show)

equipoDe70000 :: Equipo
equipoDe70000 = UnEquipo "equipoDe70000" [] 70000

equipoDe50000 :: Equipo
equipoDe50000 = UnEquipo "equipoDe44000" [peugeot] 50000

costoDeAuto :: Auto -> Float
costoDeAuto (UnAuto _ _ _ maximaVelocidad _ _) = maximaVelocidad * 1000

modificarPresupuesto :: (Float -> Float) -> Equipo -> Equipo
modificarPresupuesto modificacion unEquipo = unEquipo {presupuestoDelEquipo = modificacion . presupuestoDelEquipo $ unEquipo}

adquirirAuto :: Auto -> Equipo -> Equipo
adquirirAuto unAuto unEquipo = unEquipo {autosDelEquipo = unAuto : autosDelEquipo unEquipo}

agregarUnAutoAlEquipo :: Auto -> Equipo -> Equipo
agregarUnAutoAlEquipo unAuto unEquipo
    |verificacionDelCosto unAuto unEquipo = adquirirAuto unAuto . modificarPresupuesto (restarCostoDeUnAuto unAuto ) $ unEquipo
    |otherwise = unEquipo

verificacionDelCosto :: Auto -> Equipo -> Bool
verificacionDelCosto unAuto unEquipo = costoDeAuto unAuto <= presupuestoDelEquipo unEquipo

restarCostoDeUnAuto :: Auto -> Float -> Float 
restarCostoDeUnAuto unAuto = subtract . costoDeAuto $ unAuto 

{-
CASOS DE PRUEBA
*Ejemplo> agregarUnAutoAlEquipo ferrari equipoDe70000 
UnEquipo {nombreDeEquipo = "equipoDe70000", autos = [UnAuto {marca = "Ferrari", modelo = "F50", desgaste = (0.0,0.0), velocidadMaxima = 65.0, tiempoDeCarrera = 0.0, apodos = ["La nave","El fierro","Ferrucho"]}], presupuesto = 5000.0}
---
*Ejemplo> agregarUnAutoAlEquipo fiat equipoDe50000
UnEquipo {nombreDeEquipo = "equipoDe44000", autos = [UnAuto {marca = "Fiat", modelo = "600", desgaste = (27.0,33.0), velocidadMaxima = 44.0, tiempoDeCarrera = 0.0, apodos = ["La Bocha","La bolita","Fitito"]},UnAuto {marca = "Peugeot", modelo = "504", desgaste = (0.0,0.0), velocidadMaxima = 40.0, tiempoDeCarrera = 0.0, apodos = ["El rey del desierto"]}], presupuesto = 6000.0}
---
*Ejemplo> agregarUnAutoAlEquipo lamborghini equipoDe70000 
UnEquipo {nombreDeEquipo = "equipoDe70000", autos = [], presupuesto = 70000.0}
-}

---b---
reparacionDeEquipo :: Equipo -> Equipo
reparacionDeEquipo (UnEquipo nombre [] presupuesto) = UnEquipo nombre [] presupuesto
reparacionDeEquipo (UnEquipo nombre (primerAuto : restoDeAutos) presupuesto)
  | puedeRepararseCon presupuesto primerAuto = agregarAuto (repararUnAuto primerAuto) (reparacionDeEquipo (UnEquipo nombre restoDeAutos  (presupuesto - costoReparacion primerAuto)))
  | otherwise = agregarAuto primerAuto (reparacionDeEquipo (UnEquipo nombre restoDeAutos presupuesto))

agregarAuto :: Auto -> Equipo -> Equipo
agregarAuto unAuto (UnEquipo nombre unosAutos presupuesto) = UnEquipo nombre (unAuto : unosAutos) presupuesto

puedeRepararseCon :: Float -> Auto -> Bool
puedeRepararseCon unPresupuesto unAuto = costoReparacion unAuto <= unPresupuesto

repararChasis :: Auto -> Float
repararChasis unAuto = (* 0.15) . desgasteChasis $ unAuto

costoReparacion :: Auto -> Float
costoReparacion unAuto = (* 500) . puntosDeDesgasteReducido  $ unAuto

puntosDeDesgasteReducido :: Auto -> Float
puntosDeDesgasteReducido unAuto = restarReparacionDelChasis unAuto . desgasteChasis $ unAuto

restarReparacionDelChasis :: Auto -> Float -> Float
restarReparacionDelChasis unAuto = subtract . repararChasis $ unAuto 

equipoDe20000 :: Equipo
equipoDe20000 = UnEquipo "equipoDe20000" [UnAuto "Ferrari" "F50" (0 , 10) 65 0 ["La nave", "El fierro", "Ferrucho"], UnAuto "Lamborghini" "Diablo" (4 , 20) 73 0 ["Lambo", "La bestia"]] 20000

equipoDe10000 :: Equipo
equipoDe10000 = UnEquipo "equipoDe10000" [UnAuto "Fiat" "600" (27 , 50) 44 0 ["La Bocha","La bolita", "Fitito"]] 10000

{-
CASOS DE PRUEBA

*Ejemplo> reparacionDeEquipo equipoDe20000
UnEquipo {nombreDeEquipo = "equipoDe20000", autosDelEquipo = [UnAuto {marca = "Ferrari", modelo = "F50", desgaste = (0.0,1.5), velocidadMaxima = 65.0, 
tiempoDeCarrera = 0.0, apodos = ["La nave","El fierro","Ferrucho"]},UnAuto {marca = "Lamborghini", modelo = "Diablo", desgaste = (0.0,3.0), velocidadMaxima = 73.0, tiempoDeCarrera = 0.0, apodos = ["Lambo","La bestia"]}], presupuestoDelEquipo = 7250.0}
---
*Ejemplo> reparacionDeEquipo equipoDe10000
UnEquipo {nombreDeEquipo = "equipoDe10000", autosDelEquipo = [UnAuto {marca = "Fiat", modelo = "600", desgaste = (27.0,50.0), velocidadMaxima = 44.0, 
tiempoDeCarrera = 0.0, apodos = ["La Bocha","La bolita","Fitito"]}], presupuestoDelEquipo = 10000.0}
-}

---c---
optimizarAutosDeEquipo :: Equipo -> Equipo
optimizarAutosDeEquipo (UnEquipo nombre [] presupuesto) = UnEquipo nombre [] presupuesto
optimizarAutosDeEquipo (UnEquipo nombre (primerAuto : restoDeAutos) presupuesto)
  | puedeOptimizarseCon presupuesto primerAuto = agregarAuto (ponerNitro primerAuto) (optimizarAutosDeEquipo (UnEquipo nombre restoDeAutos (presupuesto - costoDeOptimizacion primerAuto)))
  | otherwise = agregarAuto primerAuto (UnEquipo nombre restoDeAutos presupuesto)

puedeOptimizarseCon :: Float -> Auto -> Bool
puedeOptimizarseCon unPresupuesto unAuto = costoDeOptimizacion unAuto <= unPresupuesto

costoDeOptimizacion :: Auto -> Float
costoDeOptimizacion unAuto = (* 100) . velocidadMaxima $ unAuto 

equipoDe10000' :: Equipo
equipoDe10000' = UnEquipo "equipoDe10000" [UnAuto "Ferrari" "F50" (0 , 10) 65 0 ["La nave", "El fierro", "Ferrucho"], UnAuto "Lamborghini" "Diablo" (4 , 20) 73 0 ["Lambo", "La bestia"]] 10000

{-
CASOS DE PRUEBA

*Ejemplo> optimizarAutosDeEquipo equipoDe20000 
UnEquipo {nombreDeEquipo = "equipoDe20000", autosDelEquipo = [UnAuto {marca = "Ferrari", modelo = "F50", desgaste = (0.0,10.0), velocidadMaxima = 78.0, 
tiempoDeCarrera = 0.0, apodos = ["La nave","El fierro","Ferrucho"]},UnAuto {marca = "Lamborghini", modelo = "Diablo", desgaste = (4.0,20.0), 
velocidadMaxima = 87.600006, tiempoDeCarrera = 0.0, apodos = ["Lambo","La bestia"]}], presupuestoDelEquipo = 6200.0}
---
*Ejemplo> optimizarAutosDeEquipo equipoDe10000' 
UnEquipo {nombreDeEquipo = "equipoDe10000", autosDelEquipo = [UnAuto {marca = "Ferrari", modelo = "F50", desgaste = (0.0,10.0), velocidadMaxima = 78.0, 
tiempoDeCarrera = 0.0, apodos = ["La nave","El fierro","Ferrucho"]},UnAuto {marca = "Lamborghini", modelo = "Diablo", desgaste = (4.0,20.0), 
velocidadMaxima = 73.0, tiempoDeCarrera = 0.0, apodos = ["Lambo","La bestia"]}], presupuestoDelEquipo = 3500.0}
-}

---c---
ferrarizar :: Equipo -> Equipo
ferrarizar (UnEquipo nombre [] presupuesto) = UnEquipo nombre [] presupuesto
ferrarizar (UnEquipo nombre (primerAuto : restoDeAutos) presupuesto)
  | marca primerAuto == "Ferrari" = agregarAuto primerAuto (ferrarizar (UnEquipo nombre restoDeAutos presupuesto))
  | puedeFerrarizarseCon presupuesto = agregarAuto (llevarUnAutoADesarmar "Ferrari" "F50" primerAuto) (ferrarizar (UnEquipo nombre restoDeAutos (presupuesto - 3500)))
  | otherwise = agregarAuto primerAuto (UnEquipo nombre restoDeAutos presupuesto)

puedeFerrarizarseCon :: Float -> Bool
puedeFerrarizarseCon unPresupuesto = 3500 <= unPresupuesto 

{-
CASOS DE PRUEBA

*Ejemplo> ferrarizar (UnEquipo "equipoDe20000" [peugeot, lamborghini] 20000)
UnEquipo {nombreDeEquipo = "equipoDe20000", autosDelEquipo = [UnAuto {marca = "Ferrari", modelo = "F50", desgaste = (0.0,0.0), velocidadMaxima = 40.0, 
tiempoDeCarrera = 0.0, apodos = ["Nunca taxi"]},UnAuto {marca = "Ferrari", modelo = "F50", desgaste = (4.0,7.0), velocidadMaxima = 73.0, 
tiempoDeCarrera = 0.0, apodos = ["Nunca taxi"]}], presupuestoDelEquipo = 13000.0}
---
*Ejemplo> ferrarizar (UnEquipo "equipoDe20000" [peugeot, lamborghini] 4000)
UnEquipo {nombreDeEquipo = "equipoDe20000", autosDelEquipo = [UnAuto {marca = "Ferrari", modelo = "F50", desgaste = (0.0,0.0), velocidadMaxima = 40.0,
tiempoDeCarrera = 0.0, apodos = ["Nunca taxi"]},UnAuto {marca = "Lamborghini", modelo = "Diablo", desgaste = (4.0,7.0), velocidadMaxima = 73.0, 
tiempoDeCarrera = 0.0, apodos = ["Lambo","La bestia"]}], presupuestoDelEquipo = 500.0}
---
*Ejemplo> ferrarizar (UnEquipo "equipoDe20000" [peugeot, ferrari, lamborghini] 20000)
UnEquipo {nombreDeEquipo = "equipoDe20000", autosDelEquipo = [UnAuto {marca = "Ferrari", modelo = "F50", desgaste = (0.0,0.0), velocidadMaxima = 40.0, tiempoDeCarrera = 0.0, apodos = ["Nunca taxi"]},
UnAuto {marca = "Ferrari", modelo = "F50", desgaste = (0.0,0.0), velocidadMaxima = 65.0, tiempoDeCarrera = 0.0, apodos = ["La nave","El fierro","Ferrucho"]},
UnAuto {marca = "Ferrari", modelo = "F50", desgaste = (4.0,7.0), velocidadMaxima = 73.0, tiempoDeCarrera = 0.0, apodos = ["Nunca taxi"]}], presupuestoDelEquipo = 13000.0}
-}

----------PUNTO 2----------
costoTotalDeReparacion :: Equipo -> Float
costoTotalDeReparacion unEquipo = costoDeRepararListaDeAutos . autosDelEquipo $ unEquipo

costoDeRepararListaDeAutos :: [Auto] -> Float
costoDeRepararListaDeAutos unaListaDeAutos = sum . map costoReparacion $ unaListaDeAutos

{-
CASOS DE PRUEBA

*Ejemplo> costoTotalDeReparacion equipoDe20000
12750.0
--
*Ejemplo> costoTotalDeReparacion (UnEquipo "pepe" [UnAuto "Fiat" "600" (27 , 50) 44 0 ["La Bocha","La bolita", "Fitito"], peugeot] 10000)
21250.0
-}

----------PUNTO 3----------
listaDeFerrarisDeInfinia :: Auto -> [Auto]
listaDeFerrarisDeInfinia unAuto =  iterate (aumentarVelocidad 65) unAuto

mapVelocidad :: (Float -> Float) -> Auto -> Auto
mapVelocidad modificacion unAuto = unAuto {velocidadMaxima = modificacion.velocidadMaxima $ unAuto }

aumentarVelocidad :: Float -> Auto -> Auto
aumentarVelocidad aumentoDeVelocidad unAuto = mapVelocidad (+ aumentoDeVelocidad) unAuto

infinia :: Equipo
infinia = UnEquipo "Infinia" agregarFerraris 5000

agregarFerraris :: [Auto]
agregarFerraris = listaDeFerrarisDeInfinia (UnAuto "Ferrari" "F50" (0,1) 65 0 ["La nave", "El fierro", "Ferrucho"])

{-Contestar qué sucede si:
i) Se realiza una reparación en equipo de ese equipo.

reparacionDeEquipo aplicada en Infinia no rompe, pero si se cuelga y no imprime nada por pantalla. 
Esto es debido a que Haskell utiliza lazy evaluation. Nuestra funcion, cada vez que evalua un auto, lo coloca al principio
de la lista, independientemente de la condicion (es decir, si pudo ser reparado o no). Ademas, al ser una lista infinita, 
esta no tiene fin. Por lo que, Haskell no muestra nada por pantalla porque esta modificando constantemente el prinicio de 
la lista.

ii) Se optimizan los autos de ese equipo

En la funcion optimizarAutosDeEquipo, Haskell muestra por pantalla infinidad de Ferraris. Esto es debido a que, aunque 
utilice lazy evaluation, al ser el presupesto del equipo infinia menor al costo de optimizar un Ferrari, la funcion solo 
ejecuta el otherwise devolviendo el auto evaluado sin modificar y el resto de la lista infinita.  

iii) Se ferrarizan sus autos.

Usando ferrarizar con infinia, no se rompe pero se cuelga y no imprime nada por pantalla. La funcion toma el primer elemento de la lista y como cumple 
con la condicion de ser Ferrari provoca que lo coloque al principio de la lista, y nuevamente llame a la funcion. Como la
lista no va a ser nunca vacia, todos los autos son Ferraris y el presupuesto no disminuye, la recursividad no corta. Por 
lo que, Haskell no muestra nada por pantalla porque esta modificando constantemente el prinicio de la lista.

iv) Se quiere conocer el costo total de reparación del equipo.
Si se aplica costoTotalDeReparacion a infinia, Haskell se cuelga indefinidamente y no muestra nada
por pantalla. 
Al tratarse de una lista infinita, la funcion no termina de sumar todos sus elementos, por lo que no puede
mostrar un resultado parcial y por lo tanto no imprime nada por pantalla.

-}

---PUNTO 4---
---a---

type ModificadorDeTramo = Tramo -> Tramo

boxes :: ModificadorDeTramo
boxes unTramo unAuto
  | estaEnBuenEstado unAuto = unTramo unAuto 
  | otherwise = pasarPorBox unTramo unAuto

pasarPorBox :: ModificadorDeTramo
pasarPorBox unTramo unAuto = manosAlaObra unTramo unAuto

manosAlaObra :: ModificadorDeTramo
manosAlaObra unTramo unAuto = repararUnAuto . aplicarPenalidad 10 . unTramo $ unAuto

---b---

tiempoExtra :: Tramo -> Auto -> Float
tiempoExtra unTramo unAuto = 0.5 * restarTiempoDeCarrera (unTramo unAuto) unAuto 

restarTiempoDeCarrera :: Auto -> Auto -> Float
restarTiempoDeCarrera autoModificado unAuto = tiempoDeCarrera autoModificado - tiempoDeCarrera unAuto

mojado :: ModificadorDeTramo
mojado unTramo unAuto = modificarTiempoDeCarrera (+ tiempoExtra unTramo unAuto) . unTramo $ unAuto

---c---

ripio :: ModificadorDeTramo
ripio unTramo unAuto = atravesarRipio unTramo unAuto 

atravesarRipio :: ModificadorDeTramo
atravesarRipio unTramo unAuto = unTramo . unTramo $ unAuto

---d---

obstruccion :: Float -> ModificadorDeTramo 
obstruccion metrosDePista unTramo unAuto = desgastarRuedas (metrosDePistaObstruidos metrosDePista) . unTramo $ unAuto

metrosDePistaObstruidos :: Float -> Float
metrosDePistaObstruidos metrosDePista = metrosDePista * 2

---e---

turbo :: ModificadorDeTramo
turbo unTramo unAuto = aplicarTurbo unTramo unAuto

aplicarTurbo :: ModificadorDeTramo
aplicarTurbo unTramo unAuto = restablecerVelocidad . unTramo . duplicarVelocidad $ unAuto

duplicarVelocidad :: Auto -> Auto 
duplicarVelocidad unAuto = mapVelocidad (*2)  unAuto

restablecerVelocidad :: Auto -> Auto
restablecerVelocidad unAuto = mapVelocidad (restarVelocidadMaxima unAuto) unAuto

restarVelocidadMaxima :: Auto -> Float -> Float
restarVelocidadMaxima unAuto = subtract . velocidadMaxima $ unAuto


---PUNTO 5---
pasarPorTramo :: ModificadorDeTramo
pasarPorTramo unTramo unAuto 
  | noDaMas unAuto = unAuto
  | otherwise = unTramo unAuto

---PUNTO 6---
---a---
laVueltaALaManzana :: Pista
laVueltaALaManzana = UnaPista "La manzana" "Italia" 30 [recto 130, curva 90 13, recto 130, curva 90 13, recto 130, curva 90 13, recto 130, curva 90 13]

---b---
superPista :: Pista
superPista = UnaPista "Super Pista" "Argentina" 300 [tramoRectoClassic, curvaTranca, turbo tramito, mojado tramito, ruloEnElAire 10, obstruccion 2 (curva 80 400), curva 115 650, recto 970, curvaPeligrosa, ripio tramito, boxes (recto 800), obstruccion 5 casiCurva, zigzag 2, mojado.ripio $ deseoDeMuerte, ruloClasico, zigZagLoco]

---c---
pegaLaVuelta :: Pista -> Auto -> Auto
pegaLaVuelta unaPista unAuto = foldl (flip pasarPorTramo) unAuto (tramos unaPista) 

peganLaVuelta :: Pista -> [Auto] -> [Auto]
peganLaVuelta unaPista [] = []
peganLaVuelta unaPista (primerAuto : restoDeAutos)
  | noDaMas primerAuto = peganLaVuelta unaPista restoDeAutos
  | otherwise = (pegaLaVuelta unaPista primerAuto) : (peganLaVuelta unaPista restoDeAutos) 

{-
CASOS DE PRUEBAS 

*Ejemplo> peganLaVuelta laVueltaALaManzana [ferrari, (UnAuto "Peugeot" "504" (79 , 0) 40 0 ["El rey del desierto"])]  
[UnAuto {marca = "Ferrari", modelo = "F50", desgaste = (1.7333333,5.2), velocidadMaxima = 65.0, tiempoDeCarrera = 9.6, 
apodos = ["La nave","El fierro","Ferrucho"]},UnAuto {marca = "Peugeot", modelo = "504", desgaste = (80.3,3.8999999), velocidadMaxima = 40.0,
tiempoDeCarrera = 11.7, apodos = ["El rey del desierto"]}]

-}

---PUNTO 7---
---a---
data Carrera = UnaCarrera{
  pista :: Pista,
  numeroDeVueltas :: Int

} deriving (Show)

---b---
tourBuenosAires :: Carrera
tourBuenosAires = UnaCarrera superPista 20

---c---

type Vuelta = [[Auto]]

mapNumeroDeVueltas :: (Int -> Int) -> Carrera -> Carrera
mapNumeroDeVueltas modificacion unaCarrera = unaCarrera {numeroDeVueltas = modificacion.numeroDeVueltas $ unaCarrera}

resultadosDeLaCarrera :: Carrera -> [Auto] -> Vuelta
resultadosDeLaCarrera unaCarrera unosAutos = reverse (jueganUnaCarrera unaCarrera unosAutos)

jueganUnaCarrera :: Carrera -> [Auto] -> Vuelta
jueganUnaCarrera (UnaCarrera nombre 1) autos = [autosActualizados (UnaCarrera nombre 1) autos]
jueganUnaCarrera carrera autos = autosActualizados carrera autos : jueganUnaCarrera (mapNumeroDeVueltas (subtract 1) carrera) (autosActualizados carrera autos)

autosActualizados :: Carrera -> [Auto] -> [Auto]
autosActualizados unaCarrera unosAutos = ordenDeLLegada (peganLaVuelta (pista unaCarrera) unosAutos)

quickSortBy :: Ord b => (a -> b) -> [a] -> [a]
quickSortBy _ [] = []
quickSortBy valoracion (x:xs) = anteriores ++ [x] ++ posteriores    
    where
        anteriores  = quickSortBy valoracion $ filter ((< valoracion x).valoracion)  xs
        posteriores = quickSortBy valoracion $ filter ((>= valoracion x).valoracion) xs

ordenDeLLegada :: [Auto] -> [Auto]
ordenDeLLegada unosAutos = quickSortBy tiempoDeCarrera unosAutos

listaDeFinalistas :: Carrera -> [Auto] -> [Auto]
listaDeFinalistas unaCarrera unosAutos = head . (resultadosDeLaCarrera unaCarrera) $ unosAutos

---i) El auto ganador luego de todas las vueltas de la carrera.---
ganador :: Carrera -> [Auto] -> Auto  
ganador unaCarrera unosAutos = head . (listaDeFinalistas unaCarrera) $ unosAutos

---ii) El tiempo total del segundo.---
segundoLugar :: Carrera -> [Auto] -> Auto
segundoLugar unaCarrera unosAutos = head . obtenerSegundo unaCarrera $ unosAutos

obtenerSegundo :: Carrera -> [Auto] -> [Auto]
obtenerSegundo unaCarrera unosAutos = drop 1 . (listaDeFinalistas unaCarrera) $ unosAutos

tiempoDelSegundo :: Carrera -> [Auto] -> Float
tiempoDelSegundo unaCarrera unosAutos = tiempoDeCarrera . (segundoLugar unaCarrera) $ unosAutos

---iii) El tiempo parcial tras 2 vueltas del primer auto.---
segundaVuelta :: Carrera -> [Auto] -> [Auto]
segundaVuelta unaCarrera unosAutos = (!! 1) . (resultadosDeLaCarrera unaCarrera) $ unosAutos

tiempoParcialSegundaVuelta :: Carrera -> [Auto] -> Float
tiempoParcialSegundaVuelta unaCarrera unosAutos = tiempoDeCarrera . head . (segundaVuelta unaCarrera) $ unosAutos

---iv) Cantidad de autos que terminaron la carrera.---
cantidadDeAutosQueTerminaronLaCarrera :: Carrera -> [Auto] -> Int
cantidadDeAutosQueTerminaronLaCarrera unaCarrera unosAutos = length . (listaDeFinalistas unaCarrera) $ unosAutos
