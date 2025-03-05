import Data.Time.Clock
import Data.List
import System.IO
import Control.Exception
import Control.DeepSeq (deepseq)


data Estudiante = Estudiante {   
    identify :: String,
    nombre :: String,
    entrada :: UTCTime,
    salida :: Maybe UTCTime  
} deriving (Show, Read)


registrarEntrada :: String -> String -> UTCTime -> [Estudiante] -> [Estudiante]
registrarEntrada identifyEstudiante nombreEstudiante tiempo lista =
    Estudiante identifyEstudiante nombreEstudiante tiempo Nothing : lista


registrarSalida :: String -> UTCTime -> [Estudiante] -> [Estudiante]
registrarSalida identifyEstudiante tiempo lista =
    map (\v -> if identifyEstudiante == identify v then v { salida = Just tiempo } else v) lista


buscarEstudiante :: String -> [Estudiante] -> Maybe Estudiante
buscarEstudiante identifyEstudiante lista =
    find (\v -> identifyEstudiante == identify v && isNothing (salida v)) lista
    where
        isNothing Nothing = True
        isNothing _       = False


tiempoEnLista :: Estudiante -> IO NominalDiffTime
tiempoEnLista estudiante = do
    tiempoActual <- getCurrentTime
    return $ diffUTCTime tiempoActual (entrada estudiante)


guardarLista :: [Estudiante] -> IO ()
guardarLista lista = do
    withFile "lista.txt" WriteMode $ \h -> do
        hPutStr h (unlines (map mostrarEstudiante lista))
    putStrLn "Lista guardado en el archivo lista.txt."


cargarLista :: IO [Estudiante]
cargarLista = do
    contenido <- withFile "lista.txt" ReadMode $ \h -> do
        contenido <- hGetContents h
        contenido `deepseq` return contenido
    let lineas = lines contenido
    return (map leerEstudiante lineas)
    where
        leerEstudiante linea = read linea :: Estudiante


mostrarEstudiante :: Estudiante -> String
mostrarEstudiante (Estudiante identify nombre entrada salida) =
    "Estudiante {id = \"" ++ identify ++ "\",nombre = \"" ++ nombre ++ "\", entrada = " ++ show entrada ++ ", salida = " ++ maybe "Nothing" show salida ++ "}" 


listarEstudiantes :: [Estudiante] -> IO ()
listarEstudiantes [] = putStrLn "No hay estudiantes en el lista."
listarEstudiantes estudiantes = do
    putStrLn "Estudiantes en el lista:"
    mapM_ (putStrLn . mostrarEstudiante) estudiantes


main :: IO ()
main = do

    lista <- cargarLista
    putStrLn "¡Bienvenido al Sistema de Gestión de Lista!"


    cicloPrincipal lista


cicloPrincipal :: [Estudiante] -> IO ()
cicloPrincipal lista = do
    putStrLn "\nSeleccione una opción:"
    putStrLn "1. Registrar Entrada de estudiante"
    putStrLn "2. Buscar Estudiante por id"
    putStrLn "3. Calcular Tiempo de Estudiante"
    putStrLn "4. Listar Estudiantes"    
    putStrLn "5. Registrar Salida de estudiante"
    putStrLn "6. Salir"

    opcion <- getLine
    case opcion of
        "1" -> do
            putStrLn "Ingrese la id del estudiante:"
            identifyEstudiante <- getLine
            putStrLn "Ingrese el nombre del estudiante:"
            nombreEstudiante <- getLine
            tiempoActual <- getCurrentTime
            let listaActualizado = registrarEntrada identifyEstudiante nombreEstudiante tiempoActual lista
            putStrLn $ "Estudiante con id " ++ identifyEstudiante ++ " y nombre " ++ nombreEstudiante ++ " ingresado al lista."
            guardarLista listaActualizado
            cicloPrincipal listaActualizado

        

        "2" -> do
            putStrLn "Ingrese la id del estudiante a buscar:"
            identifyEstudiante <- getLine
            case buscarEstudiante identifyEstudiante lista of
                Just estudiante -> do
                    tiempoTotal <- tiempoEnLista estudiante
                    nombreEstudiante <- return (nombre estudiante)
                    putStrLn $ "El estudiante con id " ++ identifyEstudiante ++ " se encuentra en la Universidad."
                    putStrLn $ "El nombre del estudiante es: " ++ nombreEstudiante
                Nothing -> putStrLn "Estudiante no encontrado en el lista."
            cicloPrincipal lista

        "3" -> do
            putStrLn "Ingrese la id del estudiante para saber su tiempo:"
            identifyEstudiante <- getLine
            case buscarEstudiante identifyEstudiante lista of
                Just estudiante -> do
                    tiempoTotal <- tiempoEnLista estudiante
                    nombreEstudiante <- return (nombre estudiante)
                    putStrLn $ "El estudiante con id '" ++ identifyEstudiante ++ "' lleva en la universidad: " ++ show tiempoTotal ++ " segundos." 
                Nothing -> putStrLn "Estudiante no encontrado en el lista."
            cicloPrincipal lista

        "4" -> do
            listarEstudiantes lista
            cicloPrincipal lista

        "5" -> do
                putStrLn "Ingrese la id del estudiante a salir:"
                identifyEstudiante <- getLine
                tiempoActual <- getCurrentTime
                let listaActualizado = registrarSalida identifyEstudiante tiempoActual lista
                putStrLn $ "Estudiante con id " ++ identifyEstudiante ++ " salido del lista."                   
                guardarLista listaActualizado
                cicloPrincipal listaActualizado

        "6" -> putStrLn "¡Hasta luego!"

        _ -> do
            putStrLn "Opción no válida. Por favor, seleccione una opción válida."
            cicloPrincipal lista