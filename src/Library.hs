module Library where
import PdePreludat
import Data.String (IsString)


doble :: Number -> Number
doble numero = numero + numero

triple :: Number -> Number
triple numero = numero *3

letraMenorMayor :: String -> Bool
letraMenorMayor nombreEmpresa
    |last nombreEmpresa < head nombreEmpresa = True
    |otherwise = False

letrasIntermedias :: String -> Number
letrasIntermedias  nombreEmpresa = length nombreEmpresa - 2

nombreCapicua :: String -> Bool
nombreCapicua nombreEmpresa
    |reverse nombreEmpresa == nombreEmpresa = True
    |otherwise = False

dobleCantidadLetras :: String -> Number
dobleCantidadLetras nombreEmpresa = 2*length nombreEmpresa

esDivisible :: Number -> Number -> Bool
esDivisible nombreEmpresa tresOsiete = mod nombreEmpresa tresOsiete == 0

cantidadEmpleados :: String -> Number
cantidadEmpleados nombreEmpresa
    |nombreEmpresa == "Acme" = 10
    |letraMenorMayor nombreEmpresa = letrasIntermedias nombreEmpresa
    |nombreCapicua nombreEmpresa = dobleCantidadLetras nombreEmpresa
    |esDivisible (length nombreEmpresa) 3 || esDivisible (length nombreEmpresa) 7 = 3
    |otherwise = 0
