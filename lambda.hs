type Requisito = Depto -> Bool
type Busqueda = [Requisito]
type Depto = (Int, Int, Int, String)
type Persona = (String, [Busqueda])
deptosDeEjemplo = [(3,80,7500,"Palermo"), (1,45,3500,"Villa Urquiza"), (2,50,5000,"Palermo"), (1,45,5500,"Recoleta")]

ambientes (a, _,_,_) =a
superficie (_,m2,_,_) = m2
precio (_,_,p,_)= p
barrio (_,_,_,b) = b

mail persona = fst persona
busquedas persona = snd persona

ordenarSegun :: (a -> a -> Bool) -> [a] -> [a]
ordenarSegun _ [] = []
ordenarSegun criterio (x:xs) = (ordenarSegun criterio.filter (not.criterio x)) xs ++ [x] ++ (ordenarSegun criterio.filter (criterio x)) xs
--MAYOR => filter devuelve lista con menores a X
between x y z = x <= z && y >= z

mayorPrecio depto1 depto2 = (precio depto1)>(precio depto2)
menorPrecio depto1 depto2 = (precio depto1)<(precio depto2)
mayorSuperficie depto1 depto2 = (superficie depto1)>(superficie depto2)

ubicadoEn barrios depto = elem (barrio depto) barrios
cumpleRango precioMayor precioMenor depto = between precioMenor precioMayor (precio depto)
cumpleAmbientes ambienteMayor ambienteMenor depto = between ambienteMenor ambienteMayor (ambientes depto)

cumpleBusqueda :: Depto -> Busqueda -> Bool
cumpleBusqueda depto filtros = all (aplicarFiltro depto) (filtros)
aplicarFiltro depto filtro = filtro depto

buscar filtros criterioOrdenamiento deptos = ordenarSegun criterioOrdenamiento (filter (flip cumpleBusqueda filtros) deptos)
prueba = buscar [ubicadoEn ["Recoleta","Palermo"],cumpleRango 6000 0,cumpleAmbientes 2 1] (mayorSuperficie) deptosDeEjemplo

--Mostrar un ejemplo de uso de buscar para obtener los departamentos que se encuentren en Recoleta o Palermo de 1 a 2 ambientes que se alquilen 
--a menos de $6000 por mes, de modo que el resultado se encuentre ordenado por mayor superficie con la lista de departamentos de ejemplo dada.