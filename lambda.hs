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
-- "filter (not.criterio x)" => devuelve lista con menores a X

between::(Ord a,Eq a)=>a->a->a->Bool
between x y z = x <= z && y >= z

mayorPrecio::Depto->Depto->Bool
mayorPrecio depto1 depto2 = (precio depto1)>(precio depto2)
menorPrecio::Depto->Depto->Bool
menorPrecio depto1 depto2 = (precio depto1)<(precio depto2)
mayorSuperficie::Depto->Depto->Bool
mayorSuperficie depto1 depto2 = (superficie depto1)>(superficie depto2)

ubicadoEn::[String]->Depto->Bool
ubicadoEn barrios depto = elem (barrio depto) barrios
cumpleRango::Int->Int->Depto->Bool
cumpleRango precioMayor precioMenor depto = between precioMenor precioMayor (precio depto)
cumpleAmbientes::Int->Int->Depto->Bool
cumpleAmbientes ambienteMayor ambienteMenor depto = between ambienteMenor ambienteMayor (ambientes depto)

cumpleBusqueda :: Depto -> [(Depto->Bool)] -> Bool
cumpleBusqueda depto filtros = all (aplicarFiltro depto) (filtros)
aplicarFiltro::Depto->(Depto->Bool)->Bool
aplicarFiltro depto filtro = filtro depto

buscar::[(Depto->Bool)]->(Depto->Depto->Bool)->[Depto]->[Depto]
buscar filtros criterioOrdenamiento deptos = ordenarSegun criterioOrdenamiento (filter (flip cumpleBusqueda filtros) deptos)

prueba = buscar [ubicadoEn ["Recoleta","Palermo"],cumpleRango 6000 0,cumpleAmbientes 2 1] mayorSuperficie deptosDeEjemplo
prueba2 = mailsDePersonasInteresadas  (1,80,5500,"Caballito") [("gabo",[ubicadoEn ["Recoleta","Palermo"]]),("julian",[cumpleRango 6000 0]),("soledad",[cumpleAmbientes 2 1])]

mailsDePersonasInteresadas::Depto->[(String,[Depto->Bool])]->[String]
mailsDePersonasInteresadas depto personas = map mail (filter (aciertaBusqueda depto) personas)
aciertaBusqueda::Depto->(String,[Depto->Bool])->Bool
aciertaBusqueda depto persona = cumpleBusqueda depto (busquedas persona)