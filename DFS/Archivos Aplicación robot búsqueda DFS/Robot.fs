module Robot

type estado =  string 

type acciones = Izquierda | Derecha 

let sucesor estado accion =
    match accion with
        | Izquierda ->
              match estado with
                | "D" -> Some "B"
                | "B" -> Some "A"
                | "E" -> Some "G"
                | _   -> None

        | Derecha ->
              match estado with
                | "D" -> Some "E"
                | "B" -> Some "C"
                | "E" -> Some "F"
                | _   -> None
let estado_inicial = "D"

let explorar () = 
    let problema = 
        {inicio = estado_inicial
         meta = function | "B" -> true
                         | "G" -> true
                         | _ -> false
         sucesor = sucesor
         acciones = [Izquierda;Derecha]
         costo = fun _ _ _ -> 1
        } : Busqueda.problema<estado, acciones>
    Busqueda.busqueda (DFS.estrategia_dfs<estado,acciones>) problema