module Puzzle

type estado = int * int * int * int

type acciones = ARRIBA | IZQUIERDA | DERECHA | ABAJO

let sucesor estado accion =
    match accion with
        | ARRIBA ->
              match estado with
                | (x1, x2, 0, x4) -> Some (0, x2, x1, x4)
                | (x1, x2, x3, 0) -> Some (x1, 0, x3, x2)
                | _ -> None
        | IZQUIERDA ->
              match estado with
                | (x1, 0, x3, x4) -> Some (0, x1, x3, x4)
                | (x1, x2, x3, 0) -> Some (x1, x2, 0, x3)
                | _ -> None
        | DERECHA ->
              match estado with
                | (0, x2, x3, x4) -> Some (x2, 0, x3, x4)
                | (x1, x2, 0, x4) -> Some (x1, x2, x4, 0)
                | _ -> None
        | ABAJO ->
              match estado with
                | (0, x2, x3, x4) -> Some (x3, x2, 0, x4)
                | (x1, 0, x3, x4) -> Some (x1, x4, x3, 0)
                | _ -> None

let estado_inicial_14 = (3, 1, 0, 2)

let prueba1 () = 
    let problema = 
        {inicio = estado_inicial_14
         meta = function | (1, 2, 3, 0) -> true
                         | _ -> false
         sucesor = sucesor
         acciones = [ARRIBA; IZQUIERDA; DERECHA; ABAJO]
         costo = fun _ _ _ -> 1
        } : Busqueda.problema<estado, acciones>
    Busqueda.busqueda (BFS.estrategia_bfs1<estado,acciones>) problema

let prueba2 () = 
    let problema = 
        {inicio = estado_inicial_14
         meta = function | (1, 2, 3, 0) -> true
                         | _ -> false
         sucesor = sucesor
         acciones = [ARRIBA; IZQUIERDA; DERECHA; ABAJO]
         costo = fun _ _ _ -> 1
        } : Busqueda.problema<estado, acciones>
    Busqueda.busqueda (BFS.estrategia_bfs2<estado,acciones>) problema
