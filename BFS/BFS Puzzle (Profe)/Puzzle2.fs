module Puzzle

type estado = int * int * int * int * int * int * int * int * int

type acciones = ARRIBA | IZQUIERDA | DERECHA | ABAJO

let sucesor estado accion =
    match accion with
        | ARRIBA ->
              match estado with
                | (x1, x2, x3, 0, x5, x6, x7, x8, x9) -> Some (0, x2, x3, x1, x5, x6, x7, x8, x9)
                | (x1, x2, x3, x4, 0, x6, x7, x8, x9) -> Some (x1, 0, x3, x4, x2, x6, x7, x8, x9)
                | (x1, x2, x3, x4, x5, 0, x7, x8, x9) -> Some (x1, x2, 0, x4, x5, x3, x7, x8, x9)
                | (x1, x2, x3, x4, x5, x6, 0, x8, x9) -> Some (x1, x2, x3, 0, x5, x6, x4, x8, x9)
                | (x1, x2, x3, x4, x5, x6, x7, 0, x9) -> Some (x1, x2, x3, x4, 0, x6, x7, x5, x9)
                | (x1, x2, x3, x4, x5, x6, x7, x8, 0) -> Some (x1, x2, x3, x4, x5, 0, x7, x8, x6)
                | _ -> None
        | IZQUIERDA ->
              match estado with
                | (x1, 0, x3, x4, x5, x6, x7, x8, x9) -> Some (0, x1, x3, x4, x5, x6, x7, x8, x9)
                | (x1, x2, 0, x4, x5, x6, x7, x8, x9) -> Some (x1, 0, x2, x4, x5, x6, x7, x8, x9)
                | (x1, x2, x3, x4, 0, x6, x7, x8, x9) -> Some (x1, x2, x3, 0, x4, x6, x7, x8, x9)
                | (x1, x2, x3, x4, x5, 0, x7, x8, x9) -> Some (x1, x2, x3, x4, 0, x5, x7, x8, x9)
                | (x1, x2, x3, x4, x5, x6, x7, 0, x9) -> Some (x1, x2, x3, x4, x5, x6, 0, x7, x9)
                | (x1, x2, x3, x4, x5, x6, x7, x8, 0) -> Some (x1, x2, x3, x4, x5, x6, x7, 0, x8)
                | _ -> None
        | DERECHA ->
              match estado with
                | (0, x2, x3, x4, x5, x6, x7, x8, x9) -> Some (x2, 0, x3, x4, x5, x6, x7, x8, x9)
                | (x1, 0, x3, x4, x5, x6, x7, x8, x9) -> Some (x1, x3, 0, x4, x5, x6, x7, x8, x9)
                | (x1, x2, x3, 0, x5, x6, x7, x8, x9) -> Some (x1, x2, x3, x5, 0, x6, x7, x8, x9)
                | (x1, x2, x3, x4, 0, x6, x7, x8, x9) -> Some (x1, x2, x3, x4, x6, 0, x7, x8, x9)
                | (x1, x2, x3, x4, x5, x6, 0, x8, x9) -> Some (x1, x2, x3, x4, x5, x6, x8, 0, x9)
                | (x1, x2, x3, x4, x5, x6, x7, 0, x9) -> Some (x1, x2, x3, x4, x5, x6, x7, x9, 0)
                | (x1, x2, x3, x4, x5, x6, x7, x8, 0) -> None
                | _ -> None
        | ABAJO ->
              match estado with
                | (0, x2, x3, x4, x5, x6, x7, x8, x9) -> Some (x4, x2, x3, 0, x5, x6, x7, x8, x9)
                | (x1, 0, x3, x4, x5, x6, x7, x8, x9) -> Some (x1, x5, x3, x4, 0, x6, x7, x8, x9)
                | (x1, x2, 0, x4, x5, x6, x7, x8, x9) -> Some (x1, x2, x6, x4, x5, 0, x7, x8, x9)
                | (x1, x2, x3, 0, x5, x6, x7, x8, x9) -> Some (x1, x2, x3, x7, x5, x6, 0, x8, x9)
                | (x1, x2, x3, x4, 0, x6, x7, x8, x9) -> Some (x1, x2, x3, x4, x8, x6, x7, 0, x9)
                | (x1, x2, x3, x4, x5, 0, x7, x8, x9) -> Some (x1, x2, x3, x4, x5, x9, x7, x8, 0)
                | _ -> None

let estado_inicial_14 = (0, 3, 6, 2, 1, 7, 5, 4, 8)
let estado_inicial_16 = (1, 3, 0, 7, 6, 4, 8, 5, 2)
let estado_inicial_18 = (0, 8, 5, 4, 2, 1, 7, 6, 3)
let estado_inicial_20 = (1, 3, 0, 7, 2, 5, 4, 6, 8)
let estado_inicial_23 = (5, 2, 4, 1, 3, 0, 6, 8, 7)

let prueba1 () = 
    let problema = 
        {inicio = estado_inicial_14
         meta = function | (1, 2, 3, 4, 5, 6, 7, 8, 0) -> true
                         | _ -> false
         sucesor = sucesor
         acciones = [ARRIBA; IZQUIERDA; DERECHA; ABAJO]
         costo = fun _ _ _ -> 1
        } : Busqueda.problema<estado, acciones>
    Busqueda.busqueda (BFS.estrategia_bfs1<estado,acciones>) problema

let prueba2 () = 
    let problema = 
        {inicio = estado_inicial_14
         meta = function | (1, 2, 3, 4, 5, 6, 7, 8, 0) -> true
                         | _ -> false
         sucesor = sucesor
         acciones = [ARRIBA; IZQUIERDA; DERECHA; ABAJO]
         costo = fun _ _ _ -> 1
        } : Busqueda.problema<estado, acciones>
    Busqueda.busqueda (BFS.estrategia_bfs2<estado,acciones>) problema
