module Busqueda

type nodo<'s,'a> = 
    {estado      : 's
     padre       : nodo<'s,'a> option
     profundidad : int
     accion      : 'a option
     costo_ruta  : int
    }
    
type problema<'s,'a> =
    {inicio   : 's
     meta     : 's -> bool
     sucesor  : 's -> 'a -> 's option
     acciones : 'a list
     costo    : 's -> 'a -> 's -> int
    }
    
type estrategia<'s,'a,'d> =
    {siguiente   : 'd -> nodo<'s,'a> option * 'd
     agregar     : 'd -> nodo<'s,'a> -> 'd
     inicializar : nodo<'s,'a> -> 'd
    }

let nodo_hijo problema padre accion =
    match problema.sucesor padre.estado accion with
        | Some estado -> 
            Some {estado = estado
                  padre = Some padre
                  profundidad = padre.profundidad + 1
                  accion = Some accion
                  costo_ruta = padre.costo_ruta + problema.costo padre.estado accion estado
                 }
        | None -> None
    
let busqueda estrategia problema =
    let nodo_inicio = 
        {estado = problema.inicio
         padre = None
         profundidad = 0
         accion = None
         costo_ruta = 0}
    let estados = estrategia.inicializar nodo_inicio
    let rec busqueda_aux estados =
        match estrategia.siguiente estados with
            | (Some s, estados) -> 
                        if problema.meta s.estado
                        then Some s
                        else problema.acciones
                                |> List.choose (nodo_hijo problema s)
                                |> List.fold estrategia.agregar estados
                                |> busqueda_aux
            | (None, estados) -> None
    busqueda_aux estados
    
let rec acciones nodo =
    match nodo.padre with
        | Some padre -> acciones padre @ [Option.get nodo.accion]
        | None       -> []
