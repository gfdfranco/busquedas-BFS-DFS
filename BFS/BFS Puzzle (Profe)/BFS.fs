module BFS
open System.Collections.Generic

// Estructuras de datos funcionales (no hay efectos secundarios)
let estrategia_bfs0<'s,'a> =
    {siguiente = Queue.dequeue
     agregar = Queue.enqueue
     inicializar = Queue.enqueue Queue.empty} : Busqueda.estrategia<'s,'a, Queue.queue<Busqueda.nodo<'s,'a>>>

// Estructuras de datos funcionales (no hay efectos secundarios)
// Recuerda estados visitados
let estrategia_bfs1<'s,'a when 's : comparison> =
    {siguiente = fun (estados, visitados) -> let (x, estados) = Queue.dequeue estados
                                             (x, (estados, visitados))
     agregar = fun (estados, visitados) n  ->
                    if Set.contains n.estado visitados
                    then (estados, visitados)
                    else (Queue.enqueue estados n, Set.add n.estado visitados)
     inicializar = fun n -> (Queue.enqueue Queue.empty n, set [n.estado])} : Busqueda.estrategia<'s,'a, Queue.queue<Busqueda.nodo<'s,'a>> * Set<'s>>


// Estructuras de datos imperativas (efectos secundarios)
// Recuerda estados visitados
let estrategia_bfs2<'s,'a> =
    let estados = new Queue<Busqueda.nodo<'s,'a>>()
    let visitados = new HashSet<'s>()
//    let visitados = new SortedSet<'s>()
    {siguiente = fun ()  -> try
                             (Some (estados.Dequeue ()), ())
                            with | :? System.InvalidOperationException -> (None, ())
     agregar = fun () n  -> if visitados.Contains n.estado
                            then ()
                            else estados.Enqueue n
                                 ignore (visitados.Add n.estado)
     inicializar = fun n -> estados.Enqueue n
                            ignore (visitados.Add n.estado)
                            ()} : Busqueda.estrategia<'s,'a, unit>

    
