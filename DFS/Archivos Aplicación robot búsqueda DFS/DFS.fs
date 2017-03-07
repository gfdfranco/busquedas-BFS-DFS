module DFS
open System.Collections.Generic

// Estructuras de datos funcionales (no hay efectos secundarios)
let estrategia_dfs<'s,'a> =
    {siguiente = Stack.pop
     agregar = Stack.push
     inicializar = Stack.push Stack.empty} : Busqueda.estrategia<'s,'a, Stack.stack<Busqueda.nodo<'s,'a>>>