[<EntryPoint>]
let main args =
    let solucion = match Puzzle.prueba1 () with
                    | Some solucion -> solucion |> Busqueda.acciones
                                                |> Some
                    | None -> None
    printfn "%A" solucion
    0
