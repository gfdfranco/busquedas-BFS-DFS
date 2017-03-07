[<EntryPoint>]
let main args =
    let solucion = match Robot.explorar () with
                    | Some solucion -> solucion |> Busqueda.acciones
                                                |> Some
                    | None -> None
    printfn "%A" solucion
    0
