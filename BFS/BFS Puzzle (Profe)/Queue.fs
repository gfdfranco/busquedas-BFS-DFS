module Queue

type 'a queue = 'a list * 'a list

let empty = ([], [])

let enqueue (frente, atras) x = 
    (frente, x :: atras)

let rec dequeue (frente, atras) =
    match frente with
        | x :: frente -> (Some x, (frente, atras))
        | [] -> match atras with
                    | [] -> (None, ([], []))
                    | _  -> dequeue (List.rev atras, [])