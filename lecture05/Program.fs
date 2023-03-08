

let fibA (i: int) : int =
    let rec aux acc1 acc2 j =
        match j with
        | j when j < i -> aux acc1 acc2 (j+1)
        | j when j = 0 -> max acc1 acc2
    aux 0 1 0