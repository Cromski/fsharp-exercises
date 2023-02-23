module lecture04.MultiSet

    type MultiSet<'a> when 'a : comparison = Map<'a, uint32>
    
    let empty : MultiSet<'a> = Map.empty<'a, uint32>
    let isEmpty (s: MultiSet<'a>) : bool = s.IsEmpty
    let size (s: MultiSet<'a>) : uint32 = List.fold (fun acc v -> v+acc) 0u (s |> Map.toSeq |> Seq.map snd |> List.ofSeq)
    let contains (a: 'a) (s: MultiSet<'a>) : bool = Map.containsKey a s
    let numItems (a: 'a) (s: MultiSet<'a>) : uint32 = if contains a s then s.Item a else 0u
    let add (a: 'a) (n: uint32) (s: MultiSet<'a>) : MultiSet<'a> =
        match s.TryFind a with
        | None -> s.Add(a, n)
        | Some _ -> s.Add(a, s.Item(a)+n)
    let addSingle (a: 'a) (s: MultiSet<'a>) : MultiSet<'a> = add a 1u s
    let remove (a: 'a) (n: uint32) (s: MultiSet<'a>) : MultiSet<'a> =
        match s.TryFind a with
        | None -> s
        | Some _ -> if s.Item(a) > n then s.Add(a, s.Item(a)-n) else s.Remove(a)
    let removeSingle (a: 'a) (s: MultiSet<'a>) : MultiSet<'a> = remove a 1u s
    let fold (f: 'a -> 'b -> uint32 -> 'a) (acc: 'a) (s: MultiSet<'b>) : 'a = Map.fold f acc s
    let foldBack (f: 'a -> uint32 -> 'b -> 'b) (s: MultiSet<'a>) (acc: 'b) : 'b = Map.foldBack f s acc
    let ofList (lst: 'a list) : MultiSet<'a> =
        let rec aux acc l =
            match l with
            | [] -> acc
            | x :: xs -> aux (addSingle x acc) xs
        aux empty lst
    let toList (s: MultiSet<'a>) : 'a list =
        List.fold (fun acc (k,v) -> acc@List.init v (fun _ -> k)) [] (s |> Map.toSeq |> Seq.map (fun (k,v) -> (k,int v)) |> List.ofSeq)
        