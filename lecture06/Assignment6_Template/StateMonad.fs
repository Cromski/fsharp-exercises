﻿module StateMonad

    type Error = 
        | VarExists of string
        | VarNotFound of string
        | IndexOutOfBounds of int
        | DivisionByZero 
        | ReservedName of string           

    type Result<'a, 'b>  =
        | Success of 'a
        | Failure of 'b

    type State = { vars     : Map<string, int> list
                   word     : (char * int) list 
                   reserved : Set<string> }

    type SM<'a> = S of (State -> Result<'a * State, Error>)

    let mkState lst word reserved = 
           { vars = [Map.ofList lst];
             word = word;
             reserved = Set.ofList reserved }

    let evalSM (s : State) (S a : SM<'a>) : Result<'a, Error> =
        match a s with
        | Success (result, _) -> Success result
        | Failure error -> Failure error

    let bind (f : 'a -> SM<'b>) (S a : SM<'a>) : SM<'b> =
        S (fun s ->
              match a s with
              | Success (b, s') -> 
                match f b with 
                | S g -> g s'
              | Failure err     -> Failure err)

    let ret (v : 'a) : SM<'a> = S (fun s -> Success (v, s))
    let fail err     : SM<'a> = S (fun s -> Failure err)

    let (>>=)  x f = bind f x
    let (>>>=) x f = x >>= (fun () -> f)

    let push : SM<unit> = 
        S (fun s -> Success ((), {s with vars = Map.empty :: s.vars}))

    let pop : SM<unit> =
        S (fun s -> Success ((), {s with vars = s.vars.Tail}))

    let wordLength : SM<int> = S (fun s -> Success (s.word.Length, s))    

    let characterValue (pos : int) : SM<char> =
        S (fun s ->
            match List.tryItem pos s.word with
            | Some v -> Success (fst v, s)
            | None -> Failure (IndexOutOfBounds pos)
            )
    let pointValue (pos : int) : SM<int> =
        S (fun s ->
            match List.tryItem pos s.word with
            | Some v -> Success (snd v, s)
            | None -> Failure (IndexOutOfBounds pos)
            )
    let lookup (x : string) : SM<int> = 
        let rec aux =
            function
            | []      -> None
            | m :: ms -> 
                match Map.tryFind x m with
                | Some v -> Some v
                | None   -> aux ms

        S (fun s -> 
              match aux (s.vars) with
              | Some v -> Success (v, s)
              | None   -> Failure (VarNotFound x))

    let declare (var : string) : SM<unit> =
        S (fun s ->
           match s with
           | s when Set.contains var s.reserved -> Failure (ReservedName var)
           | s when Map.containsKey var s.vars.Head -> Failure (VarExists var)
           | _ -> Success ((), {s with vars = s.vars.Head.Add(var,0) :: s.vars.Tail}) 
           )
        
    let update (var : string) (value : int) : SM<unit> =
        let rec aux =
            function
            | [] -> None
            | x :: xs ->
                match Map.tryFind var x with
                | Some _ -> Some (x.Add(var,value) :: xs)
                | None -> aux xs
        
        S (fun s ->
              match aux s.vars with
              | Some listOfMaps -> Success ((), {s with vars = listOfMaps})
              | None -> Failure (VarNotFound var)
              )
            
              

    