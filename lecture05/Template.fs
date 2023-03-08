module Assignment5o

type word = (char * int) list

type aExp =
    | N of int              (* Integer literal *)
    | V of string           (* Variable reference *)
    | WL                    (* Word length *)
    | PV of aExp            (* Point value lookup at word index *)
    | Add of aExp * aExp    (* Addition *)
    | Sub of aExp * aExp    (* Subtraction *)
    | Mul of aExp * aExp    (* Multiplication *)
    | CharToInt of cExp     (* NEW: Cast to integer *)

and cExp =
   | C  of char             (* Character literal *)
   | CV of aExp             (* Character lookup at word index *)
   | ToUpper of cExp        (* Convert character to upper case *)
   | ToLower of cExp        (* Convert character to lower case *)
   | IntToChar of aExp      (* NEW: Cast to character *)


(* Exercise 5.1 *)

let sum (m:int) (n:int) : int =
    let rec aux m n cont =
        match n with
        | n when n = 0 -> cont m
        | _            -> aux m (n-1) (fun r -> r + m + n |> cont)
    aux m n id
    

(* Exercise 5.2 *)
let length (lst: 'a list) : int =
    let rec aux l cont =
        match l with
        | []      -> cont 0
        | _ :: xs -> aux xs (fun r -> cont ( 1 + r ))
    aux lst id

(* Exercise 5.3 *)

let foldBack (f: 'a -> 'b -> 'b) (lst: 'a list) (acc: 'b) : 'b =
    let rec aux l cont =
        match l with
        | [] -> cont acc
        | x :: xs -> aux xs (fun r -> cont (f x r))
    aux lst id

(* Exercise 5.4 *)

let factA x =
    let rec aux acc =
        function
        | 0 -> acc
        | x -> aux (x * acc) (x - 1)

    aux 1 x

let factC (x: int) : int =
    let rec aux j cont =
        match j with
        | 0 -> cont 1
        | j -> aux (j-1) (fun r -> cont (r * j))
    aux x id

(* TODO: *)
(* Compare the running time between factA and factC. Which solution is faster and why? 
   <Your answer goes here>
*)

(* Exercise 5.5 *)

let fibW x =
    let mutable res1 = 0
    let mutable res2 = 1
    let mutable i = 1
    while (i <= x) do
        let temp = res1
        res1 <- res2
        res2 <- temp + res2
        i <- i + 1
    res1

let fibA (x: int) : int =
    let rec aux acc1 acc2 j =
        match j with
        | 0 -> acc1
        | 1 -> acc2
        | j -> aux acc2 (acc1+acc2) (j-1)
    aux 0 1 x
    
let fibC (x: int) : int =
    let rec aux x cont =
        match x with
        | 0 -> cont 0
        | 1 -> cont 1
        | x -> aux (x-1) (fun r -> aux (x-2) (fun r2 -> cont (r+r2) ) )
    aux x id

(* TODO: *)
(* Compare the running time of fibW, fibA and fibC
   <Your answer goes here>

*)

(* Exercise 5.6 *)

let rec bigListK c =
    function
    | 0 -> c []
    | n -> bigListK (fun r -> c(1 :: r)) (n - 1)

(* TODO *)
(* The call bigListK id 130000 causes a stack overflow. 
   Analyse the problem and describe exactly why this happens. 
   Why is this not an iterative function?

   <Your answer goes here>
*)

(* Exercise 5.7 *)

let arithEvalTail a w s cont = failwith "not implemented"

let charEvalTail c w s cont = failwith "not implemented"

let arithEval a w s = arithEvalTail a w s id
let charEval c w s  = charEvalTail c w s id