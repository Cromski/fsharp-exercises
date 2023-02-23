

let sqr (x: int) = x*x

let pow (x: float) (n: float) =  System.Math.Pow(x, n)

let rec sum (n: int) =
    match n with
    | x when x < 0 -> failwith "number is less than 0"
    | x when x = 0 -> x
    | x            -> sum (n-1) + x

let rec fib (n: int) =
    match n with
    | x when x = 0 -> 0
    | x when x = 1 -> 1
    | x            -> fib (x-1) + fib (x-2)
    
let dup (s: string) = s+s

let rec dupn (s: string) (n: int) =
    match n with
    | n when n < 1 -> ""
    | n when n = 1 -> s
    | n            -> s + dupn s (n-1)

let rec bin (n: int, k: int) = 
    match n,k with
    | n,k when k = 0 || n = k            -> 1
    | n,k when n <> 0 && k <> 0 && n > k -> bin (n-1, k-1) + bin (n-1, k)
    | _                                  -> failwith "inappropriate numbers"

let timediff (t1: int * int) (t2: int * int) : int =
    ((t2 |> fst) - (t1 |> fst)) * 60 + ((t2 |> snd) - (t1 |> snd))

let minutes (t: int * int) : int = timediff (0,0) t

let curry (f: 'a * 'b -> 'c) (x: 'a) (y: 'b) : 'c = f (x,y)
    
let uncurry (f: 'a -> 'b -> 'c) (x: 'a, y: 'b) : 'c = f x y

let empty (c:char, i:int) : (int -> char * int) = fun _ -> (c,i)

let add (newPos: int) (c:char, v:int) (word: int -> char * int) : (int -> char * int) =
    (fun pos -> if newPos = pos then (c,v) else word pos) 

let hello : (int -> char * int) =
    empty (char 0, 0) |> add 0 ('H', 4) |> add 1 ('E', 1) |> add 2 ('L', 1) |> add 3 ('L', 1) |> add 4 ('O', 1)
    
let singleLetterScore (word: int -> char * int) (pos: int) : int = word pos |> snd

let doubleLetterScore (word: int -> char * int) (pos: int) : int = (word pos |> snd) * 2
    
let trippleLetterScore (word: int -> char * int) (pos: int) : int = (word pos |> snd) * 3
