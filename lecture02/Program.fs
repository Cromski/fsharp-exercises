

let rec downto1 (n: int) : int list =
    if n > 0 then [n] @ downto1 (n-1) else []
    
let rec downto2 (n: int) : int list =
    match n with
    | n when n > 0 -> [n] @ downto1 (n-1)
    | _            -> []

let removeOddIdx (xs: 'a list) : 'a list =
    List.indexed xs
    |> List.fold (fun acc (k,v) -> if k % 2 = 0 then acc@[v] else acc ) [] 

let rec combinePair (xs: 'a list) : ('a * 'a) list =
    match xs with
    | x::y:: xss -> (x,y)::(combinePair xss)
    | _     -> []

type complex = float*float

let mkComplex (a: float) (b: float) : complex = (a,b) // c1 / c2 = c1 * 1/c2

let complexToPair (c: complex) : float * float = (fst c, snd c)
    
let (|+|) (c1: complex) (c2: complex) : complex = (fst c1 + fst c2, snd c1 + snd c2)
let (|*|) (c1: complex) (c2: complex) : complex = (fst c1 * fst c2 - snd c1 * snd c2, snd c1 * fst c2 + fst c1 * snd c2)
let (|-|) (c1: complex) (c2: complex) : complex = c1 |+| (-fst c2, -snd c2) //a - b = a + (-b)
let (|/|) (c1: complex) (c2: complex) : complex =
    c1 |*| (fst c2/(fst c2 ** 2.0 + snd c2 ** 2.0), - snd c2 / (fst c2 ** 2.0 + snd c2 ** 2.0))

let explode1 (s: string) : char list = s.ToCharArray() |> Array.toList
let rec explode2 (s: string) : char list =
    match s with
    | "" -> []
    | x  -> x.Chars 0 :: explode2 (s.Remove(0,1))
let implode (cs: char list) : string = List.foldBack (fun c acc -> (string c)+acc ) cs ""
let implodeRev (cs: char list) : string = List.fold (fun acc c -> (string c)+acc ) "" cs

let toUpper (s: string) : string = s |> explode1 |> List.map System.Char.ToUpper |> implode

let toUpper2 = explode1 >> List.map System.Char.ToUpper >> implode

let rec ack (m: int, n: int) : int =
    match m,n with
    | m,n when m = 0          -> n+1
    | m,n when m > 0 && n = 0 -> ack(m-1,1)
    | m,n when m > 0 && n > 0 -> ack(m-1, ack(m, n-1))
    | _                       -> -1
    
let time (f: unit -> 'a) =
    let start = System.DateTime.Now
    let res = f ()
    let finish = System.DateTime.Now
    (res, finish - start)
    
let timeArg1 (f: 'a -> 'b) (a: 'a) = time (fun () -> f a)
    
let rec downto3 (f: int -> 'a -> 'a) (n: int) (e: 'a) : 'a =
    match n with
    | n when n > 0  -> downto3 f (n - 1) (f n e)
    | n when n <= 0 -> e
    | _ -> failwith "xd"
    
let fac (i: int) : int =
    downto3 (*) i 1

let range (g: int -> 'a) (n: int) : 'a list =
    downto3 (fun x y -> g x::y) n []
    //List.init n (fun v -> g (v+1)) - way better tbh

type word = (char * int) list
type squareFun = word -> int -> int -> int

let hello : word = ['H', 4; 'E', 1; 'L', 1; 'L', 1; 'O', 1]


let singleLetterScore : squareFun =
    fun word pos acc -> snd word.[pos]+acc
    //snd word.[pos]+acc
let doubleLetterScore (word: word) (pos: int) (acc: int) : int = (snd word.[pos] * 2)+acc
let tripleLetterScore (word: word) (pos: int) (acc: int) : int = (snd word.[pos] * 3)+acc

let doubleWordScore (_: word) (_: int) (acc: int) : int = acc*2
let tripleWordScore (_: word) (_: int) (acc: int) : int = acc*3

let listOfVowels = ['a';'e';'i';'o';'u']

let oddConsonants (word: word) (_: int) (acc: int) : int =
    if (List.fold (fun acc (k,_) -> if not (List.contains k listOfVowels) then acc+1 else acc) 0 word) % 2 = 1
    then -acc
    else acc

type square = (int * squareFun) list




