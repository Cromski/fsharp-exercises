
type aExp =
    | N of int              // Integer value
    | V of string           // Variable
    | WL                    // Length of the word
    | PV of aExp            // Point value of character at specific word index
    | Add of aExp * aExp    // Addition
    | Sub of aExp * aExp    // Subtraction
    | Mul of aExp * aExp    // Multiplication


let (.+.) a b = Add (a, b)
let (.-.) a b = Sub (a, b)
let (.*.) a b = Mul (a, b)

let rec arithEvalSimple (aExp: aExp) : int =
    match aExp with
    | N i -> i
    | Add(a, b) -> arithEvalSimple a + arithEvalSimple b
    | Sub(a, b) -> arithEvalSimple a - arithEvalSimple b
    | Mul(a, b) -> arithEvalSimple a * arithEvalSimple b
    | _ -> -1

let rec arithEvalState (a: aExp) (s: Map<string, int>) : int =
    match a with
    | N i -> i
    | V v ->
        match Map.tryFind v s with
        | None -> 0
        | Some v -> v
    | Add(a, b) -> arithEvalState a s + arithEvalState b s
    | Sub(a, b) -> arithEvalState a s - arithEvalState b s
    | Mul(a, b) -> arithEvalState a s * arithEvalState b s
    | _ -> -1

type word = (char * int) list

let hello : word = ['H', 4; 'E', 1; 'L', 1; 'L', 1; 'O', 1]


let arithSingleLetterScore = PV (V "_pos_") .+. (V "_acc_")
let arithDoubleLetterScore = ((N 2) .*. PV (V "_pos_")) .+. (V "_acc_")
let arithTripleLetterScore = ((N 3) .*. PV (V "_pos_")) .+. (V "_acc_")
let arithDoubleWordScore = N 2 .*. V "_acc_"
let arithTripleWordScore = N 3 .*. V "_acc_"


let rec arithEval (a: aExp) (w: word) (s: Map<string, int>) : int =
    match a with
    | N i -> i
    | V v ->
        match Map.tryFind v s with
        | None -> 0
        | Some v -> v
    | WL -> w.Length
    | PV aExp   -> snd w.[(arithEval aExp w s)]
    | Add(a, b) -> arithEval a w s + arithEval b w s
    | Sub(a, b) -> arithEval a w s - arithEval b w s
    | Mul(a, b) -> arithEval a w s * arithEval b w s

type cExp =
    | C of char (* Character value *)
    | ToUpper of cExp (* Converts lower case to upper case character,non-letters are unchanged *)
    | ToLower of cExp (* Converts upper case to lower case character,non-letters are unchanged *)
    | CV of aExp (* Character lookup at word index *)
    
let rec charEval (c: cExp) (w: word) (s: Map<string, int>) : char =
    match c with
    | C c -> c
    | ToUpper cExp -> System.Char.ToUpper(charEval cExp w s)
    | ToLower cExp -> System.Char.ToLower(charEval cExp w s)
    | CV aExp -> fst w.[arithEval aExp w s]

type bExp =
    | TT (* true *)
    | FF (* false *)
    | AEq of aExp * aExp (* numeric equality *)
    | ALt of aExp * aExp (* numeric less than *)
    | Not of bExp (* boolean not *)
    | Conj of bExp * bExp (* boolean conjunction *)
    | IsDigit of cExp (* check for digit *)
    | IsLetter of cExp (* check for letter *)
    | IsVowel of cExp (* check for vowel *)

let (~~) b = Not b
let (.&&.) b1 b2 = Conj (b1, b2)
let (.||.) b1 b2 = ~~(~~b1 .&&. ~~b2) (* boolean disjunction *)

let (.=.) a b = AEq (a, b)
let (.<.) a b = ALt (a, b)
let (.<>.) a b = ~~(a .=. b) (* numeric inequality *)
let (.<=.) a b = a .<. b .||. ~~(a .<>. b) (* numeric less than or equal to *)
let (.>=.) a b = ~~(a .<. b) (* numeric greater than or equal to *)
let (.>.) a b = ~~(a .=. b) .&&. (a .>=. b) (* numeric greater than *)
//    | TT (* true *)
//    | FF (* false *)
//    | AEq of aExp * aExp (* numeric equality *)
//    | ALt of aExp * aExp (* numeric less than *)
//    | Not of bExp (* boolean not *)
//    | Conj of bExp * bExp (* boolean conjunction *)
//    | IsDigit of cExp (* check for digit *)
//    | IsLetter of cExp (* check for letter *)
//    | IsVowel of cExp (* check for vowel *)
let rec boolEval (b: bExp) (w: word) (s: Map<string, int>) : bool =
    match b with
    | TT -> true
    | FF -> false
    | AEq(aExp1, aExp2) -> (arithEval aExp1 w s) = (arithEval aExp2 w s)
    | ALt(aExp1, aExp2) -> (arithEval aExp1 w s) < (arithEval aExp2 w s)
    | Not bExp -> not(boolEval bExp w s)
    | Conj(bExp1, bExp2) -> boolEval bExp1 w s && boolEval bExp2 w s
    | IsDigit cExp -> System.Char.IsDigit(charEval cExp w s)
    | IsLetter cExp -> System.Char.IsLetter(charEval cExp w s)
    | IsVowel cExp -> ['a';'e';'i';'o';'u'] |> List.contains (charEval cExp w s)