(*
Honor code comes here:

First Name: Jiaye 
Last Name: Liu
BU ID: U36376979

First Name: Yijia 
Last Name: Liu
BU ID: U40694719

I pledge that this program represents my own
program code and that I have coded on my own. I received
help from no one in designing and debugging my program.
I have read the course syllabus of CS 320 and have read the sections on Collaboration
and Academic Misconduct. I also understand that I may be asked to meet the instructor
or the TF for a follow up interview on Zoom. I may be asked to explain my solution in person and
may also ask you to solve a related problem.
*)



open Printf

let explode s =
  List.of_seq (String.to_seq s)

let implode ls =
  String.of_seq (List.to_seq ls)



let interpreter (s : string) : string list * int = failwith "undefined"


let readlines (file : string) : string =
  let fp = open_in file in
  let rec loop () =
    match input_line fp with
    | s -> s ^ (loop ())
    | exception End_of_file -> ""
  in
  let res = loop () in
  let () = close_in fp in
  res

let runfile (file : string) : string list * int =
  let s = readlines file in
  interpreter s


(* 1. define parser and basic functions*)
type 'a parser = Parser of (string -> ('a * string ) list)

let parser p inp=
  match p with 
    Parser f -> f inp

let return (a: 'a): 'a parser  = 
  Parser (fun inp -> [a, inp])

let fail: 'a parser = Parser (fun inp -> [])

(*choice parser*)
let (<|>) (p: 'a parser) (q: 'a parser): 'a parser =
  Parser (
    fun inp ->
      match parser p inp with 
        []->parser q inp
      |
        d->d
  )

(*binding operator*)
let (>>=) (p: 'a parser) (f: 'a -> 'b parser): 'b parser =
  Parser (
    fun inp ->
      match parser p inp with 
        []->[]
      |
        (v,out)::_->  parser (f v) out 
  )

let read: char parser =
  Parser(
    fun cs ->
      match explode cs with
      | c :: cs -> [(c, implode cs)]
      | [] -> []
  )
let (let*) = (>>=)

let rec readn (n:int):string parser= 
  if n>0 then 
    let* c = read in 
    let* cs = readn(n-1) in 
    return (String.make 1 c^cs)
  else 
    return ""

(*charP 返回string头*)
let charP  = 
  Parser (
    fun s ->
      match (explode s) with 
        []->[]
      |
        h::rest->[(h,implode rest)]
  )

(*
  charP 的进阶，对比char 和string头，返回一致
   *)
let satcP (c:char)= 
  charP >>= fun x->
  if x = c then return c 
  else fail

(* 
satcP的进阶，对比string和string，返回一致
*)
let satsP s = 
  if s = "" then fail else
    let rec asats s = 
      match (explode s) with 
        h::rest -> satcP h >>= fun _ -> asats (implode rest)
      |
        []-> return([])
    in 
    asats s
(*run parser p many times OR 0 time *)
let rec many0 p =
  (p >>= fun a -> 
   many0 p >>= fun b-> 
   return (a::b))
  <|>
  return []               (*base case: until empty list*)


(* run parser p many OR at least 1 times*)
let rec many1 p =
  p >>= fun a -> 
  many0 p >>= fun b-> 
  return (a::b)

(* 2. define value and command*)
type const = Int of int
           | Bool of bool
           | Str of string
           | Unit of string
           | Name of string

type command = Push of const| Pop | Log | Swap | Add | Sub | Mul | Div | Rem | Neg   (* part 1 *)
             | Cat | And | Or | Not | Eq | Lte | Lt | Gte | Gt | Let | Ask | Begin_End of command list 
             | IfElse of (command list * command list) (*part 2 *)

type env = (string * const) list            


(* 3. define basic functions to determine the type of a string*)

let is_digit = function 
  '0' .. '9' -> true | _ -> false
let is_alpha = function 
  'a' .. 'z' | 'A' .. 'Z' -> true | _ -> false

(* 4. parsers for values*)

(*用satcP对比 char数字 和 string第一个数字，返回一致*)
let digitP = 
  satcP '0' <|> satcP '1' <|> satcP '2' <|> satcP '3' <|> satcP '4' <|> satcP '5' <|> satcP '6'<|> satcP '7' <|> satcP '8' <|> satcP '9'

let whitespaceP = 
  satcP ' ' <|> satcP '\t' <|> satcP '\n'

let symP = 
  satcP '`' <|> satcP '~' <|> satcP '!' <|> satcP '@' <|> satcP '#' <|> satcP '$' <|> satcP '%' <|> satcP '^' <|> satcP '&' <|> satcP '*' <|> satcP '(' <|> satcP ')' <|> satcP '-' <|> satcP '_' <|> satcP '+' <|> satcP '=' <|> satcP '[' <|> satcP ']' <|> satcP '{' <|> satcP '}' <|> satcP '|' <|> satcP ':' <|> satcP ';' <|> satcP '<' <|> satcP '>' <|> satcP ',' <|> satcP '.' <|> satcP '?' <|> satcP '/'

let natP = 
  (many1 digitP >>= fun a->               (*at least one digit->many1*)
   return (int_of_string (implode a)))    (*convert list of chars into integer*)

(*natP的进阶版，包含负数*)
let integerP : const parser = 
  (natP >>= fun x -> return (Int x))                                    (*正数*)
  <|>
  (satcP '-' >>= fun _->                   (*负数：第一个char是-*)
   natP >>= fun x -> 
   return (Int (-x)))

(* string parser *)
let strP (str:string): string parser =
  let len = String.length str in 
  readn len >>= fun x ->
  if str = x then return x
  else fail

let boolP : const parser =
  (strP "<true>" >>= fun s ->
   return (Bool true))
  <|> 
  (strP "<false>" >>= fun s ->
   return (Bool false))

let unitP : const parser =
  strP "<unit>" >>= fun s -> 
  return (Unit "<unit>")

let alphaP = read >>= fun x -> 
  if is_alpha x then return x 
  else fail

let stringP : const parser =
  (satcP '"' >>= fun _ -> 
   many0 (digitP <|> alphaP <|> whitespaceP <|> symP) >>= fun x -> 
   satcP '"' >>= fun _ -> 
   return (Str (implode (x))))

let nameP : const parser =
  (many1 (alphaP) >>= fun x -> 
   many0 (alphaP <|> digitP <|> satcP '_' <|> satcP '`') >>= fun y -> 
   return (Name (implode (x@y))))

let constP : const parser = integerP <|> boolP <|> unitP <|> stringP <|> nameP


(* 5. parsers for programs*)


(*part 1 *)
let addP =
  satsP "Add">>= fun _->
  return (Add)

let subP =
  satsP "Sub" >>= fun _->
  return (Sub)

let mulP=
  satsP "Mul" >>= fun _->
  return (Mul)

let divP=
  satsP "Div" >>= fun _->
  return (Div)

let remP=
  satsP "Rem" >>= fun _->
  return (Rem)

let negP=
  satsP "Neg" >>= fun _->
  return (Neg)

let logP=
  satsP "Log" >>= fun _->
  return (Log)

let popP=
  satsP "Pop" >>= fun _->
  return (Pop)

let swapP=
  satsP "Swap" >>= fun _->
  return (Swap)

let pushP = 
  (many0 whitespaceP >>= fun _-> 
   satsP "Push" >>= fun _-> 
   many1 whitespaceP >>= fun _->
   constP >>= fun x ->
   return (Push x))

(*part 2 *)
let catP =
  satsP "Cat" >>= fun _->
  return (Cat)

let andP=
  satsP "And" >>= fun _->
  return (And)

let orP=
  satsP "Or" >>= fun _->
  return (Or)

let notP=
  satsP "Not" >>= fun _->
  return (Not)

let eqP=
  satsP "Eq" >>= fun _->
  return (Eq)

let lteP=
  satsP "Lte" >>= fun _->
  return (Lte)

let ltP=
  satsP "Lt" >>= fun _->
  return (Lt)

let gteP=
  satsP "Gte" >>= fun _->
  return (Gte)

let gtP=
  satsP "Gt" >>= fun _->
  return (Gt)

let letP=
  satsP "Let" >>= fun _->
  return(Let)

let askP =
  satsP "Ask" >>= fun _->
  return (Ask)

(* if then 待补充，可能需要recursion *)


(*看到一个command
  let commandP =
  pushP <|> swapP <|> popP <|> logP <|> addP <|> subP <|> mulP <|> divP <|> remP <|> negP <|> catP <|> andP <|> orP <|> notP <|> eqP <|> lteP <|> ltP <|> gteP <|> gtP <|> letP <|> askP <|> 
*)

(*处理一个command里的；*)
let rec command_divP ()=
  (pushP <|> swapP <|> popP <|> logP <|> addP <|> subP <|> mulP <|> divP <|> remP <|> negP <|> catP <|> andP <|> orP <|> notP <|> eqP <|> lteP <|> ltP <|> gteP <|> gtP <|> letP <|> askP <|> begin_end () <|> if_elseP ()) >>= fun x ->
  many0 whitespaceP >>= fun _->
  satcP ';' >>= fun _->
  many0 whitespaceP >>= fun _->
  return (x)

and begin_end ()=
  satsP "Begin" >>= fun _->
  many0 whitespaceP >>= fun _->
  many1 (command_divP ()) >>= fun x->
  satsP "End" >>= fun _->
  return (Begin_End x)
and if_elseP () =
  satsP "If" >>= fun _->
  many0 whitespaceP >>= fun _->
  many0 (command_divP () ) >>= fun x ->
  satsP "Else" >>= fun _->
  many0 whitespaceP >>= fun _->
  many0 (command_divP ()) >>= fun y ->
  satsP "End" >>= fun _->
  many0 whitespaceP >>= fun _->
  return (IfElse (x,y))

let rec begin_endP () =
  satsP "Begin" >>= fun _->
  many0 whitespaceP >>= fun _->
  many1 (command_divP () <|> begin_endP ()) >>= fun x ->
  satsP "End" >>= fun _->
  many0 whitespaceP >>= fun _->
  return (Begin_End x)

and if_elseP () =
  satsP "If" >>= fun _->
  many0 whitespaceP >>= fun _->
  many0 (command_divP () <|> begin_endP () <|> if_elseP ()) >>= fun x ->
  satsP "Else" >>= fun _->
  many0 whitespaceP >>= fun _->
  many0 (command_divP () <|> begin_endP () <|> if_elseP ()) >>= fun y ->
  satsP "End" >>= fun _->
  many0 whitespaceP >>= fun _->
  return (IfElse (x,y))

(*处理一个command string*)
let commandsP =
  many1 (command_divP ())


(*6 execution for parser 
*)

let log x =
  match x with
    Int x -> string_of_int x
  |Bool s -> sprintf "<%b>" s
  |Unit s -> s
  |Str s -> "\""^s^"\""  (*\*)
  |Name s -> sprintf "%s" s

let is_string x=
  match x with
    Str s-> true
  |_->false

let lookup (e : env) (name : string) : const option =
  List.assoc_opt name e

let put (e : env) (name : string) (v : const): env =
  (name,v) :: e

let rec exe (coms : command list) (stack : const list) (env : env) (*string list  int *)=
  match (coms,stack) with
    ([],_)->([],0)     (*no error code:0*)
  (* part 1 *)
  |(Push v::rest, stk) -> exe rest (v::stk) env
  |(Pop::rest, _::stk) -> exe rest stk env
  |(Log::rest, head::stk) -> let (os,ec) = exe rest stk env in (log head::os, ec)
  |(Swap::rest, x::y::stk) -> exe rest (y::x::stk) env
  |(Add::rest, x::y::stk) -> (match (x,y) with
        (Int x, Int y) -> exe rest (Int(x+y)::stk) env
      |_->([],1) (*type error: 1*))
  |(Sub::rest, x::y::stk) -> (match (x,y) with
        (Int x, Int y) -> exe rest (Int(x-y)::stk) env
      |_->([],1) (*type error: 1*))
  |(Mul::rest, x::y::stk) -> (match (x,y) with
        (Int x, Int y) -> exe rest (Int(x*y)::stk) env
      |_->([],1) (*type error: 1*))
  |(Div::rest, x::y::stk) -> (match (x,y) with 
        (Int x, Int y) -> if y=0 then ([],3)
        else exe rest (Int(x/y)::stk) env
      |_->([],1))    
  |(Rem::rest, x::y::stk) -> (match (x,y) with
        (Int x, Int y) -> if y=0 then ([],3)
        else exe rest (Int(x mod y)::stk) env
      |_->([],1)) 
  |(Neg::rest, x::stk) -> (match x with
        Int x-> exe rest (Int(-x)::stk) env
      |_->([],1)  ) 
  (* part 2 *)
  |(Cat::rest, x::y::stk)-> (match (x,y) with 
        (Str x, Str y) -> exe rest (Str(x^y)::stk) env
      |_-> ([],1) )
  |(And::rest,x::y::stk) -> (match (x,y) with
        (Bool x, Bool y) -> exe rest (Bool(x&&y)::stk) env
      |_-> ([],1) ) 
  |(Or::rest, x::y::stk) -> (match (x,y) with
        (Bool x, Bool y) -> exe rest (Bool(x||y)::stk) env   
      |_->([],1) )   
  |(Not::rest, x::stk) -> (match x with
        Bool x -> if x = true then exe rest (Bool(false)::stk) env
        else exe rest (Bool(true)::stk) env
      |_-> ([],1))
  |(Eq::rest, x::y::stk) -> (match (x,y) with 
        (Int x, Int y)-> if x=y then exe rest (Bool(true)::stk) env
        else exe rest (Bool(false)::stk) env
      |_->([],1))
  |(Lte::rest, x::y::stk) -> (match (x,y) with
        (Int x, Int y) -> if x<=y then exe rest (Bool(true)::stk) env
        else exe rest (Bool(false)::stk) env
      |_->([],1))                  
  |(Lt::rest, x::y::stk) -> (match (x,y) with 
        (Int x, Int y) -> if x<y then exe rest (Bool(true)::stk) env
        else exe rest (Bool(false)::stk) env
      |_->([],1))                 
  |(Gte::rest,x::y::stk) -> (match (x,y) with 
        (Int x, Int y) -> if x>=y then exe rest (Bool(true)::stk) env
        else exe rest (Bool(false)::stk) env
      |_->([],1))                    
  |(Gt::rest, x::y::stk) -> (match (x,y) with 
        (Int x, Int y) -> if x>y then exe rest (Bool(true)::stk) env
        else exe rest (Bool(false)::stk) env
      |_->([],1))  
  |(Let::rest, x::y::stk) -> (match (x,y) with 
        (Name x, y) -> exe rest stk (put env x y)
      |_->([],1)) 
  |(Ask::rest, x::stk) -> (match x with
        Name x -> (match lookup env x with
            Some n -> exe rest (n::stk) env
          |None -> ([],4))
      |_->([],1)) 
  |((Begin_End begin_command)::rest, stk) -> (match begin_end_env begin_command [] env with
        (result,x::rest_stk,error) -> exe rest (x::stk) env
      |_->([],1)) 
  (*|(IfElse (x,y)::rest, stk) -> (match (x,y) with
    (Bool ture)::stk -> exe (x@rest) stk env 
    |(Bool false)::stk -> exe (y@rest) stk env
    |_->([],1)) *)
  |_->([],2)

and begin_end_env coms stack env = 
  match exe coms stack env with
    (result,error)->(result, stack, error)






(*7.interpreter*)
let interpreter (s : string) : string list * int = 
  exe (fst (List.hd(parser commandsP s))) [] []


let readlines (file : string) : string =
  let fp = open_in file in
  let rec loop () =
    match input_line fp with
    | s -> s ^ (loop ())
    | exception End_of_file -> ""
  in
  let res = loop () in
  let () = close_in fp in
  res

let runfile (file : string) : string list * int =
  let s = readlines file in
  interpreter s




