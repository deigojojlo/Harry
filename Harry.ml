module Dict = Map.Make(String)

exception BadEntry
exception NoValueFor of string 

type t =
  | Var of string
  | Const of string
  | Op of (t * t * t)
  | Add | Mul | Div | Sub

let rec eval dico (tt : t) = match tt with 
  | Const x -> Float.of_string x
  | Var x -> 
  begin
    match Dict.find_opt x dico with 
    | None -> raise (NoValueFor x )
    | Some y -> y 
  end 
  | Op (g,o,d) -> 
  begin 
    match o with 
    | Add -> eval dico g +. eval dico d  
    | Mul -> eval dico g *. eval dico d 
    | Sub -> eval dico g -. eval dico d 
    | Div -> eval dico g /. eval dico d 
    | _ -> 0.
  end 
  | _ -> 0.

(*check if it is a bad entry or not *)
let badEntry s = let p = ref 0 in 
 try (String.iter (fun c -> 
 if !p < 0 then raise BadEntry else 
 if c = '(' then p := !p + 1 else 
 if c = ')' then p := !p - 1 else 
 ()) s ; false )
 with BadEntry -> true

(*an aux function for from_string*)
let rec parse_expr str =
  let length = (String.length str) in 
  let buffer = Buffer.create length in 
  let _ = Buffer.add_string buffer str in 
  let var_or_const temp stack = 
    let temp = Buffer.contents temp in 
    if String.equal temp "" then Stack.pop stack
    else try Const (Float.to_string (Float.of_string temp)) with _ -> Var temp in
  let rec parcours i temp stack = 
    if i = length then Stack.pop stack else
    (begin 
      match Buffer.nth buffer i with  
      | '+' -> (Stack.push stack (var_or_const temp stack)) ; Stack.push stack Add ; Buffer.clear temp 
      | '-' -> (Stack.push stack (var_or_const temp stack)) ; Stack.push stack Sub ; Buffer.clear temp 
      | '*' -> (Stack.push stack (var_or_const temp stack)) ; Stack.push stack Mul ; Buffer.clear temp
      | '/' -> (Stack.push stack (var_or_const temp stack)) ; Stack.push stack Div ; Buffer.clear temp
      | '(' -> ()
      | ')' -> 
        begin 
          let right = var_or_const temp stack in
          let op = Stack.pop stack in 
          let left = Stack.pop stack  in 
          Stack.push stack (Op (left,op,right)) ; 
          Buffer.clear temp ;
        end 
      | x -> Buffer.add_char temp x
      end ;
      parcours (i+1) temp stack ;
      )
  in parcours 0 (Buffer.create length) (Stack.empty () )

(*from_string : string -> t *)
(*take a string with good syntax or raise badEntry *)
let from_string str =
  if badEntry str then raise BadEntry  else parse_expr str 

(*Made an associative list with the functor Map.Make more efficienty than assoc list *)
let createDict assocList = 
  let res = ref Dict.empty in
  (List.iter (fun (x,y) -> res := Dict.add x y !res) assocList ;
  !res; )

(*to_string : t -> string *)
let to_string t = 
  let rec auxToString t  =
    begin  
      match t with 
      | Var x -> x 
      | Const x -> x 
      | Op (g,op,d) -> 
      begin 
        let op = match op with 
        | Add -> "+" 
        | Sub -> "-" 
        | Div -> "/" 
        | Mul -> "*" 
        | _ -> ""
        in  
        let res =  "(" ^ auxToString g  ^ op ^ auxToString d  ^ ")" in 
        res 
      end 
      | _ -> ""
    end
  in auxToString t

let autoParenthesis entry = 
  (* auto place paratethesis by math priority at left*)
  (*var declaration *)
  (*b boolean stack for not need parenthesis*)
  (*s string stack of the actual expression*)
  let b = Stack.empty () in 
  let s = Stack.empty () in 
  let buffer = Buffer.create (10*String.length entry) in
  let addBuf c = Buffer.add_string buffer c in
  let clearBuf () = Buffer.clear buffer in 
  let pushS = Stack.push s in 
  let pushB = Stack.push b in
  let toS c = String.make 1 c in 
  let pushIfNotEmpty () = if String.length (Buffer.contents buffer) = 0 then () else pushS  (Buffer.contents buffer ) in
  (*give the string with the good number of ) *)
  let rec give_some_parenthesis () = if Stack.is_empty b then "" else 
    match Stack.pop b with 
    | true -> "" ^ give_some_parenthesis ()
    | false -> ")"  ^ give_some_parenthesis () 
  in
  let rec pull () = 
    if Stack.is_empty s 
    then "" 
    else let top  = Stack.pop s in 
    pull () ^ top  
  in
  (*I call finish when the expression is potentialy finished*)
  let finish c index entry = 
    if index = -1 + String.length entry then 
      let op = Stack.pop s in 
      let left = Stack.pop s in 
      let right = Buffer.contents buffer ^ toS c in 
      let final_parenthesis = give_some_parenthesis () in 
      pushB true ; 
      pushS (left ^ op ^ right ^ final_parenthesis )
    else 
      if entry.[index+1] = '(' then 
      (pushS ((Buffer.contents buffer) ^ toS c); 
      pushS "*"  ; 
      pushB false  ;  
      clearBuf () )
    else addBuf (toS c) 
  in
  (*my_function is the argument for a equivalent function of String.iter *)
  let my_function char index entry  = 
  match char with 
  | '(' -> pushB true  ; addBuf "("
  | ')' ->  Stack.pop b ; finish ')' index entry
  | '+' | '-' | '/' | '*'  -> 
  if not (Stack.is_empty b ) && not(Stack.peek b) 
  then let op = Stack.pop s in  (pushS ("(" ^ Stack.pop s ^ op  ^ Buffer.contents buffer ^ ")") ; clearBuf () )
  else pushB false ; pushIfNotEmpty ()  ; pushS (toS char) ; clearBuf () 
  | x -> finish x index entry
  in 
  (*my version of String.iter for this issue*)
  for k = 0 to -1+String.length entry do 
    my_function entry.[k] k entry ;
  done ;
  "(" ^ pull () ; 
  