module Dict = Map.Make(String)
exception BadEntry
exception NoValueFor of string 
let rec eval dico (tt : Type.t) = match tt with 
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
  end 
let badEntry s = let p = ref 0 in 
 try (String.iter (fun c -> 
 if !p < 0 then raise BadEntry else 
 if c = '(' then p := !p + 1 else 
 if c = ')' then p := !p - 1 else 
 ()) s ; false )
 with BadEntry -> true
let rec parse_expr str =
  (* Supprime les espaces *)
  let length = (String.length str) in 
  let buffer = Buffer.create length in 
  let _ = Buffer.add_string buffer str in 
  let var_or_const temp stack = 
    let temp = Buffer.contents temp in 
    if String.equal temp "" then 
    match Stack.pop stack with 
    | Stack.Ready x -> x | _  -> Type.Const "0"  
    else try Type.Const (Float.to_string (Float.of_string temp)) with _ -> Type.Var temp in
  let rec parcours i temp stack = 
    print_endline (Buffer.contents temp);
    if i = length then Stack.pop stack else
    (begin 
      match Buffer.nth buffer i with  
      | '+' -> (Stack.push stack (Stack.Ready (var_or_const temp stack)) ; Stack.push stack (Stack.ReadyOp Add) ; Buffer.clear temp )
      | '-' -> (Stack.push stack (Stack.Ready (var_or_const temp stack)) ; Stack.push stack (Stack.ReadyOp Sub) ; Buffer.clear temp )
      | '*' -> (Stack.push stack (Stack.Ready (var_or_const temp stack)) ; Stack.push stack (Stack.ReadyOp Mul) ; Buffer.clear temp )
      | '/' -> (Stack.push stack (Stack.Ready (var_or_const temp stack)) ; Stack.push stack (Stack.ReadyOp Div) ; Buffer.clear temp )
      | '(' -> ()
      | ')' -> 
        begin 
          let right = var_or_const temp stack in
          let op = match Stack.pop stack with 
          | ReadyOp x -> x | _ -> Type.Add in 
          let left = match Stack.pop stack with 
          | Ready x -> x | _ -> Type.Const "0" in 
          Stack.push stack (Stack.Ready ( Op (left,op,right))) ; 
          Buffer.clear temp ;
        end 
      | x -> Buffer.add_char temp x
      end ;
      parcours (i+1) temp stack ;
      )
  in parcours 0 (Buffer.create length) (Stack.empty () )
let from_string str =
  if badEntry str then raise BadEntry  else 
  match parse_expr str with Ready x -> x | _ -> Type.Const "0"
let createDict assocList = 
  let res = ref Dict.empty in
  (List.iter (fun (x,y) -> res := Dict.add x y !res) assocList ;
  !res; )
let to_string t = 
  let rec auxToString t  =
    begin  
      match t with 
      | Type.Var x -> x 
      | Type.Const x -> x 
      | Type.Op (g,op,d) -> 
      begin 
        let op = match op with 
        | Add -> "+" 
        | Sub -> "-" 
        | Div -> "/" 
        | Mul -> "*" 
        in  
        let res =  "(" ^ auxToString g  ^ op ^ auxToString d  ^ ")" in 
        res 
      end 
    end
  in auxToString t  