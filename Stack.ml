type content = 
  | Construction of string 
  | Ready of Type.t
  | ReadyOp of Type.op
type t = (content list) ref 
let empty () = ref [] 
let push t x = t :=  x :: !t 
let pop t = match !t with 
| h :: tail -> t := tail ; h  
| [] -> raise Not_found
let peek t = match !t with 
| h :: t -> h  
| [] -> raise Not_found
let is_empty t = match !t with 
| None -> true 
| _ -> false
