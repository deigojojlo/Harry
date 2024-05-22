type 'a t = ('a list) ref 
let empty () = ref [] 
let push (t:'a t ) (x : 'a) : unit = t :=  x :: !t 
let pop t = match !t with 
| h :: tail -> t := tail ; h  
| [] -> raise Not_found
let peek t = match !t with 
| h :: t -> h  
| [] -> raise Not_found
let is_empty t = match !t with 
| [] -> true 
| _ -> false
let size t = List.length !t
