type t 
exception BadEntry 
exception NoValueFor of string 
val from_string : string -> t 
val to_string : t -> string 
val eval : float Map.Make(String).t -> t -> float 
val createDict : (string * float) list -> float Map.Make(String).t
val autoParenthesis : string -> string 