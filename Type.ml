type op = | Add | Mul | Div | Sub
type t =
  | Var of string
  | Const of string
  | Op of (t * op * t)