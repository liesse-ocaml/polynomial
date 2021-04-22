(* integer power of a float *)
let pow x n =
  let rec pow_rec = function
    | 0 -> 1.0
    | n -> x *. (pow_rec (n-1))
  in
  if x = 0.0 && n = 0 then
    1.0
  else if x = 0.0 then
    0.0
  else
    pow_rec n

(* ceil to take care of rounding on float precision, found by trial and errors on
  random polynomials mainly for associativity of prod
*)
let ceil = 100000.*.Float.epsilon
(* rounding to 0.0 when smaller that the ceil *)
let round f = if Float.abs f > ceil then f else 0.0
(* is_zero upto the ceil *)
let is_zero f = (Float.abs f) <= ceil
  
(* Polynomial degrees *)
type degree = Inf | D of int 

let max_deg d1 d2 = match d1,d2 with
| Inf, o | o, Inf -> o
| D d1,D d2 -> D (max d1 d2)

let incr_deg = function
| Inf -> Inf
| D d -> D (d + 1)

let string_of_degree = function
| Inf -> "-∞"
| D d -> string_of_int d

let string_of_float = function
  | 1.0 -> ""
  | f -> Printf.sprintf "%g" f

let string_of_pow = function
  | 0 -> ""
  | 1 -> "x"
  | n -> Printf.sprintf "x^%i" n

let string_of_term to_string c n = match (c,n) with
  | (_,0) -> to_string c
  | (_,_) -> Printf.sprintf "%s%s" (to_string c) (string_of_pow n)