module Poly : Interface.POLYNOMIAL with type var := float =
struct
  type term = float * int (* (a1,k1) is a term a x^k *)
  type t = term list (* a polynomial is a sorted list of terms, lower power before, every coefficient should not be null *)

  (* the constant polynomials *)
  let zero = []
  let one = [ (1.0, 0) ]

  (* term builder *)
  let term c n =
    if n < 0 then
      raise (Invalid_argument ((string_of_int n)^" should be positive"))
    else if c = 0.0 then
      []
    else
      [ (c,n) ]

  (* building a random polynomial of degree n, useful for tests *)
  let random n =
    if n < 0 then
      raise (Invalid_argument ((string_of_int n)^" should be positive"))
    else
      let result = ref [] in
      for i = n downto 0 do
        if Random.bool () then
          let sign = if Random.bool () then 1.0 else -1.0 in
          result := (sign *. (Random.float 20.0), i) :: !result
      done;
      !result

  (* a very naive implementation *)
  let eval p x = List.fold_left (fun acc (c,p) -> acc +. c *. Utils.pow x p) 0.0 p

  (* stringify a polynomial *)
  let to_string p =
    let string_of_term c i = Utils.string_of_term Utils.string_of_float c i in
    if p = zero then
      "0"
    else
      List.fold_left (fun acc (coef,pow) -> 
        if coef = 0.0 then
          acc
        else if acc = "" then
          string_of_term coef pow
        else if coef > 0.0 then
          acc^" + "^(string_of_term coef pow)
        else
          acc^" - "^(string_of_term (Float.abs coef) pow)
          ) "" p

  (* As our polynomial have a unique representation, the equality can be syntactic *)
  (* But we will need to take care of rounding *)
  let for_all2 pred p1 p2 =
    let rec for_all2_rec = function
    | ([],[]) -> true
    | ([],_) | (_,[]) -> false
    | ((_,i1)::_,(_,i2)::_) when i1 <> i2 -> false
    | ((c1,_)::t1,(c2,_)::t2) -> (pred c1 c2) && (for_all2_rec (t1,t2))
  in for_all2_rec (p1,p2)

  (* the equal itself *)
  let equal p1 p2 = for_all2 (fun f1 f2 -> Utils.is_zero (f1 -. f2)) p1 p2

  (* The degree of the polynomial, since the invariant the length give the degree+1 *)
  let deg = List.fold_left (fun acc (_,pow) -> Utils.max_deg acc (D pow)) Utils.Inf

  (* the sum of two polynomials *)
  let sum p1 p2 =
    let rec sum_rec = function
      | [], p | p, [] -> p
      | ((_, n1) as t)::p1, (((_, n2)::_) as p2) when n1 < n2 -> t :: (sum_rec (p1,p2))
      | (c1,n1)::p1, (c2,n2)::p2 when n1 = n2 ->
        begin
          (* whenever a coefficient is 0.0 it is not put in the list *)
          match Utils.round (c1+.c2) with
          | 0.0 -> sum_rec (p1,p2)
          | c -> (c,n1) :: (sum_rec (p1,p2))
        end
      | p1, t::p2 -> t :: (sum_rec (p1,p2))
    in
    sum_rec (p1,p2)

  (* symmetric of the polynomial w.r.t. sum *)
  let sym = List.map @@ fun (coef, pow) -> (-1.0*.coef, pow)

  (* the product of two polynomials *)
  let prod_term p c n =
    let rec prod_rec = function
      | [] -> []
      | (coef,pow)::rest -> (coef *. c,pow + n)::(prod_rec rest)
    in
    prod_rec p

  let prod p1 p2 =
    List.fold_left (fun acc p -> sum acc p) [] @@ List.map (fun (coef,pow) -> prod_term p2 coef pow) p1
end
