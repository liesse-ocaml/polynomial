module Poly (T: Interface.DOMAIN) : Interface.POLYNOMIAL with type var := T.t =
struct
  type t =  (* Poly(a1,Poly(a2,...Const(an)...) is a1 + x (a2 + x (... , an x)...) *)
  | Const of T.t
  | Poly of T.t * t (* = c + x p *)

  (* the constant polynomials *)
  let zero = Const T.zero
  let one = Const T.one

  (* a general map function on polynomials *)
  let map f p =
    let rec map_rec = function
    | Const c -> Const (f c)
    | Poly(c,p) -> Poly(f c,map_rec p)
    in map_rec p

  (* symmetric of the polynomial w.r.t. sum *)
  let sym p = map (fun c -> T.sym c) p

  (* evaluation is naturally better here *)
  let rec eval p x = match p with
  | Const c -> c
  | Poly(c,p) -> T.sum c @@ T.prod x @@ eval p x

  (* The degree of the polynomial, since the invariant the length give the degree+1 *)
  let rec deg = function
  | Const c when T.equal c T.zero -> Utils.Inf
  | Const _ -> D 0
  | Poly(_,p) -> Utils.incr_deg (deg p)

  (* stringify a polynomial *)
  let sum p1 p2 =
    let rec sum_rec = function
    | (Const c1, Const c2) -> Const(T.sum c1 c2)
    | (Const c1, Poly(c2,p)) | (Poly(c1,p), Const c2) -> Poly(T.sum c1 c2,p)
    | (Poly(c1,p1), Poly(c2,p2)) ->
      begin
        match sum_rec (p1,p2) with
        | Const c when T.equal c T.zero -> Const(T.sum c1 c2)
        | p -> Poly(T.sum c1 c2, p)
      end
  in sum_rec (p1,p2)

  (* term builder *)
  let term c n =
    if n < 0 then
      raise (Invalid_argument ((string_of_int n)^" should be positive"))
    else
      let rec term_rec = function
      | 0 -> Const c
      | n -> Poly(T.zero,term_rec (n-1))
    in term_rec n

  (* building a random polynomial of degree n, useful for tests *)
  let random n =
    if n < 0 then
      raise (Invalid_argument ((string_of_int n)^" should be positive"))
    else
      let rec random_rec = function
      | 0 -> Const (T.random ())
      | n -> Poly(T.random (),random_rec (n-1))
      in random_rec n
  
  (* stringify a polynomial *)
  let to_string p =
    let string_of_term c i = Utils.string_of_term (fun r -> if T.equal r T.one then "" else T.to_string r) c i in
    if p = zero then
      "0"
    else
      let rec to_str n = function
      | Const c when T.equal c T.zero -> "",""
      | Const c when T.compare c T.zero > 0 -> "+",string_of_term c n
      | Const c -> "-",string_of_term (T.abs c) n
      | Poly(c,p) when T.equal c T.zero -> to_str (n+1) p
      | Poly(c,p) ->
        begin
          let coef,sign = if T.compare c T.zero > 0 then c,"+" else (T.abs c),"-" in
          match to_str (n+1) p with
          | "",_ -> sign, string_of_term coef n
          | op,str when T.equal c T.zero -> op,str
          | op,str -> sign, Printf.sprintf "%s %s %s" (string_of_term coef n) op str
        end
      in match to_str 0 p with
      | "-",rest -> "-"^rest
      | _,rest -> rest

  (* As our polynomial have a unique representation, the equality can be syntactic *)
  let for_all2 pred p1 p2 =
    let rec for_all2_rec = function
    | (Const c1,Const c2) -> pred c1 c2
    | (Const _,Poly _) | (Poly _,Const _) -> false
    | (Poly(c1,p1),Poly(c2,p2))-> (pred c1 c2) && (for_all2_rec (p1,p2))
    in for_all2_rec (p1,p2)
    
  let equal p1 p2 = for_all2 T.equal p1 p2

  (* (c1 + x P1) P2 = c1 P2 + x P1P2 *)
  let prod p1 p2 =
    if p1 = zero || p2 = zero then
      zero
    else
      let rec prod_rec = function
      | Const c -> map (fun coef -> T.prod c coef) p2
      | Poly(c,p) -> sum (map (fun coef -> T.prod c coef) p2) (Poly(T.zero,(prod_rec p)))
      in prod_rec p1
end
