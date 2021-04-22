module Poly : Interface.POLYNOMIAL with type var := float =
struct
  (* Polynomials are encoded as array of floats
     [| a1; a2;...; an|] is a1 + a2 x + ... + an x^(n-1)
     invariant: no trailing zero at the end of the array (an /= 0)
  *)
  type t = float array

  (* the constant polynomials *)
  let zero = [| 0.0 |]
  let one = [| 1.0 |]

  (* term builder *)
  let term c n =
    if n < 0 then
      raise (Invalid_argument ((string_of_int n)^" should be positive"))
    else
      let result = Array.make (n+1) 0.0 in
      result.(n) <- c;
      result

  (* building a random polynomial of degree n, useful for tests *)
  let random n =
    if n < 0 then
      raise (Invalid_argument ((string_of_int n)^" should be positive"))
    else
      let result = Array.make (n+1) 0.0 in
      for i = 0 to n do
        let sign = if Random.bool () then 1.0 else -1.0 in
        result.(i) <- sign *. (Random.float 20.0)
      done;
      result
  (* a very naive implementation *)
  let eval p x =
    let result = ref p.(0) in
    for i = 1 to Array.length p -1 do
      result := !result +. p.(i) *. Utils.pow x i
    done;
    !result

  (* stringify a polynomial *)
  let to_string p =
    let string_of_term c i = Utils.string_of_term Utils.string_of_float c i in
    if p = zero then
      "0"
    else
      let result = ref "" in
      let first = ref true in
      for i = 0 to Array.length p-1 do
        match p.(i), !first with
        | 0.0, _ -> ()
        | c, true -> first := false; result := (string_of_term c i)
        | c, false when c > 0.0 -> result := !result^" + "^(string_of_term c i)
        | c, false -> result := !result^" - "^(string_of_term (Float.abs c) i)
      done;
      !result

  (* As our polynomial have a unique representation, the equality can be syntactic *)
  (* But we will need to take care of rounding *)

  (* code taken for OCaml 4.11, for_all2 p l1 l2 is true all pairs e1,e2 of elements of the arrays statisfy p *)
  let for_all2 p l1 l2 =
    let n1 = Array.length l1
    and n2 = Array.length l2 in
    if n1 <> n2 then invalid_arg "Array.for_all2"
    else let rec loop i =
      if i = n1 then true
      else if p (Array.unsafe_get l1 i) (Array.unsafe_get l2 i) then loop (succ i)
      else false in
    loop 0
    
  (* the equal itself *)
  let equal p1 p2 = for_all2 (fun f1 f2 -> Utils.is_zero (f1 -. f2)) p1 p2

  (* The degree of the polynomial, since the invariant the length give the degree+1 *)
  let deg p = Utils.(if p = zero then Inf else D (Array.length p-1))

  (* Create a new array with last cell not containing 0 *)
  let simplify p =
    (* find the first non zero coefficient starting from the end *)
    let index = ref (Array.length p-1) in
    let found = ref false in
    while (not !found && !index >= 0) do
      if not (Utils.is_zero p.(!index)) then
        found := true
      else
        decr index
    done;
    (* copy the first index coefficients from p *)
    if !found then
      Array.init (!index+1) (fun i -> Utils.round p.(i))
    else
      zero

  (* the sum of two polynomials *)
  let sum p1 p2 =
    if p1 = zero then
      p2
    else if p2 = zero then
      p1
    else
      begin
        let l1 = Array.length p1 in
        let l2 = Array.length p2 in
        let result = Array.make (max l1 l2) 0.0 in
        for i = 0 to (min l1 l2)-1 do
          (* notice that the following operation may break the invariant *)
          result.(i) <- p1.(i) +. p2.(i)
        done;
        begin
          if l1 > l2 then
            for i = l2 to l1-1 do
              result.(i) <- p1.(i)
            done
          else
            for i = l1 to l2-1 do
              result.(i) <- p2.(i)
            done
        end;
        (* as invariant may be broken *)
        simplify result
      end

  (* symmetric of the polynomial w.r.t. sum *)
  let sym p =
    let len = Array.length p in
    let result = Array.make len 0.0 in
    for i = 0 to len-1 do
      result.(i) <- -. p.(i)
    done;
    result

  (* the product of two polynomials: notice that the product does not break the invariant *)
  let prod p1 p2 =
    if p1 = zero || p2 = zero then
      zero
    else
      let l1 = Array.length p1 in
      let l2 = Array.length p2 in  
      let get1 i = if i < l1 then p1.(i) else 0.0 in
      let get2 i = if i < l2 then p2.(i) else 0.0 in
      let result = Array.make (l1+l2-1) 0.0 in
      for i = 0 to l1+l2-2 do
        for j = 0 to i do
          result.(i) <- result.(i) +. get1 j *. get2 (i-j)
        done
      done;
      result
end
