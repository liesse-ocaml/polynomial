module R :
sig
  include Interface.DOMAIN
  val create: int -> int -> t
  val num: t -> int
  val den: t -> int
end = struct
  type t = { num : int ; den : int }

  let zero = {num=0; den=1}
  let one = {num=1; den=1}

  let create n = function
  | 0 -> failwith "wrong denominator"
  | d -> { num = n ; den = d }

  let num r = r.num and den r = r.den

  let abs {num;den} = { num = abs num; den = abs den}

  let sym {num;den} = { num = - num; den = den}

  let random () = let result = { num = Random.int 20; den = 1+ (Random.int 25)} in if Random.bool () then result else sym result

  let sum r1 r2 = { num = r1.num * r2.den + r2.num * r1.den ; den = r1.den * r2.den }

  let prod r1 r2 = { num = r1.num * r2.num ; den = r1.den * r2.den }

  let compare {num = n1 ; den = d1} {num = n2 ; den = d2} = compare (n1*d2) (n2*d1)

  let equal r1 r2 = compare r1 r2 = 0

  let to_string = function
    | {num=0;_} -> "0"
    | {num;den=1} -> Printf.sprintf "%i" num
    | {num;den} -> Printf.sprintf "%i/%i" num den
end

module TestRatioRing = Ring.Test(R)