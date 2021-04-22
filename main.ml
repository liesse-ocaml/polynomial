
let sum_list zero sum l =
  let rec sum_rec = function
  | [] -> zero
  | [p] -> p
  | p1::r -> sum p1 @@ sum_rec r
  in
  sum_rec l

open Test
module TestFloat = Test(struct type t = float let equal f1 f2 = Utils.is_zero (f1 -. f2) let to_string = Utils.string_of_float end)
module TestString = Test(struct type t = string let equal s1 s2 = s1 = s2 let to_string x = x end)
module TestRatio = Test(Ratio.R)

module Test(P: sig val name: string include Interface.POLYNOMIAL with type var := float end) =
struct
  open P

  let () =
    print_endline ("------------------"^name^"------------------")

  (* creating the module executes the test of Ring conformity *)
  module TR = Ring.Test(struct include P let random () = random (Random.int 17)end)

  let test_with_string to_str p res = 
    TestString.test to_str p res "wrong test with string, got \"%s\" was waiting \"%s\"\n"
  let test_for_eval p v res = 
    TestFloat.test (eval p) v res "wrong test for eval, got \"%s\" was waiting \"%s\"\n"
  
  let () =
  let test_string = test_with_string to_string in
  (* testing constructors *)
    test_string (term 0.0 0) "0";
    let test_exc f msg = try let _ = f () in print_endline msg with Invalid_argument _ -> () in
    test_exc (fun () -> term 0.0 (-2)) "wrong test for term, exception not raised on -2";
    test_exc (fun () -> term (-13.0) (-20)) "wrong test for term, exception not raised on -20";
    test_exc (fun () -> random (-167)) "wrong test for random, exception not raised on -167";
  (* the following construction also tests sum and term *)
    let build_poly = List.fold_left (fun acc (c,n) -> sum acc @@ term c n) zero in
    let p1 = build_poly [3.0,0;-2.0,1;5.0,2] in
    let p2 = build_poly [-3.0,0;2.0,1;15.0,2;1.25,4] in
    let p3 = build_poly [0.0,0;0.02,1;-1.0,3;-1.25,4] in
    (* testing to_string *)
    test_string p1 "3 - 2x + 5x^2";
    test_string p2 "-3 + 2x + 15x^2 + 1.25x^4";
    test_string p3 "0.02x - x^3 - 1.25x^4";
    (* testing sum *)
    test_string (sum p1 p2) "20x^2 + 1.25x^4"; (* with common coefficients *)
    test_string (sum p1 p3) "3 - 1.98x + 5x^2 - x^3 - 1.25x^4";
    test_string (sum p3 p2) "-3 + 2.02x + 15x^2 - x^3"; (* with symmetric leading coefficient *)
    (* testing product *)
    test_string (prod p1 p2) "-9 + 12x + 26x^2 - 20x^3 + 78.75x^4 - 2.5x^5 + 6.25x^6";
    test_string (prod p1 p3) "0.06x - 0.04x^2 - 2.9x^3 - 1.75x^4 - 2.5x^5 - 6.25x^6";
    test_string (prod p3 p2) "-0.06x + 0.04x^2 + 3.3x^3 + 1.75x^4 - 17.475x^5 - 18.75x^6 - 1.25x^7 - 1.5625x^8";
    (* testing the degree *)
    let test_string = test_with_string Utils.string_of_degree in
    test_string (deg zero) "-∞";
    test_string (deg one) "0";
    test_string (deg p1) "2";
    test_string (deg p2) "4";
    test_string (deg p3) "4";
    test_string (deg (sum p1 p2)) "4";
    test_string (deg (sum p1 p3)) "4";
    test_string (deg (sum p3 p2)) "3";
    test_string (deg (prod p1 p2)) "6";
    test_string (deg (prod p1 p3)) "6";
    test_string (deg (prod p3 p2)) "8";
    (* testing eval *)
    test_for_eval zero 0.0 0.0;
    test_for_eval zero (Random.float 175.54) 0.0;
    test_for_eval zero (Random.float (-71.94)) 0.0;
    test_for_eval one 0.0 1.0;
    test_for_eval one (Random.float 175.54) 1.0;
    test_for_eval one (Random.float (-71.94)) 1.0;
    test_for_eval p1 0.0 3.0;
    let r = Random.float 175.54 in test_for_eval p1 r (3.0 -. 2.0*.r +. 5.0*.(Utils.pow r 2));
    let r = Random.float (-71.94) in test_for_eval p1 r (3.0 -. 2.0*.r +. 5.0*.(Utils.pow r 2));
    test_for_eval p2 0.0 (-3.0);
    let r = Random.float 175.54 in test_for_eval p2 r (-3.0 +. 2.0*.r +. 15.0*.(Utils.pow r 2) +. 1.25*.(Utils.pow r 4));
    let r = Random.float (-71.94) in test_for_eval p2 r (-3.0 +. 2.0*.r +. 15.0*.(Utils.pow r 2) +. 1.25*.(Utils.pow r 4));
    test_for_eval p3 0.0 0.0;
    let r = Random.float 175.54 in test_for_eval p3 r (0.02*.r -. (Utils.pow r 3) -. 1.25*.(Utils.pow r 4));
    let r = Random.float (-71.94) in test_for_eval p3 r (0.02*.r -. (Utils.pow r 3) -. 1.25*.(Utils.pow r 4));
    report "polynomial"
end

module TestR(P: sig val name: string include Interface.POLYNOMIAL with type var := Ratio.R.t end) =
struct  
  open Printf
  open P
  open Ratio

  let () =
    print_endline ("------------------"^name^"------------------")

  (* creating the module executes the test of Ring conformity *)
  module TR = Ring.Test(struct include P let random () = random (Random.int 17)end)

  let test_with_string to_str p res = 
    TestString.test to_str p res "wrong test with string, got \"%s\" was waiting \"%s\"\n"
  let test_for_eval p v res = 
    TestRatio.test (eval p) v res "wrong test for eval, got \"%s\" was waiting \"%s\"\n"
  
  let () =
  let test_string = test_with_string to_string in
  (* testing constructors *)
    test_string (term R.zero 0) "0";
    try let _ = term R.zero (-2) in printf "wrong test, exception not raised for %i" (-2) with Invalid_argument _ -> ();
    try let _ = term (R.create (-13) 7) (-20) in printf "wrong test, exception not raised for %i" (-20) with Invalid_argument _ -> ();
    try let _ = random (-167) in printf "wrong test, exception not raised for %i" (-167) with Invalid_argument _ -> ();
  (* the following construction also tests sum and term *)
    let p1 = List.fold_left (fun acc (c,n) -> sum acc @@ term c n) zero [R.create 3 7,0; R.create (-2) 1,1; R.create 5 2,2] in
    let p2 = List.fold_left (fun acc (c,n) -> sum acc @@ term c n) zero [R.create (-3) 7,0; R.create 2 1,1; R.create 15 1,2; R.create 5 4,4] in
    let p3 = List.fold_left (fun acc (c,n) -> sum acc @@ term c n) zero [R.zero,0; R.create 1 50,1; R.create (-1) 1,3; R.create (-5) 4,4] in
    (* testing to_string *)
    test_string p1 "3/7 - 2x + 5/2x^2";
    test_string p2 "-3/7 + 2x + 15x^2 + 5/4x^4";
    test_string p3 "1/50x - x^3 - 5/4x^4";
    (* testing sum *)
    test_string (sum p1 p2) "35/2x^2 + 5/4x^4"; (* with common coefficients *)
    test_string (sum p1 p3) "3/7 - 99/50x + 5/2x^2 - x^3 - 5/4x^4";
    test_string (sum p3 p2) "-3/7 + 101/50x + 15x^2 - x^3"; (* with symmetric leading coefficient *)
    (* testing product *)
    test_string (prod p1 p2) "-9/49 + 84/49x + 133/98x^2 - 350/14x^3 + 2130/56x^4 - 20/8x^5 + 25/8x^6";
    test_string (prod p1 p3) "3/350x - 28/700x^2 - 265/700x^3 + 82/56x^4 - 25/8x^6";
    test_string (prod p3 p2) "-3/350x + 14/350x^2 + 255/350x^3 - 8200/5600x^4 - 13980/800x^5 - 300/16x^6 - 20/16x^7 - 25/16x^8";
    (* testing the degree *)
    let test_string = test_with_string Utils.string_of_degree in
    test_string (deg zero) "-∞";
    test_string (deg one) "0";
    test_string (deg p1) "2";
    test_string (deg p2) "4";
    test_string (deg p3) "4";
    test_string (deg (sum p1 p2)) "4";
    test_string (deg (sum p1 p3)) "4";
    test_string (deg (sum p3 p2)) "3";
    test_string (deg (prod p1 p2)) "6";
    test_string (deg (prod p1 p3)) "6";
    test_string (deg (prod p3 p2)) "8";
    (* testing eval *)
    test_for_eval zero R.zero R.zero;
    test_for_eval zero (R.random ()) R.zero;
    test_for_eval zero (R.random ()) R.zero;
    test_for_eval one R.zero R.one;
    test_for_eval one (R.random ()) R.one;
    test_for_eval one (R.random ()) R.one;
    test_for_eval p1 R.zero @@ R.create 3 7;
    let r = R.random () in test_for_eval p1 r (let n = R.num r and d = R.den r in R.create (6*d*d-28*n*d+35*n*n) (14*d*d));
    let r = R.random () in test_for_eval p1 r (let n = R.num r and d = R.den r in R.create (6*d*d-28*n*d+35*n*n) (14*d*d));
    test_for_eval p2 R.zero @@ R.create (-3) 7;
    let r = R.random () in test_for_eval p2 r (let n = R.num r and d = R.den r in R.create (-12*d*d*d*d+56*n*d*d*d+420*n*n*d*d+35*n*n*n*n) (7*4*d*d*d*d));
    let r = R.random () in test_for_eval p2 r (let n = R.num r and d = R.den r in R.create (-12*d*d*d*d+56*n*d*d*d+420*n*n*d*d+35*n*n*n*n) (7*4*d*d*d*d));
    test_for_eval p3 R.zero R.zero;
    let r = R.random () in test_for_eval p3 r (let n = R.num r and d = R.den r in R.create (4*n*d*d*d-200*n*n*n*d-250*n*n*n*n) (200*d*d*d*d));
    let r = R.random () in test_for_eval p3 r (let n = R.num r and d = R.den r in R.create (4*n*d*d*d-200*n*n*n*d-250*n*n*n*n) (200*d*d*d*d));
    report "polynomial"
end
(* creating the module to launch the tests *)
module TA = Test(struct let name = "ARRAY" include Array_polynomial.Poly end)
(* creating the module to launch the tests *)
module TL = Test(struct let name = "LIST" include List_polynomial.Poly end)
(* creating the module to launch the tests *)
module TH = TestR(struct let name = "HORNER" include Horner_polynomial.Poly(Ratio.R) end)
