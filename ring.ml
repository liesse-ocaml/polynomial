open Interface
module Test (R: sig include RING include TESTABLE with type t := t end) =
struct
  open Test
  open Test(R)
  open R
  let () =
    let eq_str p1 p2 = equal p1 p2, to_string p1, to_string p2 in
    (* zero neutral element for sum *)
    repeat_true 7 (fun () -> let p = random () in eq_str (sum p zero) p) "test %i: P + 0 <> P (%s versus %s)\n";
    repeat_true 7 (fun () -> let p = random () in eq_str (sum zero p) p) "test %i: 0 + P <> P (%s versus %s)\n";
    (* unit neutral element for prod *)
    repeat_true 7 (fun () -> let p = random () in eq_str (prod p one) p) "test %i: P * 1 <> P (%s versus %s)\n";
    repeat_true 7 (fun () -> let p = random () in eq_str (prod one p) p) "test %i: 1 * P <> P (%s versus %s)\n";
    (* absorbant element *)
    repeat_true 7 (fun () -> eq_str (prod (random ()) zero) zero) "test %i: P * 0 <> 0 (%s versus %s)\n";
    repeat_true 7 (fun () -> eq_str (prod zero (random ())) zero) "test %i: 0 * P <> 0 (%s versus %s)\n";
    (* sum is commutative *)
    repeat_true 12 (fun () -> let p1 = random () and p2 = random () in eq_str (sum p1 p2) (sum p2 p1)) "test %i: P1 + P2 <> P2 + P1 (%s versus %s)\n";
    (* sum is associative *)
    repeat_true 17 (fun () ->
      let p1 = random () and p2 = random () and p3 = random () in
      eq_str (sum p1 (sum p2 p3)) (sum (sum p1 p2) p3)) "test %i: P1 + (P2 + P3) <> (P1 + P2) + P3 (%s versus %s)\n";
    (* sum x (sym x) = zero *)
    repeat_true 5 (fun () -> let p = random () in eq_str (sum p (sym p)) zero) "test %i: R - R <> 0 (%s versus %s)\n";
    (* prod is associative *)
    repeat_true 17 (fun () ->
      let p1 = random () and p2 = random () and p3 = random () in
      eq_str (prod p1 (prod p2 p3)) (prod (prod p1 p2) p3)) "test %i: P1 * (P2 * P3) <> (P1 * P2) * P3 (%s versus %s)\n";
    (* prod is distributive w.r.t. sum *)
    repeat_true 13 (fun () ->
      let p1 = random () and p2 = random () and p3 = random () in
      eq_str (prod p1 (sum p2 p3)) (sum (prod p1 p2) (prod p1 p3))) "test %i: P1 * (P2 + P3) <> (P1 * P2) + (P1 * P3) (%s versus %s)\n";
    report "ring"
end
