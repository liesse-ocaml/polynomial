module Pol :
sig
  type t
  type deg

  val make : float list -> t
  val eval : t -> float -> float
  val x : t
  val zero : t
  val un : t
  val deg : t -> string
  val string_of_pol : t -> string
  val add : t -> t -> t
  val ( ++ ) : t -> t -> t
  val opp : t -> t
  val ( -- ) : t -> t -> t
  val mult : t -> t -> t
  val ( ** ) : t -> t -> t
  val pow : t -> int -> t
  val ( ^* ) : t -> int -> t
  val div : t -> t -> t * t
  val quo : t -> t -> t
  val rem : t -> t -> t
  val pgcd : t -> t -> t
  val bezout : t -> t -> t * t
  val diff : t -> t
  val diff_k : t -> int -> t
end =
struct
  type deg = Inf | D of int
  (* Ce type implémente le degré des polynômes. Inf code le degré du polynôme
     nul. *)

  type t = deg * float list
  (* Ce type implémente les polynômes. Un polynôme est un couple
     composé d'un degré et de la liste des coefficients. (Inf, [])
     représente le polynôme nul et (D d, [a_d,a_{d-1},...,a_0]
     représente le polynôme a_0 + a_ 1 X + ... + a_d X^d. Le premier
     élément de la liste est donc non nul et le nombre d'éléments de
     la liste est égal à d + 1. Par ailleurs, (Inf, _::_) ne code pas
     un polynôme. *)

     (* zero des flottants *)
  let is_zero x =
    x < 1e-15 && (-.x) < 1e-15

  (* retire les zéros en tête d'une liste *)
  let rec remove_zeros l =
    match l with
    | [] -> []
    | x :: queue when is_zero x -> remove_zeros queue
    | _ -> l

  (* fonction qui rétablit les invariants pour un polynôme *)
  let rec cleanup p =
    match p with
    | (Inf, []) -> p
    | (D 0, [a]) when is_zero a -> (Inf, [])
    | (D d, a :: q) when is_zero a -> cleanup (D (d - 1), q)
    | (D _, _) -> p
    | _ -> failwith "cleanup : polynôme mal formé"

  (* constructeur des polynomes. La fonction reçoit en entrée la liste
     des coefficients dans l'ordre décroissant en degré et construit
     le polyôme associé *)
  let make l =
    let liste = remove_zeros l in
    match liste with
    | [] -> (Inf, [])
    | _ -> cleanup (D (List.length liste - 1), liste)

  (* polynome X *)
  let x = make [1.; 0.]

  (* polynôme nul *)
  let zero = make [0.]

  (* polynôme constant égal à 1 *)
  let un = make [1.]

  (* évalue le polynôme p en x *)
  let eval p x =
    let rec aux p acc =
      match p with
      | (Inf, []) -> acc
      | (D _, an :: queue) -> aux (make queue) (x *. acc +. an)
      | _ -> failwith "p est mal formé" in
    aux p 0.

  (* renvoie le degré d'un polynôme *)
  let deg p =
    match fst p with
    | Inf -> "-∞"
    | D k -> string_of_int k

  (* renvoie une chaine de caractères représentant le polynôme p *)
  let string_of_pol p =
    let rec aux p =
      match p with
      | (Inf, []) -> "0"
      | (D 0, [a]) when is_zero a -> ""
      | (D 0, [a]) -> string_of_float a
      | (D d, 1. :: queue) when d = 1 ->
        "X" ^ " + " ^ aux (D (d - 1), queue)
      | (D d, 1. :: queue) ->
        "X^" ^ (string_of_int d) ^ " + " ^ aux (D (d - 1), queue)
      | (D d, a :: queue) when a <> 0. && d = 1 ->
        string_of_float a ^ "X" ^ " + " ^ aux (D (d - 1), queue)
      | (D d, a :: queue) when a <> 0. ->
        string_of_float a ^ "X^" ^ (string_of_int d) ^ " + " ^ aux (D (d - 1), queue)
      | (D d, a :: queue) ->  aux (D (d - 1), queue)
      | _ -> failwith "string_of_pol : p est mal formé" in
    let cleanup s =
      let n = String.length s in
      if n >= 3 && s.[n - 2] = '+' then String.sub s 0 (n - 3) else s in
    cleanup (aux p)

  (* fonction pour debugger *)
  let debug p =
    let f x = print_float x; print_string " " in
    let l = snd p in
    let d = match fst p with Inf -> -1 | D k -> k in
    print_string "["; List.iter f l; print_string "]\t"; print_int d;print_newline()

  (* calcule p + q *)
  let add p q =
    let rec aux p q =
      match p, q with
      | (Inf, []), a | a, (Inf, []) -> a
      | (D 0, [a]), (D 0, [b]) -> (D 0, [a +. b])
      | (D d1, a :: r1), (D d2, b :: r2) when d1 > d2 ->
        (D d1, a :: snd (aux (D (d1 - 1), r1) q))
      | (D d1, a :: r1), (D d2, b :: r2) when d1 < d2 ->
        (D d2, b :: snd (aux p (D (d2 - 1), r2)))
      | (D d1, a :: r1), (D d2, b :: r2) ->
        (D d1, (a +. b) :: snd (aux (D (d1 - 1), r1) (D (d2 - 1), r2)))
      | _ -> failwith "add : l'un des polynomes est mal formé" in
    cleanup (aux p q)

  (* version infixe de l'addition *)
  let ( ++ ) p q = add p q

  (* calcule -p *)
  let opp p =
    match p with
    | (Inf, []) -> p
    | (D d, l) -> (D d, List.map (fun x -> -.x) l)
    | _ -> failwith "opp : p est mal formé"

  (* calcule p - q - version infixe *)
  let ( -- ) p q = p ++ (opp q)

  (* renvoie le couple (a, b) si p = a X^k + b où k est environ égal à deg(p) / 2 *)
  let divide p =
    match p with
    | (Inf, []) -> p, p
    | (Inf, _ ) -> failwith "divide : p est mal formé"
    | (D 0, l) -> p, (Inf, [])
    | D d, l ->
      let rec aux l l1 l2 counter =
        match l, counter with
        | [], _ -> l1, l2
        | x :: queue, k when k <= d/2 -> aux queue (x :: l1) l2 (counter + 1)
        | x :: queue, _ -> aux queue l1 (x :: l2) (counter + 1)  in
      let l1, l2 = aux l [] [] 0 in
      make (List.rev l1), make (List.rev l2)

  (* calcule p X^d + q *)
  let shift_add p q d =
    let rec zeros_add l d =
      match d with
      | 0 -> l
      | _ -> zeros_add (0. :: l) (d - 1) in
    let l1 = List.rev (zeros_add (List.rev (snd p)) d) in
    add (make l1) q

  (* calcule p * q à l'aide de l'algorithme de Karatsuba : si p = p1
     X^d + p2 et q = q1 X^d + q2, alors p * q = m1 X^{2d} + (m3 - m1 -
     m2) X^d + m2 où m1 = p1 * q1, m2 = p2 q2 et m3 = (p1 + p2) * (q1
     + q2) *)
  let rec mult p q =
    let rec zeros_add l d =
      match d with
      | 0 -> l
      | _ -> zeros_add (0. :: l) (d - 1) in
    match p, q with
    | (Inf, []), _ | _, (Inf, []) -> make [0.]
    | (D 0, [a]), (D d, l) | (D d, l), (D 0, [a])  -> (D d, List.map (fun x -> a *. x) l)
    | (D n, lp), (D m, lq) when n <= m ->
      let lp_augmente = zeros_add lp (m - n)
      (* on ajoute des zeros en tete pour que les listes aient la même longueur *)
      in
      let d = m - m / 2 (* partie entière supérieure de m / 2 *) in
      let p1, p2 = divide (D m, lp_augmente) in let q1, q2 = divide q in
      let m1 = mult p1 q1 and m2 = mult p2 q2 and m3 = mult (p1 ++ p2) (q1 ++ q2) in
      shift_add m1 (shift_add ((m3 -- m1) -- m2) m2 d) (2 * d)
    | (D n, _), (D m, _) when n > m -> mult q p (* on se ramène au cas précéedent *)
    | _ -> failwith "mult : l'un des polynomes est mal formé"

  (* version infixe de la multiplication *)
  let ( ** ) p q = mult p q

  (* calcule lambda * P où lambda est un scalaire *)
  let ext_mult lambda p = mult (make [lambda]) p

  (* calcule p^n par exponentiation rapide *)
  let rec pow p n =
    match n with
    | 0 -> make [1.]
    | k when k < 0 -> failwith "pow : puissance negative"
    | k when k mod 2 = 0 -> let y = pow p (n / 2) in mult y y
    | _ -> let y = pow p (n / 2) in mult p (mult y y)

  (* version infixe de p^n *)
  let ( ^* ) p n = pow p n

  (* effectue la division euclidienne de p par q *)
  let rec div p q =
    match q with
    | (Inf, []) -> failwith "div : le diviseur est nul"
    | (Inf, _) | (D _, []) -> failwith "div : q est mal formé"
    | (D d, bd :: queue_q) ->
      match p with
      | (Inf, []) -> p, p
      | (Inf, _) | (D _, []) -> failwith "div : p est mal formé"
      | (D n, _) when n < d -> make [0.], p
      | (D n, an :: queue_p) -> let pp = p ++ (opp (ext_mult (an /. bd) (x^*(n - d)) ** q)) in
        let q1, r = div pp q in
        (ext_mult (an /. bd) (x^*(n - d))) ++ q1, r

  (* calcule le reste de la division euclidienne de p par q *)
  let quo p q = fst (div p q)

  (* calcule le quotient de la division euclidienne de p par q *)
  let rem p q = snd (div p q)

  (* calcule le polynôme nul ou unitaire associé à p *)
  let normalize p =
    match p with
    | (Inf, []) -> p
    | (_, an :: _) -> ext_mult (1. /. an) p
    | _ -> failwith "normalize : p est mal formé"

  (* calcule le pgcd de p et q par l'agorithme d'Euclide *)
  let rec pgcd p q =
    match q with
    | (Inf, []) -> normalize p
    | (D _, _ :: _) -> pgcd q (rem p q)
    | _ -> failwith "pgcd : q est mal formé"

  (* calcule un couple de Bézout asssocié à a et b à l'aide de
     l'algorithme d'Euclide étendu *)
  let bezout a b =
    let rec aux a u v b u' v' =
      match b with
      | (Inf, []) -> u, v
      | (D _, _ :: _) -> let q, r = div a b in
        aux b u' v' r (u --  q ** u') (v -- q ** v')
      | _ -> failwith "bezout : b est mal formé" in
    aux a un zero b zero un

  (* calcule la dérivée formelle de p *)
  let rec diff p =
    match p with
    | (Inf, []) -> p
    | (D 0, _::_) -> zero
    | (D d, ad :: queue) -> let r = diff (make queue) in
      (D (d - 1), (float_of_int d *. ad) :: snd r)
    | _ -> failwith "diff : p est mal formé"

  (* calcule la dérivée k^eme formelle de p *)
  let rec diff_k p k =
    match k with
    | 0 -> p
    | u when u < 0 -> failwith "diff_k : k est négatif"
    | _ -> diff_k (diff p) (k - 1)
end;;

open Pol;;

let p = make [1.; 3.; 2.; 1.];;
let q = make [1.; 1.];;
string_of_pol (pow q 100);;
string_of_pol (p ++ q);;
string_of_pol (add p q);;
string_of_pol (p ** q);;
string_of_pol (x ^* 100);;
string_of_pol (p ++ (opp p));;
string_of_pol (pgcd p q);;
string_of_pol (fst (bezout p q));;
(eval p 1.);;
