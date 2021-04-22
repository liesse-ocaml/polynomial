module type IO_OBJECT =
sig
  type t
  (* IO functions *)
  (* val of_string: string -> t *)
  val to_string: t -> string
end

module type EQ =
sig
  type t
  val equal: t -> t -> bool
end

module type COMPARABLE =
sig
  include EQ
  val compare: t -> t -> int
end

module type TESTABLE =
sig
  type t
  include EQ with type t := t
  include IO_OBJECT with type t := t
  val random: unit -> t (* random returns a random element *)
end

module type RING =
sig
  type t
  (* zero element *)
  val zero: t (* neutral for sum absorbant for prod *)
  (* unit element *)
  val one: t (* neutral for prod *)
  (* operations *)
  val sum: t -> t -> t  (* sum is commutative and associative *)
  val sym: t -> t  (* sum x (sym x) = zero *)
  val prod: t -> t -> t (* prod is associative and distributive w.r.t. sum *)
end

module type DOMAIN =
sig
  include RING
  include COMPARABLE with type t := t
  include TESTABLE with type t := t

  val abs: t -> t
end

module type POLYNOMIAL =
sig
  include RING
  include EQ with type t := t
  include IO_OBJECT with type t := t

  type var
  val term: var -> int -> t (* term a n build the term a x^n *)
  val deg: t -> Utils.degree (* the degree of the polynomial *)
  val eval: t -> var -> var (* eval p v evaluate the polynomial p on v, p(v) *)
  val random: int -> t (* random n returns a random polynomial of degree n *)
end
