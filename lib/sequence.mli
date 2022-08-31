(** Definition of sequences of values. *)
module type Sequence_definition =
sig
  (** The type of the sequence. *)
  type 'a t

  (** The empty sequence.

     The unit argument is for array sequences to avoid
     the monomorphism restriction. *)
  val nil : unit -> 'a t

  (** Puts one element into the sequence. *)
  val cons : (unit -> 'a * 'a t) -> 'a t

  (** Returns the head and the tail of the sequence. *)
  val get : 'a t -> ('a * 'a t) option

  (** Convert an imperative function to a sequence.
   * If the function argument returns None then this will be the end of the
   * sequence. *)
  val from_fun : (unit -> 'a option) -> 'a t
end

(**
   This module type contains operations that sequences must provide.
   The function names mostly correspond to known functions on lists.
 *)
module type Sequence =
sig
  include Sequence_definition

  (** Applies a function to all elements of a sequence. *)
  val map : ('a -> 'b) -> 'a t -> 'b t

  (** Returns a sequence of all elements that fulfill a condition. *)
  val filter : ('a -> bool) -> 'a t -> 'a t

  (** Applies a function to all elements of a sequence
     but omits all [None]s. *)
  val map_filter : ('a -> 'b option) -> 'a t -> 'b t

  (** Appends two sequences. If the first one is infinite then the result behaves like the first argument. *)
  val append : 'a t -> 'a t -> 'a t

  (** Like append but for a whole sequence of sequences. *)
  val flatten : ('a t) t -> 'a t

  (** Convert a list to a sequence. *)
  val of_list : 'a list -> 'a t

  (** Performs fold and map at the same time but drops the accumulator. *)
  val fold_map : ('a -> 'b -> 'c * 'b) -> 'a t -> 'b -> 'c t

  (** Accumulates a value during an iteration over the sequence. *)
  val fold : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b

  (** Inverts the order of a finite sequence. *)
  val reverse : 'a t -> 'a t
end

(** Returns a sequence implementation when given a definition. *)
module Make_sequence (S : Sequence_definition) : Sequence with type 'a t = 'a S.t

(** Arrays as sequences. *)
module Array_sequence : functor (_ :
    sig
      (** If [true], allows putting more elements into
        the sequence than the initial capacity of the
        array. If [false], cons may throw an exception
        if too many cons operations are performed. *)
      val allow_copy_on_resize : bool

      (** If [true], multiple cons calls per sequence
        are allowed. The array is copied for every
        cons operation except the first one. If [false],
        only one cons call per sequence is allowed and
        an exception is thrown otherwise. *)
      val allow_copy_on_multiple_cons : bool
    end) ->
  sig
    include Sequence

    (** Creates a new array sequence with an initial
      capacity and a default element. *)
    val make : int -> 'a -> 'a t

    (** Initializes the sequence with the given array. *)
    val init : 'a array -> 'a t

    (** Returns the element at a certain position of
     the underlying array. The index zero points to
     the last element of the sequence. *)
    val array_get : 'a t -> int -> 'a

    (** Convert a sequence of characters to a string. *)
    val to_string : char t -> string

    (** Convert a string to a sequence of characters. *)
    val of_string : string -> char t

    (** Convert an array sequence to a list. *)
    val to_list : 'a t -> 'a list

    (** Returns the number of elements in the sequence. *)
    val length : 'a t -> int
  end

(** A sequence implementation using finite lists. *)
module List_sequence : Sequence with type 'a t = 'a list

(** A sequence implementation using lazy lists. *)
module Lazy_list_sequence : Sequence
