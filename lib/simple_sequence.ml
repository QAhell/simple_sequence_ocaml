module type Sequence_definition =
sig
  type 'a t
  val nil : unit -> 'a t
  val cons : (unit -> 'a * 'a t) -> 'a t
  val get : 'a t -> ('a * 'a t) option
  val from_fun : (unit -> 'a option) -> 'a t
end

module type Sequence =
sig
  include Sequence_definition
  val map : ('a -> 'b) -> 'a t -> 'b t
  val filter : ('a -> bool) -> 'a t -> 'a t
  val map_filter : ('a -> 'b option) -> 'a t -> 'b t
  val append : 'a t -> 'a t -> 'a t
  val flatten : ('a t) t -> 'a t
  val of_list : 'a list -> 'a t
  val fold_map : ('a -> 'b -> 'c * 'b) -> 'a t -> 'b -> 'c t
  val fold : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  val reverse : 'a t -> 'a t
end

module Make_sequence (S : Sequence_definition) : Sequence with type 'a t = 'a S.t =
struct
  include S
  let rec map f xs =
    match get xs with
      | Some (x, xs) ->
          cons (fun () -> (f x, map f xs))
      | None -> nil()
  let rec fold f xs acc =
    match get xs with
      | Some (x, xs) ->
          fold f xs (f x acc)
      | None -> acc
  let rec fold_map f xs acc =
    match get xs with
      | Some (x, xs) ->
          let (fx, acc) = f x acc in
          cons (fun () -> (fx, fold_map f xs acc))
      | None -> nil ()
  let rec filter f xs =
    match get xs with
      | Some (x, xs) when f x ->
          cons (fun () -> (x, filter f xs))
      | Some (_, xs) -> filter f xs
      | None -> nil ()
  let rec map_filter f xs =
    match get xs with
      | Some (x, xs) ->
         (match f x with
           | Some y -> cons (fun () -> (y, map_filter f xs))
           | None -> map_filter f xs)
      | None -> nil ()
  let rec append xs ys =
    match get xs with
      | Some (x, xs) ->
          cons (fun () -> (x, append xs ys))
      | None -> ys
  let rec flatten xss =
    match get xss with
      | Some (xs, xss) ->
          append xs (flatten xss)
      | None -> nil ()
  let of_list xs =
    List.fold_right (fun x xs -> cons (fun () -> (x, xs))) xs (nil ())
  let reverse xs =
    fold (fun x xs -> cons (fun () -> (x, xs))) xs (nil ())
end

module Array_sequence : functor (_ :
    sig
      val allow_copy_on_resize : bool
      val allow_copy_on_multiple_cons : bool
    end) ->
  sig
    include Sequence
    val make : int -> 'a -> 'a t
    val init : 'a array -> 'a t
    val array_get : 'a t -> int -> 'a
    val to_string : char t -> string
    val of_string : string -> char t
    val to_list : 'a t -> 'a list
    val length : 'a t -> int
  end =
    functor (A : sig val allow_copy_on_resize : bool
                     val allow_copy_on_multiple_cons : bool end) ->
struct
  open A
  let make i c = (0, ref 0, Array.make i c)
  let init a = (Array.length a, ref (Array.length a), a)
  let array_get (_, _, a) i = a.(i)
  let to_string (i, _, a) =
    if i <= 0 then ""
    else
      let s = Bytes.make i a.(0) in
      for j = 0 to i - 1 do
        (* characters are stored in reverse *)
        Bytes.set s j a.(i - j - 1)
      done ; Bytes.to_string s
  let to_list (i, _, a) =
    let res = ref [] in
    for j = 0 to i - 1 do
      res := a.(j) :: !res
    done ;
    !res
  let of_string s =
    let i = String.length s in
    if i <= 0 then (0, ref 0, [||])
    else
      let a = Array.init i (fun j -> String.get s (i - 1 - j)) in
      (i, ref i, a)
  let length (i, _, _) = i
  module M = Make_sequence (struct
      type 'a t = int * int ref * 'a array
      let nil () = (0, ref 0, [||])
      let rec cons f =
        let (x, (i, j, a)) = f () in
        if i = !j && 0 <= i && i < Array.length a then
         (a.(!j) <- x ;
          j := !j + 1 ;
          (i + 1, j, a))
        else if i = !j && i >= Array.length a then
          (* a copy is needed because of the array being too small *)
         if allow_copy_on_resize then
           (let new_size = Array.length a * 2 + 1 in
            let a_copy = Array.make new_size x in
            Array.iteri (fun i a_i -> a_copy.(i) <- a_i) a ;
            a_copy.(i) <- x ;
            (i + 1, ref (i + 1), a_copy))
         else
           raise (Invalid_argument "Array_sequence.cons: array too small!")
        else if 0 <= i && i < Array.length a then
          (* repeated cons on this sequence ==> copy! *)
          if allow_copy_on_multiple_cons then
            cons (fun () -> (x, (i, ref i, Array.copy a)))
          else
            raise (Invalid_argument "Array_sequence.cons: repeated cons!")
        else (* repeated cons and array too small! dead code?! *)
          if allow_copy_on_multiple_cons && allow_copy_on_resize then
            (let new_size = Array.length a * 2 + 1 in
             let a_copy = Array.make new_size x in
             Array.iteri (fun i a_i -> a_copy.(i) <- a_i) a ;
             a_copy.(i) <- x ;
             (i + 1, ref (i + 1), a_copy))
          else
            raise
              (Invalid_argument
                "Array_sequence.cons: array too small and repeated cons!")
      let get (i, j, a) =
        if i <= 0 then
          None
        else
          Some (a.(i - 1), (i - 1, j, a))
      let from_fun f =
        let rec get_list acc =
          match f () with
            | Some x -> get_list (x :: acc)
            | None -> acc in
        let a = Array.of_list (get_list []) in
        (Array.length a, ref (Array.length a), a)
    end)
  include M
  let fold_map f (j, i, a) acc =
    if Array.length a = 0 then
      (j, i, [||])
    else
      begin
        let st = ref acc in
        let res = Array.make !i (fst (f a.(0) acc)) in
        for k = 0 to !i - 1 do
          let (a', st') = f a.(!i - k - 1) !st in
          st := st' ;
          res.(!i - k - 1) <- a'
        done ;
        (!i, ref !i, res)
      end
end

module List_sequence : Sequence with type 'a t = 'a list =
  Make_sequence (struct
    type 'a t = 'a list
    let nil () = []
    let cons f =
      let (x, xs) = f () in
      x :: xs
    let get = function
      | x :: xs -> Some (x, xs)
      | [] -> None
    let from_fun f =
      let rec ff result =
        match f () with
          | Some x -> ff (x :: result)
          | _ -> List.rev result in
      ff []
  end)

(* Why won't the compiler let me inline this definition?
 Error:
  File "lib/.sequence.objs/native/_unknown_", line 1, characters 0-0:
  >> Fatal error: nondep_supertype not included in original module type
  Fatal error: exception Misc.Fatal_error
  File "lib/.sequence.objs/byte/_unknown_", line 1, characters 0-0:
  >> Fatal error: nondep_supertype not included in original module type
  Fatal error: exception Misc.Fatal_error *)
module Lazy_list_sequence_definition =
struct
    type 'a ty = ('a l) Lazy.t
    and 'a l =
      | Nil
      | Cons of 'a * 'a ty
    type 'a t = 'a ty
    let nil () = Lazy.from_val Nil
    let cons f = Lazy.from_fun (fun () ->
      let (x, xs) = f () in
      Cons (x, xs))
    let get xs =
      match Lazy.force xs with
        | Nil -> None
        | Cons (x, xs) -> Some (x, xs)
    let rec from_fun f =
      Lazy.from_fun (fun () ->
        match f () with
          | Some x -> Cons (x, from_fun f)
          | None -> Nil)
  end

module Lazy_list_sequence : Sequence =
  Make_sequence (Lazy_list_sequence_definition)
