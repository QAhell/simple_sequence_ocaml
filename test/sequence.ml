open Sequence

module Sequence_test (S : Sequence) =
  struct
    let rec list_of sequence =
      match S.get sequence with
        | None -> []
        | Some (x, xs) -> x :: list_of xs

    let test_of_list () =
      assert (list_of (S.of_list [1;2;3;4;5]) = [1;2;3;4;5])

    let test_map () =
      assert (list_of (S.map string_of_int (S.of_list [1;2;3;4]))
                = ["1"; "2"; "3"; "4"]) ;
      assert (list_of (S.map (fun x -> x * x) (S.nil ())) = [])

    let test_filter () =
      assert (list_of (S.filter (fun x -> 0 = x mod 2)
                        (S.of_list [1;2;3;4;5;6])) = [2;4;6])

    let test_map_filter () =
      assert (list_of (S.map_filter (fun x ->
                if x mod 2 = 0 then Some (x / 2) else None)
                  (S.of_list [1;3;5;7;9;4;42])) = [2;21])

    let test_append () =
      let open S in
      assert (list_of (
        append
          (cons (fun () -> (1, (cons (fun () -> (2, (nil ())))))))
          (cons (fun () -> (3, (cons (fun () -> (4, (nil ())))))))) =
          [1; 2; 3; 4]) ;
      assert (list_of (append (nil ())
                        (cons (fun () -> (1, (nil ()))))) = [1]) ;
      assert (list_of (append (of_list [3; 4]) (nil ())) = [3; 4])

    let test_flatten () =
      let open S in
      assert (list_of (flatten (
        of_list [of_list [1;2]; of_list [3;4]])) = [1;2;3;4])

    let test_fold_map () =
      assert (list_of
        (S.fold_map (fun x y -> ((x, y), x + y))
            (S.of_list [1;2;3]) 0) = [(1,0); (2,1); (3,3)])

    let test_fold () =
      assert (S.fold (fun x acc -> acc ^ x)
                (S.of_list ["a"; "b"; "c"]) "" = "abc")

    let test_reverse () =
      assert (list_of (S.reverse (S.of_list [1;2;3;4])) = [4;3;2;1]) ;
      assert (list_of (S.reverse (S.nil ())) = [])

    let test_from_fun () =
      let i = ref 0 in
      let f () =
        if !i > 9 then None
        else
         (incr i ;
          Some !i) in
      assert (list_of (S.from_fun f) = [1;2;3;4;5;6;7;8;9;10])

    let test () =
      test_of_list () ;
      test_map () ;
      test_filter () ;
      test_map_filter () ;
      test_append () ;
      test_flatten () ;
      test_fold_map () ;
      test_fold () ;
      test_reverse () ;
      test_from_fun ()
  end

module Array_sequence_parameters =
  struct
    let allow_copy_on_resize = true
    let allow_copy_on_multiple_cons = true
  end
module Array_sequence_test =
  Sequence_test (Array_sequence (Array_sequence_parameters))
module List_sequence_test = Sequence_test (List_sequence)
module Lazy_list_sequence_test = Sequence_test (Lazy_list_sequence) ;;

Array_sequence_test.test () ;;
List_sequence_test.test () ;;
Lazy_list_sequence_test.test () ;;
