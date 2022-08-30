let assertTrue condition message =
  assert (condition || (print_endline message ; condition)) ;;

assertTrue (Sequence.times_three 2 = 6) "Two times three should be six!" ;;
