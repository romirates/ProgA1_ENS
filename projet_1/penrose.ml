#use "penrose_triangle.ml"
#use "tesselation.ml"

let size = 450.
and margin = 10.
and generations = 6
;;

(*
 *********************************************************
 *                     Tests                             *
 *********************************************************
 *)

let penrose = tesselation draw_triangle_and_outline divide_triangle;;
let first_triangle = get_basic_acute_triangle size margin;;

let init_screen () =
  close_graph ();
  open_graph " 800x600-0+0"
;;

init_screen ();
penrose first_triangle generations;;
