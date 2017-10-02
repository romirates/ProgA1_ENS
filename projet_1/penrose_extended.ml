#load "unix.cma";;
#use "penrose_triangle.ml";;
#use "tesselation_extended.ml";;

let speed = 1.5
and size = 450.
and margin = 10.
;;


 let penrose_extended =
  tesselation_extended speed
                       draw_triangle
                       draw_triangle_outline
                       draw_triangle_children_separation
                       divide_triangle
;;

let first_triangle = get_basic_acute_triangle size margin;;

 let init_screen () =
  close_graph ();
  open_graph " 800x600-0+0"
;;

init_screen ();
penrose_extended first_triangle 6;;
