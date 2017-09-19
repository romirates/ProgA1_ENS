#load "graphics.cma";;
open Graphics;;
(* a point is a couple of int *)
type point = int * int;;

type rod = color * point * point * point* point;;

type rod_content = disc list;;

type disc = int * int;;


let width = 1920
and height = 1800;;
let width_percent = width/100
and height_percent = height/100;;

let rod_height = 50*width_percent
and rod_width = 5*width_percent
and spacing = 20*width_percent;;

let max_disc_width = 20*width_percent;;
let rodA:rod = (red, (spacing,0), (spacing,rod_height), (spacing+rod_width,rod_height), (spacing+rod_width,0))
and rodB:rod = (green, (2*spacing+rod_width,0), (2*spacing+rod_width,rod_height), (2*(spacing+rod_width),rod_height), (2*(spacing+rod_width),0))
and rodC:rod = (blue,(3*spacing+2*rod_width,0),(3*spacing+2*rod_width,rod_height),
                (3*(spacing+rod_width),rod_height), (3*(spacing+rod_width),0));;

let init_screen = function () ->
                            close_graph ();
                            open_graph (" "^(string_of_int width)^"x"^(string_of_int height)^"-0+0")
;;

let draw_rod (color, a, b, c, d : rod) =
  let poly = [| a ; b ; c ; d |]
  in
  set_color color;
  fill_poly poly
;;

let init_rod () =
  draw_rod rodA;
  draw_rod rodB;
  draw_rod rodC
;;

    
(*counter *)
let count = ref 0;;
let init_counter() = (count := 0);;
let step() = (incr count);;
let get() = (!count);;
(*
let movement (origin:rod) (destination:rod) =
  print_string
    ("I move a disc from rod " ^ origin ^
       " to rod " ^ destination);
  print_newline()
;;

let rec hanoi (a:rod) (b:rod) (c:rod) = function
  |0 -> ()
  |n_disc ->
    step();
    hanoi a c b (n_disc-1);
    movement a c;
    hanoi b a c (n_disc-1)
;;
 *)
