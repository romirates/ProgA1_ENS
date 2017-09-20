#load "graphics.cma";;
open Graphics;;
(* a point is a couple of int *)
type point = int * int;;
type disc = int * int;;

type rod_shape = color * point * point * point* point;;
type rod_content = disc list;;

type rod = rod_content * rod_shape;;

type game = rod*rod*rod;;


let width = 1920
and height = 1800;;
let width_percent = width/100
and height_percent = height/100;;

let nb_disc = 10;;
let rod_height = 50*width_percent
and rod_width = 5*width_percent
and spacing = 20*width_percent;;

let max_disc_width = 40*width_percent;;

let rod_shapeA:rod_shape = (red, (spacing,0), (spacing,rod_height), (spacing+rod_width,rod_height), (spacing+rod_width,0))
and rod_shapeB:rod_shape = (green, (2*spacing+rod_width,0), (2*spacing+rod_width,rod_height),
                            (2*(spacing+rod_width),rod_height), (2*(spacing+rod_width),0))
and rod_shapeC:rod_shape = (blue,(3*spacing+2*rod_width,0),(3*spacing+2*rod_width,rod_height),
                (3*(spacing+rod_width),rod_height), (3*(spacing+rod_width),0));;


let init_screen () =
  close_graph ();
  open_graph (" "^(string_of_int width)^"x"^(string_of_int height)^"-0+0")
;;

let draw_rod (color, a, b, c, d : rod_shape) =
  let poly = [| a ; b ; c ; d |]
  in
  set_color color;
  fill_poly poly
;;

let init_rod (content, shape : rod) =
  draw_rod shape
;;
let start_game() =
  let game = init_board nb_disc
  in
  draw_game game;
  hanoi game nb_disc
;;

let init_board nb_disc =
  let base_width = max_width_disc/nb_disc and base_height = rod_height/nb_disc
  in
  let (disc_pile:rod_content) = construct_disc (base_width base_height nb_disc)
  in
  let rodA:rod = (disc_pile,rodA_shape)
  and rodB:rod = ([],rodB_shape)
  and robC:rod = ([],rodC_shape)
  in
  (rodA,rodB,rodC:game)
;;
        
let rec construct_disc base_width base_height = function
  |0 -> []
  |nb_disc -> let disc = (base_width*nb_disc,base_height)
              in
              disc::(constuct_disc(base_width base_height (nb_disc-1)))
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
