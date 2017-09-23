#load "graphics.cma";;
#load "unix.cma";;
open Graphics;;

(*
 *********************************************************
 *                  Types                                *
 *********************************************************
*)

type point = int * int;;

type disc = int * int;;

type rod_pos = int * int;;
type rod_shape = color * point * point * point* point;;
type rod_content = disc list;;

type rod =
  {
    pos : rod_pos;
    shape : rod_shape;
    mutable content : rod_content;
  };;

(*
 *********************************************************
 *                  Constants                            *
 *********************************************************
 *)

let width = 1920
and height = 1800;;
let width_percent = width/100
and height_percent = height/100;;
let game_speed = 0.5;;

let rod_height = 30*width_percent
and rod_width = width_percent
and spacing = 18*width_percent;;
let max_disc_width = 10*width_percent;;

let rodA_shape : rod_shape = (red, (spacing,0), (spacing,rod_height), (spacing+rod_width,rod_height), (spacing+rod_width,0))
and rodB_shape : rod_shape = (green, (2*spacing+rod_width,0), (2*spacing+rod_width,rod_height),
                              (2*(spacing+rod_width),rod_height), (2*(spacing+rod_width),0))
and rodC_shape : rod_shape = (blue,(3*spacing+2*rod_width,0),(3*spacing+2*rod_width,rod_height),
                              (3*(spacing+rod_width),rod_height), (3*(spacing+rod_width),0));;

let rodA_pos : rod_pos = (spacing+rod_width/2,0)
and rodB_pos : rod_pos = (2*spacing+rod_width+rod_width/2,0)
and rodC_pos : rod_pos = (3*spacing+2*rod_width+rod_width/2,0);;


(*A simple counter *)
let count = ref 0;;
let init_counter() = (count := 0);;
let step() = (incr count);;
let get() = (!count);;
(*----------------------------------*)

(*
 *********************************************************
 *            Printing functions                         *
 *********************************************************
 *)

let init_screen () =
  open_graph (" "^(string_of_int width)^"x"^(string_of_int height)^"-0+0");
  clear_graph()
;;

let draw_rod_shape (color, a, b, c, d : rod_shape) =
  let poly = [| a ; b ; c ; d |]
  in
  set_color color;
  fill_poly poly
;;

let rec draw_disc (x,y : rod_pos) nb_disc = function
  |[] -> ()
  |(width,height)::l ->
    let poly = [|(x-width, (nb_disc-1)*height) ; (x-width, nb_disc*height) ;
                 (x+width, nb_disc*height) ; (x+width, (nb_disc-1)*height) |]
    in
    fill_poly poly;
    draw_disc (x,y : rod_pos) (nb_disc - 1) l;
;;

let draw_rod (rod : rod) =
  draw_rod_shape rod.shape;
  let length_pile = List.length rod.content
  in
  set_color black;
  draw_disc rod.pos length_pile rod.content
;;

let draw_game rodA rodB rodC =
  draw_rod rodA;
  draw_rod rodB;
  draw_rod rodC
;;
(*
 *********************************************************
 *            Initialization functions                   *
 *********************************************************
 *)

(*
init_pile_of_disc :

used to construct the pile of disc from the smaller to the the bigger one 
*)

let rec init_pile_of_disc base_width base_height disc_no = function
  |0 -> []
  |nb_disc -> let disc = (disc_no*base_width, base_height)
              in
              disc::(init_pile_of_disc base_width base_height (disc_no+1) (nb_disc-1))
;;

(*
init_board :

used to create all the elements of the hanoi towers 
 *)

let init_board nb_disc =
  let base_width = max_disc_width/nb_disc
  and base_height = rod_height/nb_disc
  in
  
  (* it work with the position of each disc with the aim to avoid division 
     and thus numerical imprecision *)

  let (disc_pile : rod_content) = init_pile_of_disc base_width base_height 1 nb_disc
  in
  let rodA : rod = init_rod rodA_pos rodA_shape disc_pile
  and rodB : rod = init_rod rodB_pos rodB_shape []
  and rodC : rod = init_rod rodC_pos rodC_shape []

  in
  (rodA, rodB, rodC)
;;

(*
 *********************************************************
 *                     Algorithm                         *
 *********************************************************
 *)

let movement (origin:rod) (destination:rod) =
  let origin_hd = List.hd origin.content
  in
  origin.content <- List.tl origin.content;
  destination.content <- origin_hd::destination.content
;;

let rec hanoi (a:rod) (b:rod) (c:rod) = function
  |0 -> ()
  |n_disc ->
    step();
    hanoi a c b (n_disc-1);
    movement a c;
    clear_graph();
    draw_game a b c;
    Unix.sleepf(game_speed);
    hanoi b a c (n_disc-1)
;;

(*
 *********************************************************
 *            Obvious  functions                         *
 *********************************************************
 *)

let start_game nb_disc =
  init_screen();
  let (rodA, rodB, rodC) = init_board nb_disc
  in
  draw_game rodA rodB rodC;
  Unix.sleep(1);
  hanoi rodA rodB rodC nb_disc 
;;
