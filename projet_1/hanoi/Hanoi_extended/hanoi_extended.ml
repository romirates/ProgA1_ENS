(*
          Hanoi extended 
 ***************************
 *                         *
 *       HOW TO PLAY       *
 *                         *
 ***************************
1. charge this file on the ocaml-toplevel

2. run "start_game <nb_disc>;;" 
   where nb_disc is the number of disc you want.

3. admire 

4. run get() to know how many step it took to complete the game.

Things you might want to change :

- height and width : screen's proportion

- speed_game : lower is faster

*)

#load "unix.cma";;
#use "rod.ml";;
#use "io_hanoi.ml";;

(*

 *********************************************************
 *                  Constants                            *
 *********************************************************
 *)

let width = 1920
and height = 1800;;
let width_percent = width/100
and height_percent = height/100;;
let game_speed = 0.1;;

let rod_height = 30*height_percent
and rod_width = width_percent
and spacing = 18*width_percent;;
let max_disc_width = 10*width_percent;;

let rodA_pos : Rod.pos = Rod.make_pos (spacing+rod_width/2) 0
and rodB_pos : Rod.pos = Rod.make_pos (2*spacing+rod_width+rod_width/2) 0
and rodC_pos : Rod.pos = Rod.make_pos (3*spacing+2*rod_width+rod_width/2) 0;;

let rodA_shape : Rod.shape = Rod.make_shape Graphics.red rod_width rod_height
and rodB_shape : Rod.shape = Rod.make_shape Graphics.blue rod_width rod_height
and rodC_shape : Rod.shape = Rod.make_shape Graphics.green rod_width rod_height

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

let init_board nb_disc =
  let first_pile = Rod.make_content nb_disc max_disc_width rod_height
  in
  let rodA : Rod.rod = Rod.make_rod rodA_pos rodA_shape first_pile
  and rodB : Rod.rod = Rod.make_rod rodB_pos rodB_shape (Rod.empty_content())
  and rodC : Rod.rod = Rod.make_rod rodC_pos rodC_shape (Rod.empty_content())
  in
  (rodA, rodB, rodC)
;;

(*
 *********************************************************
 *                     Algorithm                         *
 *********************************************************
 *)

let movement (origin:Rod.rod) (destination:Rod.rod) =
  Rod.push destination (Rod.pop origin) 
;;

(*
hanoi :

resolve the hanoi game with 3 tower and "nb_disc" disc
 *)
let rec hanoi (a:Rod.rod) (b:Rod.rod) (c:Rod.rod) (nb_disc:int) =
  if nb_disc = 0 then
    ()
  else
    begin
      step();
      hanoi a c b (nb_disc-1);
      movement a c;
      IO_hanoi.draw_board [a; b; c];
      Unix.sleepf(game_speed);
      hanoi b a c (nb_disc-1)
    end
;;

(*
 *********************************************************
 *            Obvious  functions                         *
 *********************************************************
 *)

let start_game nb_disc =
  IO_hanoi.init_screen width height;
  let (rodA, rodB, rodC) = init_board nb_disc
  in
  IO_hanoi.draw_board [rodA; rodB; rodC];
  Unix.sleep(1);
  hanoi rodA rodB rodC nb_disc 
;;
