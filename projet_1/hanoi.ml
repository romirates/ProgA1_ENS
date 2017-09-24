(*
 ***************************
 *                         *
 *       HOW TO PLAY       *
 *                         *
 ***************************
1. charge this file on the ocaml-toplevel

2. run "start_game <nb_disc>;;" 
   where nb_disc is the number of disc you want.

3. let our code spam the console.

4. run get() to know how many step it take to resolve the game.

*)

type rod = string;;

let rodA:rod = "A"
and rodB:rod = "B"
and rodC:rod = "C";;
    
(*counter *)
let count = ref 0;;
let init_counter() = (count := 0);;
let step() = (incr count);;
let get() = (!count);;


let movement (origin:rod) (destination:rod) =
  print_string
    ("I move a disc from rod " ^ origin ^
       " to rod " ^ destination);
  print_newline()
;;

(* the first argument is the source, and the 3rd the destination. *)

let rec hanoi (a:rod) (b:rod) (c:rod) = function
  |0 -> ()
  |n_disc ->
    step();
    hanoi a c b (n_disc-1);
    movement a c;
    hanoi b a c (n_disc-1)
;;

let start_game nb_disc =
  hanoi rodA rodB rodC nb_disc
;;
