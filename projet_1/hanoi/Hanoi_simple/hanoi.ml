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
(*counter *)
let count = ref 0;;
let init_counter() = (count := 0);;
let step() = (incr count);;
let get() = (!count);;



type rod = string;;

let rodA:rod = "A"
and rodB:rod = "B"
and rodC:rod = "C";;

let movement (origin:rod) (destination:rod) =
  print_string
    ("I move a disc from rod " ^ origin ^
       " to rod " ^ destination);
  print_newline()
;;

let rec hanoi (nb_disc:int) (a:rod) (b:rod) (c:rod) = 
    if nb_disc = 0 then ()
    else
      begin
        hanoi a c b (n_disc-1);
        movement a c;
        hanoi b a c (n_disc-1)
      end
;;

let start_game nb_disc =
  hanoi rodA rodB rodC nb_disc
;;
