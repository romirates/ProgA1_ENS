#load "graphics.cma";;
open Graphics;;

(*
 *********************************************************
 *                  Types                                *
 *********************************************************
 *)

type point = float * float;;
type vector = point;;
type angle = Obtuse | Acute;;
type triangle = angle * point * point * point;;

(*
 *********************************************************
 *                  Constants                            *
 *********************************************************
 *)

let phi = 1.61803398875;;
let inv_phi = 0.61803398875;;
let first_triangle =
  let len = 500.0 in
  (Acute,
  (0. , 0.),
  (len *. phi , 0.),
  (1.30901699438 *. len , 0.95105651629 *. len)
  : triangle)
;;

(*
 *********************************************************
 *             Data manipulation functions               *
 *********************************************************
 *)

let int_tuple_of_point (x,y :point) =
  int_of_float x, int_of_float y
;;
let get_vector (xa,ya : point) (xb,yb:point) =
  (xb -. xa, yb -. ya:vector)
;;
let add_vector (x,y:point) (u,v:vector) =
  (x +. u, y +. v : point)
;;
let mult_vector scalar (u,v:vector) =
  (scalar *. u, scalar  *. v : vector)
;;

(*
 *********************************************************
 *            Printing functions                         *
 *********************************************************
 *)

let init_screen () =
  close_graph ();
  open_graph " 800x600-0+0"
;;


let draw (t,a,b,c:triangle) =
  (*
   * draws the triangle and its outline
   *)
  let xa,ya = int_tuple_of_point a
  and xb,yb = int_tuple_of_point b
  and xc,yc = int_tuple_of_point c
  in let poly = [| xa,ya ; xb,yb ; xc,yc |]
     in
     begin match t with
     |Obtuse -> set_color red
     |Acute -> set_color green
     end;
     fill_poly poly;
     set_color black;
     moveto xa ya;
     lineto xb yb;
     lineto xc yc;
     lineto xa ya;
;;


(*
 *********************************************************
 *                     Algorithm                         *
 *********************************************************
 *)


let rec divide (t,a,b,c as tri : triangle) generation  =
  (*
   * Renders the triangle divided 'generation' times.
   *)
  match generation with
  |0 -> draw tri;
  |_ ->
    let new_gen = generation - 1 in
    begin match t with
    |Obtuse ->
      let new_point = add_vector b (mult_vector inv_phi (get_vector b c))
      in
      let t1 = (Acute, b, new_point, a : triangle)
      and t2 = (Obtuse, new_point, c, a : triangle)
      in
      divide t1 new_gen;
      divide t2 new_gen;
    |Acute ->
      let new_point1 = add_vector c (mult_vector inv_phi (get_vector c a))
      and new_point2 = add_vector a (mult_vector inv_phi (get_vector a b))
      in
      let t1 = (Acute, c, new_point1, new_point2 : triangle)
      and t2 = (Acute, c, new_point2, b : triangle)
      and t3 = (Obtuse, new_point1, a, new_point2 : triangle)
      in
      divide t1 new_gen;
      divide t2 new_gen;
      divide t3 new_gen;
    end
;;
          
(*
 *********************************************************
 *                     Tests                             *
 *********************************************************
 *)

init_screen ();
divide first_triangle 6;;
