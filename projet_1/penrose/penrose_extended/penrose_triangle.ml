(*
 * This file defines several types, constants and functions
 * manipulating and rendering the triangles of Penrose's tesselation.
 *
 *)

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

let phi = (1. +. sqrt(5.)) /. 2.;;
let inv_phi = 1. /. phi;;

(*
 *********************************************************
 *             Data manipulation functions               *
 *********************************************************
 *)

let get_basic_acute_triangle size margin =
  (Acute,
  (margin , margin),
  (margin+. (size *. phi) , margin),
  (1.30901699438 *. size +. margin, 0.95105651629 *. size +. margin)
  : triangle)
;;

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

let divide_triangle (t,a,b,c:triangle) =
  (*
   * Returns the list of the triangle's children 
   *)
  match t with
  | Acute ->
    let new_point1 = add_vector c (mult_vector inv_phi (get_vector c a))
    and new_point2 = add_vector a (mult_vector inv_phi (get_vector a b))
    in
    [
      (Acute, c, new_point1, new_point2 : triangle);
      (Acute, c, new_point2, b : triangle);
      (Obtuse, new_point1, a, new_point2 : triangle)
    ]
  | Obtuse ->
    let new_point = add_vector b (mult_vector inv_phi (get_vector b c)) in
    [
      (Acute, b, new_point, a : triangle);
      (Obtuse, new_point, c, a : triangle)
    ] 
;;

(*
 *********************************************************
 *                   Rendering                           *
 *********************************************************
 *)

let draw_black_line a b =
  (*
   * draws a black line between points a and b.
   *)
  let xa, ya = int_tuple_of_point a
  and xb, yb = int_tuple_of_point b
  in
  set_color black;
  moveto xa ya;
  lineto xb yb
;;

let triangle_color (t,_,_,_ : triangle) =
  (*
   * Sets the color according to the triangle's type.
   *)
  match t with
  | Obtuse -> set_color red
  | Acute -> set_color green
;;

let draw_triangle (t,a,b,c as tri : triangle) =
  (*
   * Draws the triangle. 
   *)
  let xa,ya = int_tuple_of_point a
  and xb,yb = int_tuple_of_point b
  and xc,yc = int_tuple_of_point c
  in let poly = [| xa,ya ; xb,yb ; xc,yc |]
  in
  triangle_color tri;
  fill_poly poly
;;

let draw_triangle_outline (_,a,b,c:triangle) =
  (*
   * draws the triangle's outline
   *)
   draw_black_line a b;
   draw_black_line b c;
   draw_black_line c a
;;

let draw_triangle_and_outline t =
  draw_triangle t;
  draw_triangle_outline t
;;

let rec draw_triangle_children_separation (t,a,b,c:triangle) =
  (*
   * Draws lines separation this triangle's children.
   *)
  match t with
   | Acute ->
     let new_point1 = add_vector c (mult_vector inv_phi (get_vector c a))
     and new_point2 = add_vector a (mult_vector inv_phi (get_vector a b))
     in
     draw_black_line new_point1 new_point2;
     draw_black_line new_point2 c
   | Obtuse ->
     let new_point = add_vector b (mult_vector inv_phi (get_vector b c))
     in
     draw_black_line a new_point
;;