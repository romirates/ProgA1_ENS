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
  let len = 450.0 in
  (Acute,
  (10. , 10.),
  (10.+. (len *. phi) , 10.),
  (1.30901699438 *. len +. 10., 0.95105651629 *. len +.10.)
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


let divide (t,a,b,c:triangle) =
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
 *            Printing functions                         *
 *********************************************************
 *)

let init_screen () =
  close_graph ();
  open_graph " 800x600-0+0"
;;


let draw (t,a,b,c:triangle) =
  (*
   * draws the triangle 
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
;;


let draw_outline (t,a,b,c:triangle) =
        (*
         * draws the triangle's outline
         *)
        let xa, ya = int_tuple_of_point a
        and xb, yb = int_tuple_of_point b
        and xc, yc = int_tuple_of_point c
        in
        set_color black;
        moveto xa ya;
        lineto xb yb;
        lineto xc yc;
        lineto xa ya
;;

(*
 * TODO: make this work
 * @Romain : utilise la feuille avec des gribouillis sur les triangles
 * pour voir quelles lignes sont à afficher.
 *)
let rec draw_inline (t,_,_,_:triangle) l =
  (*
   * Takes a triangle and the list of its children,
   * draws the children's outlines, except those that
   * are also the parent triangle's outline.
   *)
  match t with
  | Acute -> ()
  | Obtuse -> ()
;;

(*
 *********************************************************
 *                     Algorithm                         *
 *********************************************************
 *)


let penrose (tri : triangle) generation =
        (*
         * Draws the Penrose tessellation, the triangle will be divided generation times.
         *)
  let rec draw_triangle_list l = match l with
    | h::t -> draw h; draw_triangle_list t
    | _ -> ()
  in
  let rec aux l n = match n with
    | 0 -> draw_triangle_list l
    | _ ->
      let rec aux2 l = match l with
        | [] -> ()
        | h::t ->
           let children = divide h
           in
          aux children (n-1);
          draw_inline h children;
          aux2 t
      in
      aux2 l
  in
  aux [tri] generation;
  draw_outline tri
;;
          
(*
 *********************************************************
 *                     Tests                             *
 *********************************************************
 *)

init_screen ();
penrose first_triangle 6;;
