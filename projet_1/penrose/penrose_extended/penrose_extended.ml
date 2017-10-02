#load "graphics.cma";;
#load "unix.cma";;
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
let length_triangle = 450.0;;
let phi = 1.61803398875;;
let inv_phi = 0.61803398875;;

let first_triangle_acute =
  (Acute,
  (10. , 10.),
  (10. +. length_triangle *. phi , 10. ),
  (10. +. 1.30901699438 *. length_triangle, 10. +. 0.95105651629 *. length_triangle)
  : triangle)
;;

let first_triangle_obtuse =
  (Obtuse,
  (10. , 10.),
  (10. +. (length_triangle *. phi) , 10.),
  (10. +. phi/.2. *. length_triangle, 10. +. 0.5877852523 *.  length_triangle)
  : triangle)
;;

let speed = 1.5;;

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

let rec draw_triangle_list l = match l with
  | h::t ->
    draw h;
    draw_triangle_list t
  | _ -> ()
;;

let draw_outline (t,a,b,c:triangle) =
  (*
   * draws the triangle's outline
   *)
   draw_black_line a b;
   draw_black_line b c;
   draw_black_line c a
;;

let rec draw_inline (t,a,b,c:triangle) =
  (*
   * Takes a triangle draws the triangle's children's outlines,
   * except those that are also the parent triangle's outline.
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

(*
 *********************************************************
 *                     Algorithm                         *
 *********************************************************
 *)


let penrose tri generation =
  (*
   * Draws the Penrose tesselation one generation at a time,
   * for 'generation' generations.
   * The zeroth generation is given by the triangle 'tri'.
   *)
  
  (*Remember the triangles already visited in order to draw their "inline".*)
  let list_of_visited_triangles = ref []
  in
  (* Draws the outline of all the current generation's triangles using 
   * 'list_of_visited_triangles'
   *)
  let draw_outlines () =
    draw_outline tri;
    let rec aux l_o_v_t = match l_o_v_t with
      | h::t ->
        draw_inline h;
        aux t
      | _ -> ()
    in
    aux !list_of_visited_triangles
  in
  (*
   * Draws the current generation, given the list of triangles
   * of this generation.
   *)
  let draw_current_gen triangles =
    clear_graph();
    draw_triangle_list triangles;
    draw_outlines ();
  in
  (*
   * 'draw_all_gens' takes the list of triangle in one gen,
   * creates the list of their children, then draws the children.
   * The function does this 'gen' times.
   *)
  let rec draw_all_gens triangle_list children gen =
    match gen with
    | 0 ->()
    | _ ->
      begin match triangle_list with
      | h::t ->
        let new_children = divide h in
        list_of_visited_triangles := h::(!list_of_visited_triangles);
        draw_all_gens t (new_children @ children) gen
      | _ ->
        Unix.sleepf speed;
        draw_current_gen children;
        draw_all_gens children [] (gen-1)
      end
  in
  draw_current_gen [tri];
  draw_all_gens [tri] [] generation
;;
          
(*
 *********************************************************
 *                     Tests                             *
 *********************************************************
 *)
let start_game t_triangle generation =
  init_screen ();
  match t_triangle with
  |"acute" -> penrose first_triangle_acute generation
  |"obtuse" -> penrose first_triangle_obtuse generation
  |_ -> failwith "acute or obtuse"
;;
