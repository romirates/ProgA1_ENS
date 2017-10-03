
let tesselation_extended speed draw draw_outline draw_separation divide root generations =
  (*
   * Divides the object 'root' 'generations' times, and renders each generation.
   *
   * - speed : float
   *   The minimal time in seconds between each generation's rendering.
   *
   * - draw : 'a -> ()
   *   Renders an objects.
   *
   * - draw_outline : 'a -> ()
   *   Draws an object's outline.
   *
   * - draw_separation : 'a -> ()
   *   Draws the separation between an object's children.
   * 
   * - divide : 'a -> 'a list
   *   Divides an object into a list of its children.
   *)
  
  (*Remember the triangles already visited in order to draw their children's separation.*)
  let list_of_visited_nodes = ref []
  in
  (*
   * Draws the current generation, given the list of
   * objects of this generation.
   *)
  let draw_current_gen objects =
    clear_graph();
    List.iter draw objects;
    draw_outline root;
    List.iter draw_separation !list_of_visited_nodes
  in
  (*
   * 'draw_all_gens' takes the list of objects in one generation,
   * creates the list of their children through the argument
   * 'children', then draws the children.
   * The function does this 'gen' times.
   *)
  let rec draw_all_gens generation_list children gen =
    match gen with
    | 0 ->()
    | _ ->
      begin match generation_list with
      | node::tail ->
        let new_children = divide node in
        list_of_visited_nodes := node::(!list_of_visited_nodes);
        draw_all_gens tail (new_children @ children) gen
      | _ ->
        Unix.sleepf speed;
        draw_current_gen children;
        draw_all_gens children [] (gen-1)
      end
  in
  draw_current_gen [root];
  draw_all_gens [root] [] generations
;;