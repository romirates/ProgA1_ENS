let rec tesselation draw divide root generation  =
  (*
   * Renders the object 'root' divided 'generation' times.
   *  - draw : 'a -> () : draws one object
   *  - divide : 'a -> 'a list : divides an object into a list of its children.
   *)
  let rec rec_call_on_list l = match l with
    | h::t ->
      tesselation draw divide h (generation - 1);
      rec_call_on_list t
    | _ -> ()
  in

  if generation = 0 then
    draw root
  else
    rec_call_on_list (divide root)
;;