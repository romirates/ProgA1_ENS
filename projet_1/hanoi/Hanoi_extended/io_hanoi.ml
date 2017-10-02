#load "graphics.cma";;
#use "rod.ml";;

module IO_hanoi:
sig
  val init_screen: int -> int -> unit
  val draw_board: Rod.rod list -> unit
end
  =
  
  struct
    
    let init_screen (width:int) (height:int) =
      Graphics.open_graph (" "^(string_of_int width)^"x"^(string_of_int height)^"0+0");
      Graphics.clear_graph();
      (Graphics.auto_synchronize false)
    ;;
    let rec draw_disc x disc_no = function
      |[] -> ()
      |(hd::tl) ->
        let width = Disc.get_width hd
        and height = Disc.get_height hd
        in
        Graphics.fill_rect (x-width/2) (height*(disc_no-1)) width height;       draw_disc x (disc_no - 1) tl
    ;;

    let draw_rod_shape x y (color, width, height) =
      Graphics.set_color color;
      Graphics.fill_rect (x-width/2) y width height
    ;;
    
    let draw_rod (rod:Rod.rod) =
      let (x,y) = Rod.get_pos rod
      and shape = Rod.get_shape rod
      and pile = Rod.get_content rod
      in
      draw_rod_shape x y shape;
      Graphics.set_color Graphics.black;
      draw_disc x (List.length pile) pile
    ;;
    
    let rec draw_board = function
      |[] -> Graphics.synchronize ();
             Graphics.clear_graph ()
      |h::t -> draw_rod h;
               draw_board t
    ;;
  end
