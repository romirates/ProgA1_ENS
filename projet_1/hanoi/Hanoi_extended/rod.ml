#use "disc.ml";;
#load "graphics.cma";;
module Rod:
sig
  
  type rod
  type pos
  type content
  type shape
  type disc
  val make_shape: Graphics.color -> int -> int -> shape
  val get_shape: rod -> Graphics.color*int*int
  val make_pos: int -> int -> pos
  val get_pos: rod -> int*int
  val make_rod: pos -> shape -> content -> rod
  val make_content: int -> int -> int -> content
  val empty_content: unit -> content
  val get_content: rod -> Disc.disc list
  val pop: rod -> disc
  val push: rod -> disc -> unit    
end
  =
  struct  
    type disc = Disc.disc
    type pos = int*int
    type shape = Graphics.color*int*int
    type content = Disc.disc list
                 
    type rod =
      {
        pos: pos;
        shape: shape;
        mutable content: content;
      };;
    
    let make_rod pos shape content =
      {pos;shape;content}
    ;;
    
    let make_shape color x y =
      (color,x,y:shape)
    ;;

    let get_shape rod =
      rod.shape
    ;;
    
    let make_pos x y =
      (x, y:pos)
    ;;

    let get_pos rod =
      rod.pos
    ;;
    
    let rec make_content_acc base_width base_height (acc:content) (nb_disc:int) =
      if nb_disc = 0 then
        acc
      else
        make_content_acc base_width base_height ((Disc.make_disc (base_width*nb_disc) base_height) :: acc:content) (nb_disc - 1)
    ;;
    
    let make_content nb_disc width height =
      let base_width = width/nb_disc
      and base_height = height/nb_disc
      in
      (make_content_acc base_width base_height [] nb_disc)
    ;;

    let get_content (rod:rod) =
      rod.content
    ;;

    let empty_content () =
      []
    ;;
    let pop rod =
      let head = List.hd(rod.content) in
      rod.content <- List.tl(rod.content);
      head
    ;;

    let push rod disc =
      rod.content <- disc::rod.content
    ;;
  end
  
