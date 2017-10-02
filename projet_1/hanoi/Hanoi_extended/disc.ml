module Disc:
sig
  type disc
  val make_disc: int -> int -> disc
  val get_width: disc -> int
  val get_height: disc -> int
end
  =
  struct
    type disc = int*int
    let make_disc (width:int) (height:int) =
      (width,height)
    ;;
    let get_width = function
      |(x,_) -> x
    ;;
    let get_height = function
      |(_,y) -> y
    ;;
  end
  
