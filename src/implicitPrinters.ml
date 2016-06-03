open Printf

let make_buffer () = Buffer.create 20
let bprintf b = Printf.ksprintf (Buffer.add_string b)

module type SHOW = sig
    type t
    val show : t -> string
end

let show {S : SHOW} x = S.show x;;

implicit module Show_float: (SHOW with type t = float) = struct
    type t = float
    let show x = sprintf "%f" x
end

module Show_list_explicit {X : SHOW}: (SHOW with type t=X.t list) = struct
    type t = X.t list
    let show xs =
      let b = Buffer.create 30 in
      bprintf b "Show_list: [";
      List.iter (fun x -> bprintf b "%s" @@ X.show x) xs;
      bprintf b "]%!";
      Buffer.contents b
end

implicit module Show_list = Show_list_explicit

module Show_option_explicit {X : SHOW}: (SHOW with type t=X.t option) = struct
    type t = X.t option
    let show = function
      | None -> "None"
      | Some x -> sprintf "Some (%s)" (X.show x)
end

implicit module Show_option = Show_option_explicit

implicit module Show_int : (SHOW with type t = int) = struct
    type t = int
    let show x = sprintf "%d" x
end

implicit module Show_bool : (SHOW with type t = bool) = struct
    type t = bool
    let show x = sprintf "%b" x
end

implicit module Show_string : (SHOW with type t = string) = struct
    type t = string
    let show x = x
end

implicit module Show_pair {X : SHOW} {Y : SHOW}: (SHOW with type t = X.t * Y.t) = struct
    type t = X.t * Y.t
    let show (x,y) = sprintf "(%s,%s)" (X.show x) (Y.show y)
end

module Show_pair_explicit (X : SHOW) (Y : SHOW): (SHOW with type t = X.t * Y.t) = struct
    type t = X.t * Y.t
    let show (x,y) = sprintf "(%s,%s)" (X.show x) (Y.show y)
end


implicit module Show_tiplet {X : SHOW} {Y : SHOW} {Z : SHOW}:
   (SHOW with type t = X.t * Y.t * Z.t) =
struct
    type t = X.t * Y.t * Z.t
    let show (x,y,z) = sprintf "(%s,%s,%s)" (X.show x) (Y.show y) (Z.show z)
end

module Show_tiplet_explicit (X : SHOW) (Y : SHOW) (Z : SHOW):
   (SHOW with type t = X.t * Y.t * Z.t) =
struct
    type t = X.t * Y.t * Z.t
    let show (x,y,z) = sprintf "(%s,%s,%s)" (X.show x) (Y.show y) (Z.show z)
end
