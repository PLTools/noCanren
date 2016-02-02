open Printf

let make_buffer () = Buffer.create 20
let bprintf b = Printf.ksprintf (Buffer.add_string b)

module type SHOW = sig
    type t
    val show : t -> string
end

let show {S : SHOW} x = S.show x;;

implicit module Show_float = struct
    type t = float
    let show x = sprintf "%f" x
end

module Show_list_impl {X : SHOW} = struct
    type t = X.t list
    let show xs =
      let b = Buffer.create 30 in
      bprintf b "Show_list: [";
      List.iter (fun x -> bprintf b "%s" @@ X.show x) xs;
      bprintf b "]%!"
end

implicit module Show_list = Show_list_impl

implicit module Show_int = struct
    type t = int
    let show x = sprintf "%d" x
end

implicit module Show_string = struct
    type t = string
    let show x = x
end
