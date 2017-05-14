open Printf
open Test599_1
open Test599_2

type concrete_a = char
type t3 = concrete_a t2

class type virtual ['inh, 'syn] t3_tt
  = object
  inherit [ < >
          , t3
          , concrete_a
          , string
          , 'inh, 'syn] t_meta_tt

end

let t3_meta_gcata x = t2_meta_gcata id x

let (t3 :
  ( ('inh,'syn)#t3_tt ->
    'inh ->
    t3 ->
    'syn, unit)
    GT.t)
  =
  let rec t3_gcata transformer initial_inh subject =
    let parameter_transforms_obj = object end in
    t3_meta_gcata
      parameter_transforms_obj
      transformer initial_inh subject
  in
  { GT.gcata = t3_gcata; plugins = () }

class virtual [ 'heck
              , 'type_itself
              , 'inh,'syn ] t3_meta_t = object
  inherit [ 'heck, 'type_itself, concrete_a
          , 'inh, 'syn] t2_meta_t
end
class virtual ['inh, 'syn] t3_t
  : [ <  > as 'heck, t3, 'inh, 'syn] t3_meta_t =
  object (this)
    inherit ['heck, t3, 'inh, 'syn] t3_meta_t
  end

class ['heck] show_meta_t3 =
  let for_a = GT.lift (GT.char.GT.plugins)#show () in
  object
    inherit [ 'heck, concrete_a, concrete_a ] show_meta_t2 for_a
  end

class ['a] show_t3 = object(this)
  inherit [ <  > as 'heck] show_meta_t3

  method t_t3  = GT.transform t3 this
end

let () =
  let show (e: t3) = t3.GT.gcata (new show_t3) () e in
  printf "%s\n%!" (show (OK '3'));
  printf "%s\n%!" (show (Error "error3"));
  ()
