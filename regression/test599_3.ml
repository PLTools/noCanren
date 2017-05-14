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

  (* method t_t3 : 'inh -> t3 -> 'syn *)
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

class virtual ['inh, 'syn] t3_t =
  object (this)
    method virtual c_OK :
      'inh -> ( 'inh, t3, 'syn, < > ) GT.a -> concrete_a ->    'syn
    method virtual c_Error :
      'inh -> ( 'inh, t3, 'syn, < > ) GT.a ->     string ->    'syn
  end

class show_meta_t3 =
  let for_a = GT.lift (GT.char.GT.plugins)#show () in
  object
    inherit [ concrete_a, concrete_a
            ] show_meta_t2 for_a
  end

class ['a] show_result3 = object(this)
  inherit  [unit, string] t3_t
  method c_OK    () : _ -> _ -> string
    = fun _subj p0 -> sprintf "OK '%s'"
                        (GT.lift (GT.char.GT.plugins)#show () p0)
  method c_Error () : _ -> _ -> string
    = fun _subj p1 -> sprintf "Error %s"
                        (GT.lift (GT.string.GT.plugins)#show () p1)

  (* The only appropriate place for this is here *)
  method t_t3  = GT.transform t3  this
end

let () =
  let show (e: t3) = t3.GT.gcata (new show_result3) () e in
  printf "%s\n%!" (show (OK '3'));
  printf "%s\n%!" (show (Error "error3"));
  ()
