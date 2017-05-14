open Test599
open Printf

type  'a     t2 = ('a, string) t

class type virtual ['a,'a_inh,'a_syn,'inh,'syn] t2_tt
  = object
  inherit [ < a: 'a_inh -> 'a -> 'a_syn > as 'heck
          , 'a t2
          , ( 'a_inh, 'a, 'a_syn, 'heck) GT.a
          , string
          , 'inh, 'syn] t_meta_tt
    (* method  t_t2 : ('a_inh -> 'a -> 'a_syn) ->
      'inh -> 'a t2 -> 'syn *)
  end

let t2_meta_gcata on_a_arg = t_meta_gcata on_a_arg id

let (t2 :
  ( ('a_inh -> 'a -> 'a_syn) ->
    ('a,'a_inh,'a_syn,'inh,'syn)#t2_tt ->
    'inh ->
    'a t2 ->
    'syn, unit)
    GT.t)
  =
  let rec t2_gcata transform_a
      transformer initial_inh subject =
    let parameter_transforms_obj = object method a = transform_a end in
    t2_meta_gcata
      (fun arg0 -> GT.make transform_a arg0 parameter_transforms_obj)
      parameter_transforms_obj
      transformer initial_inh subject
  in
  { GT.gcata = t2_gcata; plugins = () }

class virtual
  [ 'heck
  , 'type_itself
  , 'gt_a_for_a
  , 'inh,'syn ] t2_meta_t = object
    inherit [ 'heck, 'type_itself, 'gt_a_for_a, string
            , 'inh, 'syn] t_meta_t
end

class virtual ['a,'a_inh,'a_syn,'inh,'syn] t2_t
  : [ < a: 'a_inh -> 'a -> 'a_syn > as 'heck
    , 'a t2
    , ( 'a_inh, 'a, 'a_syn, 'heck) GT.a
    , 'inh, 'syn] t2_meta_t =
  object (this)
    method virtual  c_OK :
      'inh ->
      ( 'inh,   'a t2, 'syn, 'heck) GT.a ->
      ( 'a_inh, 'a,  'a_syn, 'heck) GT.a ->
      'syn
    method virtual  c_Error :
      'inh ->
      ( 'inh, 'a t2, 'syn, 'heck) GT.a ->
      string ->
      'syn
    (* omitted for sake of right types *)
    (* method t_t2 transform_a =
      GT.transform t2 transform_a this *)
  end

class ['a, 'a_holder] show_meta_t2 = fun for_a ->
  let for_b x = GT.lift (GT.string.GT.plugins)#show () x in
  object(this)
    inherit [ 'a, 'a_holder
            , string, string
            ] show_meta_t for_a for_b

  end

class ['a] show_result2 = object(this)
  inherit  ['a,unit,string,unit,string] t2_t
  method c_OK    () : _ -> _ GT.a -> string
    = fun _subj p0 -> sprintf "OK %s" (p0.GT.fx ())
  method c_Error () : _ -> string -> string
    = fun _subj p1 -> sprintf "Error %s"
                        (GT.lift (GT.string.GT.plugins)#show () p1)

  method t_t2 transform_a =
    GT.transform t2 transform_a this
end

let () =
  let show fa (e: _ t2) =
    t2.GT.gcata (GT.lift fa) (new show_result2) () e in
  printf "%s\n%!" (show string_of_float (OK 2.));
  printf "%s\n%!" (show string_of_int (Error "error2"));
  ()
