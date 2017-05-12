open Test599meta
open Printf

class type virtual ['a,'a_inh,'a_syn,'b,'b_inh,'b_syn,'inh,'syn] t_tt
  = object
    inherit [ < a: 'a_inh -> 'a -> 'a_syn
              ; b: 'b_inh -> 'b -> 'b_syn >
            , ('a,'b) t
            , ( 'a_inh, 'a, 'a_syn,
                <
                  a: 'a_inh -> 'a -> 'a_syn
                ; b: 'b_inh -> 'b -> 'b_syn
                > ) GT.a
            , ( 'b_inh,
                'b,
                'b_syn,
                < a: 'a_inh -> 'a -> 'a_syn
                ; b: 'b_inh -> 'b -> 'b_syn
                > ) GT.a
            , 'inh,'syn] t_meta_tt

    method  t_t :
      ('a_inh -> 'a -> 'a_syn) ->
      ('b_inh -> 'b -> 'b_syn) ->
      'inh ->
      ('a,'b) t ->
      'syn
  end

let (t :
  ( ('a_inh -> 'a -> 'a_syn) ->
    ('b_inh -> 'b -> 'b_syn) ->
    ('a,'a_inh,'a_syn,'b,'b_inh,'b_syn,'inh,'syn)#t_tt ->
    'inh ->
    ('a,'b) t ->
    'syn, unit)
    GT.t)
  =
  let t_gcata transform_a transform_b
              transformer initial_inh subject =
    let parameter_transforms_obj =
      object method a = transform_a method b = transform_b end in
    t_meta_gcata
      (fun arg0 -> GT.make transform_a arg0 parameter_transforms_obj)
      (fun arg0 -> GT.make transform_b arg0 parameter_transforms_obj)
      parameter_transforms_obj
      transformer initial_inh subject
  in

  { GT.gcata = t_gcata; plugins = () }

class virtual ['a,'a_inh,'a_syn,'b,'b_inh,'b_syn,'inh,'syn] t_t =
  object (this)
    method virtual  c_OK :
      'inh ->
      ( 'inh,
        ('a,'b) t,
        'syn,
        <
          a: 'a_inh -> 'a -> 'a_syn
        ; b: 'b_inh -> 'b -> 'b_syn
        > ) GT.a ->
      ( 'a_inh,'a,'a_syn,
        <
          a: 'a_inh -> 'a -> 'a_syn
        ; b: 'b_inh -> 'b -> 'b_syn
        > ) GT.a ->
      'syn
    method virtual  c_Error :
      'inh ->
      ( 'inh,
        ('a,'b) t,
        'syn,
        <
          a: 'a_inh -> 'a -> 'a_syn
        ; b: 'b_inh -> 'b -> 'b_syn
        > ) GT.a ->
      ( 'b_inh,
        'b,
        'b_syn,
        <
          a: 'a_inh -> 'a -> 'a_syn
        ; b: 'b_inh -> 'b -> 'b_syn
        > ) GT.a ->
      'syn
    method t_t transform_a transform_b =
      GT.transform t transform_a transform_b this
  end


let generic_show impl fok ferr () e =
  match e with
  | OK s    -> impl#c_OK    () fok s
  | Error e -> impl#c_Error () ferr e

class ['a, 'b] show_meta_t = fun for_a for_b -> object (this)

end
class ['a, 'b] show_result = object(this)
  inherit  ['a,unit,string,'b,unit,string,unit,string] t_t
  method c_OK    () : _ -> _ -> string
    = fun _subj p0 -> sprintf "OK %s" (p0.GT.fx ())
  method c_Error () :
    _ -> _ -> string
    = fun _subj p1 -> sprintf "Error %s" (p1.GT.fx ())
  method qqqq fa fb : unit -> ('a, 'b) t -> string = fun ()  ->
    GT.transform t fa fb this ()
  (* method t_t fa fb : unit -> ('a,'b) t -> string = fun () x ->
    GT.transform t (GT.lift fa) (GT.lift fb) this () x *)
end

let () =
  let show fa fb (e: (_,_) t) =
    t.GT.gcata (GT.lift fa) (GT.lift fb) (new show_result) () e in
  printf "%s\n%!" (show string_of_int id (OK 1));
  printf "%s\n%!" (show string_of_int id (Error "asdf"));
  ()
