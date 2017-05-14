open Printf

(* The most general type. We will concretize it in the 2nd and
   3rd snippet *)
type ('a,'b) t  = OK of 'a | Error of 'b

let id x = x

class type virtual
      [ 'heck
      , 'type_itself
      , 'gt_a_for_a, 'gt_a_for_b
      , 'inh,'syn ]
      t_meta_tt = object
  method virtual c_OK : 'inh ->
                  ( 'inh, 'type_itself, 'syn, 'heck) GT.a ->
                  'gt_a_for_a ->
                  'syn
  method c_Error :  'inh ->
                  ( 'inh, 'type_itself, 'syn, 'heck) GT.a ->
                  'gt_a_for_b ->
                  'syn
  (* we omitted from meta_tt a method for type itself *)
end

let rec t_meta_gcata on_a_arg on_b_arg parameter_transforms_obj transformer
                     initial_inh subject =
  let self = t_meta_gcata on_a_arg on_b_arg
                        parameter_transforms_obj transformer
  in

  match subject with
  | OK arg0 ->
      transformer#c_OK initial_inh
        (GT.make self subject parameter_transforms_obj)
        (on_a_arg arg0)
  | Error arg0 ->
      transformer#c_Error initial_inh
        (GT.make self subject parameter_transforms_obj)
        (on_b_arg arg0)

class type virtual ['a,'a_inh,'a_syn,'b,'b_inh,'b_syn,'inh,'syn] t_tt
  = object
    inherit [ < a: 'a_inh -> 'a -> 'a_syn
              ; b: 'b_inh -> 'b -> 'b_syn > as 'heck
            , ('a,'b) t
            , ('a_inh, 'a, 'a_syn, 'heck) GT.a
            , ('b_inh, 'b, 'b_syn, 'heck) GT.a
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

class virtual
  [ 'heck
  , 'type_itself
  , 'gt_a_for_a, 'gt_a_for_b
  , 'inh,'syn ] t_meta_t = object

  method virtual c_OK : 'inh ->
                ('inh, 'type_itself, 'syn, 'heck) GT.a ->
                'gt_a_for_a ->
                'syn
  method virtual c_Error : 'inh ->
                    ('inh, 'type_itself, 'syn, 'heck) GT.a ->
                    'gt_a_for_b ->
                    'syn
end

class virtual [ 'a, 'a_inh, 'a_syn, 'gt_a_for_a
              , 'b, 'b_inh, 'b_syn, 'gt_a_for_b
              , 'heck
              , 'inh, 'syn] t_t
  : [ (* < a: 'a_inh -> 'a -> 'a_syn; b: 'b_inh -> 'b -> 'b_syn > as*)
      'heck
    , ('a, 'b) t
    , 'gt_a_for_a
    , 'gt_a_for_b
    , 'inh, 'syn] t_meta_t =
  object (this)
    method virtual  c_OK : 'inh ->
      ( 'inh, ('a,'b) t, 'syn, 'heck) GT.a ->
      'gt_a_for_a ->
      'syn
    method virtual  c_Error : 'inh ->
      ( 'inh, ('a,'b) t, 'syn, 'heck) GT.a ->
      'gt_a_for_b ->
      'syn

    (* method t_t transform_a transform_b =
      GT.transform t transform_a transform_b this *)
  end


(* The types 'param_holder were invented to denote constructors'
  arguments. In case of polymorphic value they are (_) GT.a
  and in case of concrete arguments they are, for example, string

  TODO: check what will happend when we write alias like
  type 'a option_list = ('a option) list
  *)

class [ 'a, 'a_holder
      , 'b, 'b_holder
      , 'heck
      ] show_meta_t = fun for_a for_b -> object(this)
  inherit [ 'a,unit,string,'a_holder
          , 'b,unit,string,'b_holder
          , 'heck
          , unit, string] t_t
  method c_OK    () : _ -> 'a_holder -> string
    = fun _subj p0 -> sprintf "OK %s" (for_a p0)
  method c_Error () : _ -> 'b_holder -> string
    = fun _subj p1 -> sprintf "Error %s" (for_b p1)

  (* moved from t_t virtual class *)
  (* commented because breaks inference of 'b_holder in t2_meta_t *)
  (* method t_t transform_a transform_b =
    GT.transform t transform_a transform_b this *)
end

class ['a, 'b] show_result = object(this)
  inherit [ 'a, (unit, 'a, string, 'heck) GT.a
          , 'b, (unit, 'b, string, 'heck) GT.a
          , < a: unit -> 'a -> string; b: unit -> 'b -> string > as 'heck
          ] show_meta_t  (fun pa -> pa.GT.fx ())
                         (fun pa -> pa.GT.fx ())

  (* seems the only decent place for applying transformer *)
  method t_t transform_a transform_b =
    GT.transform t transform_a transform_b this
end

let () =
  let show fa fb (e: (_,_) t) =
    t.GT.gcata (GT.lift fa) (GT.lift fb) (new show_result) () e in
  printf "%s\n%!" (show string_of_int id (OK 1));
  printf "%s\n%!" (show string_of_int id (Error "error1"));
  ()
