type ('a,'b) t = OK of 'a | Error of 'b;;

class type virtual
      [ 'heck
      (* , 'a,'a_inh,'a_syn
      , 'b,'b_inh,'b_syn *)
      , 'type_itself
      , 'gt_a_for_a, 'gt_a_for_b
      ,'inh,'syn]
      t_meta_tt = object
  method c_OK : 'inh ->
                ( 'inh,
                  'type_itself,
                  'syn,
                  'heck) GT.a ->
                'gt_a_for_a ->
                'syn
  method c_Error :  'inh ->
                    ( 'inh,
                      'type_itself,
                      'syn,
                      'heck) GT.a ->
                    'gt_a_for_b ->
                    'syn
  (* we omitted from meta_tt a method for type itself *)
end
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
  let rec
    t_gcata
      transform_a
      transform_b
      transformer initial_inh subject =
    let rec self = t_gcata transform_a transform_b transformer
    and parameter_transforms_obj =
      object method a = transform_a method b = transform_b end
    in
    match subject with
    | OK arg0 ->
        transformer#c_OK initial_inh
          (GT.make self subject parameter_transforms_obj)
          (GT.make transform_a arg0 parameter_transforms_obj)
    | Error arg0 ->
        transformer#c_Error initial_inh
          (GT.make self subject parameter_transforms_obj)
          (GT.make transform_b arg0 parameter_transforms_obj)
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
