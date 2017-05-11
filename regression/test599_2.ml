type 'a    t2 = OK of 'a | Error of string;;

class type virtual ['a,'a_inh,'a_syn,'inh,'syn] t2_tt
  = object
    method  c_OK :
      'inh ->
      ( 'inh,
        'a t2,
        'syn,
        <
          a: 'a_inh -> 'a -> 'a_syn
        > ) GT.a ->
      ( 'a_inh, 'a, 'a_syn,
        <
          a: 'a_inh -> 'a -> 'a_syn
        > ) GT.a ->
      'syn
    method  c_Error :
      'inh ->
      ( 'inh,
        'a t2,
        'syn,
        < a: 'a_inh -> 'a -> 'a_syn
        > ) GT.a ->
      string ->
      'syn
    method  t_t2 :
      ('a_inh -> 'a -> 'a_syn) ->
      'inh ->
      'a t2 ->
      'syn
  end

let (t2 :
  ( ('a_inh -> 'a -> 'a_syn) ->
    ('a,'a_inh,'a_syn,'inh,'syn)#t2_tt ->
    'inh ->
    'a t2 ->
    'syn)
    GT.t)
  =
  let rec
    t2_gcata
      transform_a
      transformer initial_inh subject =
    let rec self = t2_gcata transform_a transformer
    and parameter_transforms_obj = object method a = transform_a end
    in
    match subject with
    | OK arg0 ->
        transformer#c_OK initial_inh
          (GT.make self subject parameter_transforms_obj)
          (GT.make transform_a arg0 parameter_transforms_obj)
    | Error arg0 ->
        transformer#c_Error initial_inh
          (GT.make self subject parameter_transforms_obj)
          arg0
     in
  { GT.gcata = t2_gcata }

class virtual ['a,'a_inh,'a_syn,'inh,'syn] t2_t =
  object (this)
    method virtual  c_OK :
      'inh ->
      ( 'inh,
        'a t2,
        'syn,
        <
          a: 'a_inh -> 'a -> 'a_syn
        > ) GT.a ->
      ( 'a_inh,
        'a,
        'a_syn,
        <
          a: 'a_inh -> 'a -> 'a_syn
        > ) GT.a ->
      'syn
    method virtual  c_Error :
      'inh ->
      ( 'inh,
        'a t2,
        'syn,
        <
          a: 'a_inh -> 'a -> 'a_syn
        > ) GT.a ->
        string ->
        'syn
    method t_t2 transform_a =
      GT.transform t2 transform_a this
  end
