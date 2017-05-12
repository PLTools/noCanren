type ('a,'b) t  = OK of 'a | Error of 'b
type  'a     t2 = ('a, string) t

let id x = x

class type virtual
      [ 'heck
      , 'type_itself
      , 'gt_a_for_a, 'gt_a_for_b
      , 'inh,'syn]
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

let rec t_meta_gcata (* transform_a transform_b *)
                    on_a_arg on_b_arg
                    parameter_transforms_obj
                    transformer initial_inh subject =
  let self = t_meta_gcata (* transform_a transform_b *)
                        on_a_arg on_b_arg
                        parameter_transforms_obj
                        transformer
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
