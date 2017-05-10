type ('a, 'b) glist = Nil | Cons of 'a * 'b
 (* [@@deriving gt] *)

class type virtual ['a,'ia,'sa,'b,'ib,'sb,'inh,'syn] glist_tt = object
  method c_Nil :
    'inh ->
    ('inh,('a,'b) glist,'syn,< a: 'ia -> 'a -> 'sa  ;b: 'ib -> 'b -> 'sb > ) GT.a ->
    'syn
  method c_Cons :
    'inh ->
    ('inh, ('a,'b) glist,'syn,< a: 'ia -> 'a -> 'sa  ;b: 'ib -> 'b -> 'sb > ) GT.a ->
    ('ia,'a,'sa,< a: 'ia -> 'a -> 'sa; b: 'ib -> 'b -> 'sb > ) GT.a ->
    ('ib,'b,'sb,< a: 'ia -> 'a -> 'sa; b: 'ib -> 'b -> 'sb > ) GT.a -> 'syn
  method t_glist :
    ('ia -> 'a -> 'sa) -> ('ib -> 'b -> 'sb) -> 'inh -> ('a,'b) glist -> 'syn
end

let (glist :
  (('ia -> 'a -> 'sa) -> ('ib -> 'b -> 'sb) ->
      ('a,'ia,'sa,'b,'ib,'sb,'inh,'syn) #glist_tt ->
      'inh -> ('a,'b) glist -> 'syn, unit)
    GT.t)
  =
  let rec glist_gcata fa fb trans inh subj =
    let rec self = glist_gcata fa fb trans
    and tpo = object method a = fa method b = fb end in
    match subj with
    | Nil  -> trans#c_Nil inh (GT.make self subj tpo)
    | Cons (p0,p1) ->
        trans#c_Cons inh (GT.make self subj tpo) (GT.make fa p0 tpo)
          (GT.make fb p1 tpo)
     in
  { GT.gcata = glist_gcata; GT.plugins = () }

class virtual ['a,'ia,'sa,'b,'ib,'sb,'inh,'syn] glist_t =
  object (this)
    method virtual c_Nil :
      'inh ->
      ('inh,('a,'b) glist,'syn,< a: 'ia -> 'a -> 'sa  ;b: 'ib -> 'b -> 'sb > )
      GT.a -> 'syn
    method virtual c_Cons :
      'inh ->
        ('inh,('a,'b) glist,'syn,<
                                   a: 'ia -> 'a -> 'sa  ;b: 'ib -> 'b -> 'sb
                                   > )
          GT.a ->
          ('ia,'a,'sa,< a: 'ia -> 'a -> 'sa  ;b: 'ib -> 'b -> 'sb   > ) GT.a
            ->
            ('ib,'b,'sb,< a: 'ia -> 'a -> 'sa  ;b: 'ib -> 'b -> 'sb   > )
              GT.a -> 'syn
    method t_glist fa fb = (GT.transform glist) fa fb this
  end
let glist = { GT.gcata = (glist.GT.gcata); GT.plugins = (object  end) }
class type virtual ['a,'ia,'sa,'b,'ib,'sb,'inh,'syn] glist_tt =
  object
    method  c_Nil :
      'inh ->
        ('inh,('a,'b) glist,'syn,<
                                   a: 'ia -> 'a -> 'sa  ;b: 'ib -> 'b -> 'sb
                                   > )
          GT.a -> 'syn
    method  c_Cons :
      'inh ->
        ('inh,('a,'b) glist,'syn,<
                                   a: 'ia -> 'a -> 'sa  ;b: 'ib -> 'b -> 'sb
                                   > )
          GT.a ->
          ('ia,'a,'sa,< a: 'ia -> 'a -> 'sa  ;b: 'ib -> 'b -> 'sb   > ) GT.a
            ->
            ('ib,'b,'sb,< a: 'ia -> 'a -> 'sa  ;b: 'ib -> 'b -> 'sb   > )
              GT.a -> 'syn
    method  t_glist :
      ('ia -> 'a -> 'sa) ->
        ('ib -> 'b -> 'sb) -> 'inh -> ('a,'b) glist -> 'syn
  end
let (glist :
  (('ia -> 'a -> 'sa) ->
     ('ib -> 'b -> 'sb) ->
       ('a,'ia,'sa,'b,'ib,'sb,'inh,'syn)#glist_tt ->
         'inh -> ('a,'b) glist -> 'syn,unit)
    GT.t)
  =
  let rec glist_gcata fa fb trans inh subj =
    let rec self = glist_gcata fa fb trans

    and tpo = object method a = fa method b = fb end
     in
    match subj with
    | Nil  -> trans#c_Nil inh (GT.make self subj tpo)
    | Cons (p0,p1) ->
        trans#c_Cons inh (GT.make self subj tpo) (GT.make fa p0 tpo)
          (GT.make fb p1 tpo)
     in
  { GT.gcata = glist_gcata; GT.plugins = () }
class virtual ['a,'ia,'sa,'b,'ib,'sb,'inh,'syn] glist_t =
  object (this)
    method virtual  c_Nil :
      'inh ->
        ('inh,('a,'b) glist,'syn,<
                                   a: 'ia -> 'a -> 'sa  ;b: 'ib -> 'b -> 'sb
                                   > )
          GT.a -> 'syn
    method virtual  c_Cons :
      'inh ->
        ('inh,('a,'b) glist,'syn,<
                                   a: 'ia -> 'a -> 'sa  ;b: 'ib -> 'b -> 'sb
                                   > )
          GT.a ->
          ('ia,'a,'sa,< a: 'ia -> 'a -> 'sa  ;b: 'ib -> 'b -> 'sb   > ) GT.a
            ->
            ('ib,'b,'sb,< a: 'ia -> 'a -> 'sa  ;b: 'ib -> 'b -> 'sb   > )
              GT.a -> 'syn
    method t_glist fa fb = (GT.transform glist) fa fb this
  end
let glist = { GT.gcata = (glist.GT.gcata); GT.plugins = (object  end) }
(* type 'a list = ('a, 'a list) glist [@@deriving gt] *)
