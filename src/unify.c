#include <stdio.h>
#include <assert.h>

#include <caml/mlvalues.h>
#include <caml/callback.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/fail.h>

#define Val_none Val_int(0)
#define caml_alloc_some() caml_alloc_small(1,0)
#define caml_alloc_cons() caml_alloc_small(2,0)

static value * lookup = NULL;
// extend (index, var, term subst) returns always Subst.t
static value * extend = NULL;
static value * real_anchor = NULL;

CAMLprim value
caml_unify_preload_stuff(value _unit)
{
  CAMLparam1(_unit);
  if (lookup == NULL) {
    lookup = caml_named_value("Subst.lookup");
    assert(lookup != NULL);
  }
  caml_register_global_root(lookup);

  if (extend == NULL) {
    extend = caml_named_value("Subst.extend");
    assert(extend != NULL);
  }
  caml_register_global_root(extend);

  if (real_anchor == NULL) {
    real_anchor = caml_named_value("global_anchor");
    assert(real_anchor != NULL);
  }
  caml_register_global_root(real_anchor);
  assert(Is_block(*real_anchor));

  CAMLreturn(Val_unit);
}

static value env_anchor = Val_int(-1);
static value cur_scope  = Val_int(-1000);

int is_var(value _term)
{
  CAMLparam1(_term);
  CAMLlocal2(_anchor, _real_anchor);

  if (Is_long(_term))
    CAMLreturnT(int, 0);
  if (Tag_val(_term) != 0 )
    CAMLreturnT(int, 0);
  // now we need to check that it is a variable

  _anchor = Field(_term, 0);
  if (_anchor != *real_anchor)
    CAMLreturnT(int, 0);

  // We skip chekcing with the env
  // it should be a list with
  if ( !Is_long(Field(_term,1)) || (Field(_term,1) != env_anchor) ) {
    caml_failwith("OCanren fatal (Env.var): wrong environment");
    CAMLreturnT(int, 0);
  }

  CAMLreturnT(int, 1);
}

#define OK 0
#define FAIL 1

int get_var_idx(value _term)
{
  CAMLparam1(_term);
  CAMLreturnT(int, Int_val(Field(_term, 2)) );
}

value
caml_walk(value _subst, value _term)
{
  CAMLparam2(_subst,_term);
  if (is_var(_term)) {
    if (Is_long(Field(_term,3))) {
      int idx = get_var_idx(_term);
      value _maybe_term = caml_callback2_exn(*lookup, _subst, Val_int(idx));
      if (Is_exception_result(_maybe_term))
        CAMLreturn(_term);

      // we take second element of pair which will be a term
      // TODO: return from OCaml only the term
      CAMLreturn( caml_walk(_subst, Field(_maybe_term, 1)) );
    } else {
      // there is inline substitution
      CAMLreturn( caml_walk(_subst, Field(Field(_term,3), 0)) );
    }
  } else {
    CAMLreturn(_term);
  }
}

// fails when the check wasn't passed
int occurs_check(value _subst, value _logicVar, int varIdx)
{
  CAMLparam2(_subst, _logicVar);
  CAMLlocal1(_term);

  _term = caml_walk(_subst, _logicVar);
  if (is_var(_term)) {
    if (get_var_idx(_term) == varIdx )
      CAMLreturnT(int, FAIL);
    else
      CAMLreturnT(int, OK);
  } else {
    if (Is_long(_term))
      CAMLreturnT(int, OK);
    // it is a block now
    int thetag = Tag_val(_term);
    if (thetag == String_tag)
      CAMLreturnT(int, OK);
    if (thetag >=0 && thetag <= 245) {
      unsigned size = Wosize_val(_term);
      for (unsigned i=0; i<size; ++i) {
        if (FAIL == occurs_check(_subst, Field(_term,i), varIdx) )
          CAMLreturnT(int, FAIL);
      }
      CAMLreturnT(int, OK);
    }
    CAMLreturnT(int, FAIL);
  }
  fprintf(stderr,"should not happen\n");
  CAMLreturnT(int, FAIL);
}

int do_extend(value *new_prefix, value *new_subst,
  int key_idx, value _key, value _term)
{
  CAMLparam0();
  CAMLlocal5(_subst, _prefix, _cnt, _var_scope, _inline_term);

  if (FAIL == occurs_check(*new_subst, _term, key_idx) ) {
    CAMLreturnT(int,FAIL);
  }

  // now we do cons in C
  _cnt = caml_alloc_tuple(2);
  Store_field( _cnt, 0, _key);              // var
  Store_field( _cnt, 1, _term);              // term

  _var_scope = Field(_key, 4);
  if (_var_scope == cur_scope) {
    // set-var-val! optimization
    _inline_term = caml_alloc_some();
    Store_field( _inline_term, 0, _term);     // head

    Store_field( _key, 3, _inline_term);     // head

    _subst = *new_subst;
  } else {
    value args[] = { Val_int(key_idx), _key, _term, *new_subst};
    _subst = caml_callbackN(*extend, 4, args);
  }

  _prefix = caml_alloc_cons();
  Store_field( _prefix, 0, _cnt);        // head
  Store_field( _prefix, 1, *new_prefix); // tail

  *new_prefix = _prefix;
  *new_subst  = _subst;

  CAMLreturnT(int,OK);
}

int
caml_unify_in_c_impl(value _x, value _y, value *_prefix, value *_subst)
{
  CAMLparam2(_x, _y);
  CAMLlocal2(_new_subst, _new_prefix);

  _x = caml_walk(*_subst, _x);
  _y = caml_walk(*_subst, _y);

  int is_var_x = is_var(_x);
  int is_var_y = is_var(_y);

  if (is_var_x && is_var_y) {
    if (get_var_idx(_x) == get_var_idx(_y)) {
      CAMLreturnT(int,OK);
    } else {
      int rez = do_extend(_prefix, _subst, get_var_idx(_x), _x, _y );
      CAMLreturnT(int,rez);
    }
  } else if (is_var_x) {
    // буквально то же самое
    int rez = do_extend(_prefix, _subst, get_var_idx(_x), _x, _y );
    CAMLreturnT(int,rez);
  } else if (is_var_y) {
    int rez = do_extend(_prefix, _subst, get_var_idx(_y), _y, _x );
    CAMLreturnT(int,rez);
  } else {
    /* printf("last case of unification: %u and %u\n", _x, _y); */
      if (_x == _y) {
        // pointer equality
        CAMLreturnT(int,OK);
      } else if (Is_block(_x) && Is_block(_y)) {
        if (Tag_val(_x) != Tag_val(_y) ) {
          CAMLreturnT(int,FAIL);
        }
        if (Tag_val(_x) == String_tag) {
          if (Val_int(0) == caml_string_compare(_x,_y) )
            CAMLreturnT(int,OK);
          else
            CAMLreturnT(int,FAIL);
        }
        int size = Wosize_val(_x);
        if (size != Wosize_val(_y) ) {
          CAMLreturnT(int,FAIL);
        }
        for (int i=0; i<size; ++i) {
          int rez = caml_unify_in_c_impl(Field(_x, i), Field(_y, i), _prefix, _subst);
          if (rez == FAIL)
            CAMLreturnT(int,FAIL);
        }
        CAMLreturnT(int,OK);
      } else if (Is_block(_x) ^ Is_block(_y))
        CAMLreturnT(int,FAIL);
      else
        CAMLreturnT(int,FAIL);
  }
}


// external unify_in_c : scope:Var.scope -> Env.t -> t -> 'a -> 'a -> (content list * t) option
// extern "C"
CAMLprim value
caml_unify_in_c(value _scope, value _env, value _subst, value _x, value _y)
{
  CAMLparam5(_scope, _env, _subst, _x, _y);
  CAMLlocal4(_prefix, _new_subst, _pair, _answer);

  env_anchor = Field(_env, 0);
  cur_scope = _scope;

  /* assert(Is_long(env_anchor)); */

  _new_subst = _subst;
  _prefix = Val_emptylist;

  int rez = caml_unify_in_c_impl(_x, _y, &_prefix, &_new_subst);

  if (rez != OK) {
    CAMLreturn(Val_none);
  }

  _answer = caml_alloc_small(1, 0);
  _pair = caml_alloc_small(2, 0);
  Store_field( _pair, 0, _prefix );
  Store_field( _pair, 1, _new_subst );
  Store_field( _answer, 0, _pair );
  CAMLreturn(_answer);
}
