#include <stdio.h>
#include <assert.h>

// extern "C" {
#include <caml/mlvalues.h>
#include <caml/callback.h>
#include <caml/memory.h>
#include <caml/alloc.h>
// }

// extern "C"
CAMLprim value
caml_print_hello(value unit)
{
    printf("Hello\n");
    return Val_unit;
}

#define Val_none Val_int(0)

static value * lookup = NULL;
static value * get_MK_anchor = NULL;
// extend (index, var, term subst) returns always Subst.t
static value * extend = NULL;
static value * real_anchor = NULL;

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
  if (_anchor != Val_int(78955))
    CAMLreturnT(int, 0);

  // We skip chekcing with the env
  // it should be a list with
  CAMLreturnT(int, 1);
}

#define GLOBAL_ANCHOR_N = -8

int get_var_idx(value _term)
{
  CAMLparam1(_term);
  CAMLreturnT(int, Int_val(Field(_term, 2)) );
}

value
caml_walk(value _subst, value _term)
{
  if (is_var(_term)) {
    int idx = get_var_idx(_term);
    value _maybe_term = caml_callback2(*lookup, _subst, Val_int(idx));
    if (Is_block(_maybe_term)) {
      return caml_walk(_subst, Field(_maybe_term, 0));
    } else {
      return _term;
    }
  } else {
    return _term;
  }
}


void do_extend(value *new_prefix, value *new_subst,
  int idx, value _x, value _y )
{
  CAMLparam0();
  CAMLlocal3(_subst, _prefix, _pair);

  value args[] = {Val_int(idx), _x, _y, *new_subst};
  _subst = caml_callbackN(*extend, 4, args);
  // now we do cons in C
  _pair = caml_alloc_tuple(2);
  Store_field( _pair, 0, _x );              // var
  Store_field( _pair, 1, _y );              // term

  _prefix = caml_alloc_small(2,0);
  Store_field( _prefix, 0, _pair );     // head
  Store_field( _prefix, 1, *new_prefix );       // tail

  *new_prefix = _prefix;
  *new_subst  = _subst;

  CAMLreturn0;
}

#define OK 0
#define FAIL 1
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
      return OK;
    } else {
      // _new_prefix = caml_alloc_small(2,0);
      do_extend(_prefix, _subst, get_var_idx(_x), _x, _y );
      return OK;
      // тут вернуть пару из new_prefix и _new_subst
    }
  } else if (is_var_x) {
    // буквально то же самое
    // _new_prefix = caml_alloc_small(2,0);
    do_extend(_prefix, _subst, get_var_idx(_x), _x, _y );
    return OK;
  } else if (is_var_y) {
    // _new_prefix = caml_alloc_small(2,0);
    do_extend(_prefix, _subst, get_var_idx(_y), _y, _x );
    return OK;
  } else {
      if (_x == _y) {
        // pointer equality
        // return acc
        return OK;
      } else if (Is_block(_x) && Is_block(_y)) {
        if (Tag_val(_x) != Tag_val(_y) ) {
          return FAIL;
        }
        int size = Wosize_val(_x);
        if (size != Wosize_val(_y) ) {
          return FAIL;
        }
        for (int i=0; i<size; ++i) {
          int rez = caml_unify_in_c_impl(Field(_x, i), Field(_y, i), _prefix, _subst);
          if (!rez)
            return FAIL;
        }
        return OK;
      } else if (Is_block(_x) ^ Is_block(_y))
        return FAIL;
      else
        return FAIL;
  }
}


// external unify_in_c : scope:Var.scope -> Env.t -> t -> 'a -> 'a -> (content list * t) option
// extern "C"
CAMLprim value
caml_unify_in_c(value _scope, value _env, value _subst, value _x, value _y)
{
  CAMLparam5(_scope, _env, _subst, _x, _y);
  CAMLlocal4(_prefix, _new_subst, _pair, _answer);

  if (lookup == NULL) {
    lookup = caml_named_value("Subst.lookup");
    assert(lookup != NULL);
  }

  if (extend == NULL) {
    extend = caml_named_value("Subst.extend");
    assert(extend != NULL);
  }

  // if (get_MK_anchor == NULL) {
  //   get_MK_anchor = caml_named_value("get_MK_anchor");
  //   assert(get_MK_anchor != NULL);
  // }


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
