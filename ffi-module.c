
#include "emacs_module.h"

#include <ffi.h>
#include <ltdl.h>
#include <string.h>
#include <assert.h>

int plugin_is_GPL_compatible;

static emacs_value nil;
static emacs_value wrong_type_argument;
static emacs_value vector;
static emacs_value aref;
static emacs_value length;
static emacs_value error;
static emacs_value make_vector;
static emacs_value aset;

#define ARRAY_SIZE(x) (sizeof (x) / sizeof (x[0]))

union holder
{
  int8_t i8;
  uint8_t ui8;
  int16_t i16;
  uint16_t ui16;
  int32_t i32;
  uint32_t ui32;
  int64_t i64;
  uint64_t ui64;
  float f;
  double d;
  unsigned char uc;
  signed char c;
  short s;
  unsigned short us;
  int i;
  unsigned int ui;
  long l;
  unsigned long ul;
  void *p;
};

struct type_descriptor
{
  const char *name;
  ffi_type *type;
  emacs_value value;
};

static struct type_descriptor type_descriptors[] =
{
  { ":void", &ffi_type_void },
  { ":int8", &ffi_type_sint8 },
  { ":uint8", &ffi_type_uint8 },
  { ":int16", &ffi_type_sint16 },
  { ":uint16", &ffi_type_uint16 },
  { ":int32", &ffi_type_sint32 },
  { ":uint32", &ffi_type_uint32 },
  { ":int64", &ffi_type_sint64 },
  { ":uint64", &ffi_type_uint64 },
  { ":float", &ffi_type_float },
  { ":double", &ffi_type_double },
  { ":uchar", &ffi_type_uchar },
  { ":char", &ffi_type_schar },
  { ":ushort", &ffi_type_ushort },
  { ":short", &ffi_type_sshort },
  { ":uint", &ffi_type_uint },
  { ":int", &ffi_type_sint },
  { ":ulong", &ffi_type_ulong },
  { ":long", &ffi_type_slong },
  { ":pointer", &ffi_type_pointer },

  { ":size_t", NULL },
  { ":ssize_t", NULL },
  { ":bool", NULL }
};

// Description of a closure, freed by free_closure_desc.
struct closure_description
{
  emacs_env *env;
  void *closure;
  void *code;
  emacs_value cif_ref;
  emacs_value func_ref;
};

static void free_closure_desc (void *);



static void
null_finalizer (void *ptr)
{
}

/* (ffi--dlopen str) */
static emacs_value
ffi_dlopen (emacs_env *env, int nargs, emacs_value args[], void *ignore)
{
  lt_dlhandle handle;

  size_t length = 0;
  env->copy_string_contents (env, args[0], NULL, &length);
  char *name = malloc (length);
  env->copy_string_contents (env, args[0], name, &length);

  handle = lt_dlopenext (name);
  free (name);

  if (!handle)
    {
      // FIXME lt_dlerror
      // FIXME use NAME here in the error
      env->error_signal (env, error, nil);
      return NULL;
    }
  return env->make_user_ptr (env, null_finalizer, handle);
}

/* (ffi--dlsym symbol-name &optional handle) */
static emacs_value
ffi_dlsym (emacs_env *env, int nargs, emacs_value args[], void *ignore)
{
  lt_dlhandle handle = env->get_user_ptr_ptr (env, args[1]);
  if (env->error_check (env))
    return NULL;

  size_t length = 0;
  env->copy_string_contents (env, args[0], NULL, &length);
  char *name = malloc (length);
  env->copy_string_contents (env, args[0], name, &length);

  void *sym = lt_dlsym (handle, name);
  free (name);

  if (sym == NULL)
    return nil;
  return env->make_user_ptr (env, null_finalizer, sym);
}

static ffi_type *
convert_type_from_lisp (emacs_env *env, emacs_value ev_type)
{
  emacs_value type = ev_type;
  unsigned int i;
  for (i = 0; i < ARRAY_SIZE (type_descriptors); ++i)
    if (env->eq (env, type, type_descriptors[i].value))
      return type_descriptors[i].type;
  env->error_signal (env, wrong_type_argument, ev_type);
  return NULL;
}

static bool
check_vector (emacs_env *env, emacs_value value)
{
  emacs_value type = env->type_of (env, value);
  if (!type)
    return false;
  if (!env->eq (env, type, vector))
    {
      env->error_signal (env, wrong_type_argument, value);
      return false;
    }
  return true;
}

static void
free_cif (void *p)
{
  ffi_cif *cif = p;
  free (cif->arg_types);
  free (cif);
}

/* (ffi--prep-cif return-type arg-types &optional n-fixed-args) */
static emacs_value
module_ffi_prep_cif (emacs_env *env, int nargs, emacs_value args[],
		     void *ignore)
{
  unsigned int i;
  ffi_type *return_type;
  ptrdiff_t n_types;
  ffi_type **arg_types;
  emacs_value typevec = args[1];
  emacs_value result = NULL;

  return_type = convert_type_from_lisp (env, args[0]);
  if (!return_type)
    return NULL;

  if (!check_vector (env, typevec))
    return NULL;

  emacs_value n_types_val;
  n_types_val = env->funcall (env, length, 1, &typevec);
  if (!n_types_val)
    return NULL;
  n_types = env->fixnum_to_int (env, n_types_val);
  if (env->error_check (env))
    return NULL;
  arg_types = malloc (n_types * sizeof (ffi_type *));

  for (i = 0; i < n_types; ++i)
    {
      // A bit heavy.
      emacs_value evi = env->make_fixnum (env, i);
      if (!evi)
	goto fail;
      emacs_value args[2] = { typevec, evi };
      emacs_value this_type = env->funcall (env, aref, 2, args);
      if (!this_type)
	goto fail;
      arg_types[i] = convert_type_from_lisp (env, this_type);
      if (!arg_types[i])
	goto fail;
    }

  ffi_cif *cif = malloc (sizeof (ffi_cif));
  ffi_status status;
  if (nargs == 3)
    status = ffi_prep_cif_var (cif, FFI_DEFAULT_ABI,
			       env->fixnum_to_int (env, args[2]),
			       n_types, return_type, arg_types);
  else
    status = ffi_prep_cif (cif, FFI_DEFAULT_ABI, n_types,
			   return_type, arg_types);

  if (status)
    /* FIXME add some useful message */
    env->error_signal (env, error, nil);
  else
    result = env->make_user_ptr (env, free_cif, cif);

  if (!result)
    free_cif (cif);
 fail:
  return result;
}

static void
convert_from_lisp (emacs_env *env, ffi_type *type, emacs_value ev,
		   union holder *result)
{
#define MAYBE_NUMBER(ftype, field)			\
  else if (type == &ffi_type_ ## ftype)			\
    {							\
      result->field = env->fixnum_to_int (env, ev);	\
    }

  if (type == &ffi_type_void)
    error;
  MAYBE_NUMBER (sint8, i8)
  MAYBE_NUMBER (uint8, ui8)
  MAYBE_NUMBER (sint16, i16)
  MAYBE_NUMBER (uint16, ui16)
  MAYBE_NUMBER (sint32, i32)
  MAYBE_NUMBER (uint32, ui32)
  MAYBE_NUMBER (sint64, i64)
  MAYBE_NUMBER (uint64, ui64)
  MAYBE_NUMBER (schar, c)
  MAYBE_NUMBER (uchar, uc)
  MAYBE_NUMBER (sint, i)
  MAYBE_NUMBER (uint, ui)
  MAYBE_NUMBER (sshort, s)
  MAYBE_NUMBER (ushort, us)
  MAYBE_NUMBER (slong, l)
  MAYBE_NUMBER (ulong, ul)
  else if (type == &ffi_type_float)
    result->f = env->float_to_c_double (env, ev);
  else if (type == &ffi_type_double)
    result->d = env->float_to_c_double (env, ev);
  else if (type == &ffi_type_pointer)
    {
      result->p = env->get_user_ptr_ptr (env, ev);
      // We use the finalizer to detect whether we have a closure
      // pointer; these are converted to their code pointer, not their
      // raw pointer.
      if (!env->error_check (env)
	  && env->get_user_ptr_finalizer (env, ev) == free_closure_desc)
	{
	  struct closure_description *desc = result->p;
	  result->p = desc->code;
	}
    }

  /* FIXME else error */

#undef MAYBE_NUMBER
}

static emacs_value
convert_to_lisp (emacs_env *env, ffi_type *type, union holder *value)
{
  emacs_value result;

#define MAYBE_NUMBER(ftype, field)		\
  else if (type == &ffi_type_ ## ftype)		\
    result = env->make_fixnum (env, value->field);

  if (type == &ffi_type_void)
    result = nil;
  MAYBE_NUMBER (sint8, i8)
  MAYBE_NUMBER (uint8, ui8)
  MAYBE_NUMBER (sint16, i16)
  MAYBE_NUMBER (uint16, ui16)
  MAYBE_NUMBER (sint32, i32)
  MAYBE_NUMBER (uint32, ui32)
  MAYBE_NUMBER (sint64, i64)
  MAYBE_NUMBER (uint64, ui64)
  MAYBE_NUMBER (schar, c)
  MAYBE_NUMBER (uchar, uc)
  MAYBE_NUMBER (sint, i)
  MAYBE_NUMBER (uint, ui)
  MAYBE_NUMBER (sshort, s)
  MAYBE_NUMBER (ushort, us)
  MAYBE_NUMBER (slong, l)
  MAYBE_NUMBER (ulong, ul)
  else if (type == &ffi_type_float)
    result = env->make_float (env, value->f);
  else if (type == &ffi_type_double)
    result = env->make_float (env, value->d);
  else if (type == &ffi_type_pointer)
    result = env->make_user_ptr (env, null_finalizer, value->p);
  /* FIXME else error */

#undef MAYBE_NUMBER

  return result;
}

/* (ffi--call cif function &rest args) */
static emacs_value
module_ffi_call (emacs_env *env, int nargs, emacs_value *args, void *ignore)
{
  ffi_cif *cif = env->get_user_ptr_ptr (env, args[0]);
  if (env->error_check (env))
    return NULL;

  void *fn = env->get_user_ptr_ptr (env, args[1]);
  if (env->error_check (env))
    return NULL;

  void **values;
  union holder *holders;
  union holder result;
  emacs_value lisp_result = NULL;

  nargs -= 2;
  args += 2;

  if (nargs == 0)
    {
      values = NULL;
      holders = NULL;
    }
  else
    {
      holders = malloc (nargs * sizeof (union holder));
      values = malloc (nargs * sizeof (void *));

      unsigned int i;
      for (i = 0; i < nargs; ++i)
	{
	  convert_from_lisp (env, cif->arg_types[i], args[i], &holders[i]);
	  if (env->error_check (env))
	    goto fail;
	  values[i] = &holders[i];
	}
    }

  ffi_call (cif, fn, &result, values);

  lisp_result = convert_to_lisp (env, cif->rtype, &result);

 fail:
  free (values);
  free (holders);

  return lisp_result;
}

/* (ffi--mem-ref POINTER SIZE) */
static emacs_value
module_ffi_mem_ref (emacs_env *env, int nargs, emacs_value *args, void *ignore)
{
  unsigned char *ptr = env->get_user_ptr_ptr (env, args[0]);
  if (env->error_check (env))
    return NULL;
  int64_t len = env->fixnum_to_int (env, args[1]);
  if (env->error_check (env))
    return NULL;

  // Access to unibyte strings would be super -- or else this is the
  // slowest memcpy ever written.
  emacs_value mvargs[2] = { args[1], nil };
  emacs_value result = env->funcall (env, make_vector, 2, mvargs);
  if (!result)
    return NULL;

  int i;
  for (i = 0; i < len; ++i)
    {
      emacs_value evi = env->make_fixnum (env, i);
      if (!env->error_check (env))
	return NULL;
      emacs_value datum = env->make_fixnum (env, ptr[i]);
      if (!env->error_check (env))
	return NULL;
      emacs_value asetargs[3] = { result, evi, datum };
      if (!env->funcall (env, aset, 3, args))
	return NULL;
    }

  return result;
}

/* (ffi--mem-set POINTER DATA) */
static emacs_value
module_ffi_mem_set (emacs_env *env, int nargs, emacs_value *args, void *ignore)
{
  unsigned char *ptr = env->get_user_ptr_ptr (env, args[0]);
  if (env->error_check (env))
    return NULL;

  emacs_value datavec = args[2];
  if (!check_vector (env, datavec))
    return NULL;

  emacs_value len_val;
  len_val = env->funcall (env, length, 1, &datavec);
  if (!len_val)
    return NULL;
  int64_t len = env->fixnum_to_int (env, len_val);
  if (env->error_check (env))
    return NULL;

  int i;
  for (i = 0; i < len; ++i)
    {
      emacs_value evi = env->make_fixnum (env, i);
      if (!env->error_check (env))
	return NULL;
      emacs_value args[2] = { datavec, evi };
      emacs_value elt = env->funcall (env, aref, 2, args);
      if (!elt)
	return NULL;
      int64_t byteval = env->fixnum_to_int (env, elt);
      if (env->error_check (env))
	return NULL;
      ptr[i] = (unsigned char) byteval;
    }

  return nil;
}

/* (ffi--pointer+ POINTER NUM) */
static emacs_value
module_ffi_pointer_plus (emacs_env *env, int nargs, emacs_value *args,
			 void *ignore)
{
  char *ptr = env->get_user_ptr_ptr (env, args[0]);
  if (env->error_check (env))
    return NULL;
  ptr += env->fixnum_to_int (env, args[1]);
  if (env->error_check (env))
    return NULL;
  return env->make_user_ptr (env, null_finalizer, ptr);
}

/* (ffi-get-c-string POINTER) */
static emacs_value
module_ffi_get_c_string (emacs_env *env, int nargs, emacs_value *args,
			 void *ignore)
{
  char *ptr = env->get_user_ptr_ptr (env, args[0]);
  if (env->error_check (env))
    return NULL;
  size_t len = strlen (ptr);
  return env->make_string (env, ptr, len);
}



static void
generic_callback (ffi_cif *cif, void *ret, void **args, void *d)
{
  struct closure_description *desc = d;
  emacs_env *env = desc->env;

  emacs_value *argvalues = malloc (cif->nargs * sizeof (emacs_value));
  int i;
  for (i = 0; i < cif->nargs; ++i)
    {
      argvalues[i] = convert_to_lisp (env, cif->arg_types[i], args[i]);
      if (!argvalues[i])
	goto fail;
    }

  emacs_value value = env->funcall (env, desc->func_ref, cif->nargs, argvalues);
  if (value)
    convert_from_lisp (env, cif->rtype, value, ret);

  env->error_clear (env);

 fail:
  // On failure we might leave the result uninitialized, but there's
  // nothing to do about it.
  free (argvalues);
}

static void
free_closure_desc (void *d)
{
  struct closure_description *desc = d;

  desc->env->free_global_ref (desc->env, desc->cif_ref);
  desc->env->free_global_ref (desc->env, desc->func_ref);
  ffi_closure_free (desc->closure);
}

/* (ffi-make-closure CIF FUNCTION) */
static emacs_value
module_ffi_make_closure (emacs_env *env, int nargs, emacs_value *args,
			 void *ignore)
{
  emacs_value cif_ref = NULL;
  emacs_value func = NULL;
  struct closure_description *desc = NULL;

  cif_ref = env->make_global_ref (env, args[0]);
  if (!cif_ref)
    return NULL;
  func = env->make_global_ref (env, args[1]);
  if (!func)
    goto fail;

  ffi_cif *cif = env->get_user_ptr_ptr (env, args[0]);
  if (env->error_check (env))
    goto fail;

  void *code;
  void *writable = ffi_closure_alloc (sizeof (ffi_closure), &code);

  desc = malloc (sizeof (struct closure_description));
  desc->env = env;
  desc->closure = writable;
  desc->code = code;
  desc->cif_ref = cif_ref;
  desc->func_ref = func;

  ffi_status status = ffi_prep_closure_loc (writable, cif, generic_callback,
					    desc, code);
  if (status)
    /* FIXME add some useful message */
    env->error_signal (env, error, nil);
  else
    {
      emacs_value desc_val = env->make_user_ptr (env, free_closure_desc, desc);
      if (desc_val)
	return desc_val;
    }

 fail:
  free (desc);
  if (func)
    env->free_global_ref (env, func);
  env->free_global_ref (env, cif_ref);
  return NULL;
}



struct descriptor
{
  const char *name;
  int min, max;
  emacs_subr subr;
};

static const struct descriptor exports[] =
{
  { "ffi--dlopen", 1, 1, ffi_dlopen },
  { "ffi--dlsym", 2, 2, ffi_dlsym },
  { "ffi--prep-cif", 1, emacs_variadic_function, module_ffi_prep_cif },
  { "ffi--call", 2, emacs_variadic_function, module_ffi_call },
  { "ffi--mem-ref", 2, 2, module_ffi_mem_ref },
  { "ffi--mem-set", 3, 3, module_ffi_mem_set },
  { "ffi-pointer+", 2, 2, module_ffi_pointer_plus },
  { "ffi-get-c-string", 1, 1, module_ffi_get_c_string },
  { "ffi-make-closure", 2, 2, module_ffi_make_closure }
};

static bool
get_global (emacs_env *env, emacs_value *valptr, const char *name)
{
  *valptr = env->intern (env, name);
  if (!*valptr)
    return false;
  *valptr = env->make_global_ref (env, *valptr);
  return *valptr != NULL;
}

static void
init_type_alias (const char *name, bool is_unsigned, int size)
{
  int i;
  // Start at the end since we know it is a bit more efficient.
  for (i = ARRAY_SIZE (type_descriptors) - 1; i >= 0; --i)
    {
      if (!strcmp (type_descriptors[i].name, name))
	break;
    }
  assert (i >= 0);

  ffi_type **type = &type_descriptors[i].type;
  switch (size)
    {
    case 1:
      *type = is_unsigned ? &ffi_type_sint8 : &ffi_type_uint8;
      break;
    case 2:
      *type = is_unsigned ? &ffi_type_sint16 : &ffi_type_uint16;
      break;
    case 4:
      *type = is_unsigned ? &ffi_type_sint32 : &ffi_type_uint32;
      break;
    case 8:
      *type = is_unsigned ? &ffi_type_sint64 : &ffi_type_uint64;
      break;
    default:
      abort ();
    }
}

int
emacs_module_init (struct emacs_runtime *runtime)
{
  unsigned int i;
  emacs_env *env = runtime->get_environment (runtime);

  lt_dlinit ();

  init_type_alias (":size_t", true, sizeof (size_t));
  init_type_alias (":ssize_t", false, sizeof (ssize_t));
  init_type_alias (":bool", true, sizeof (bool));

  if (!get_global (env, &nil, "nil")
      || !get_global (env, &wrong_type_argument, "wrong-type-argument")
      || !get_global (env, &vector, "vector")
      || !get_global (env, &aref, "aref")
      || !get_global (env, &length, "length")
      || !get_global (env, &error, "error")
      || !get_global (env, &make_vector, "make-vector")
      || !get_global (env, &aset, "aset"))
    return -1;

  emacs_value fset = env->intern (env, "fset");

  for (i = 0; i < ARRAY_SIZE (type_descriptors); ++i)
    {
      if (!get_global (env, &type_descriptors[i].value,
		       type_descriptors[i].name))
	return -1;
    }

  for (i = 0; i < ARRAY_SIZE (exports); ++i)
    {
      emacs_value func = env->make_function (env, exports[i].min,
					     exports[i].max, exports[i].subr,
					     NULL);
      if (!func)
	return -1;
      emacs_value sym = env->intern (env, exports[i].name);
      if (!sym)
	return -1;

      emacs_value args[2] = { sym, func };
      if (!env->funcall (env, fset, 2, args))
	return -1;
    }

  return 0;
}
