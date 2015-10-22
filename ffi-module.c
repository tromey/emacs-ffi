#include <config.h>
#include "emacs_module.h"
#include "lisp.h"
#include <ffi.h>
#include <ltdl.h>

int plugin_is_GPL_compatible;

static emacs_value nil;
static emacs_value eq;
static emacs_value make_bool_vector;

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
  { ":pointer", &ffi_type_pointer }
};

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

  // FIXME lt_dlerror
  // FIXME use NAME here in the error
  if (!handle)
    error ("cannot dlopen file");
  return env->make_user_ptr (env, null_finalizer, handle);
}

/* (ffi--dlsym symbol-name &optional handle) */
static emacs_value
ffi_dlsym (emacs_env *env, int nargs, emacs_value args[], void *ignore)
{
  lt_dlhandle handle = env->get_user_ptr_ptr (env, args[1]);
  void *sym;

  size_t length = 0;
  env->copy_string_contents (env, args[0], NULL, &length);
  char *name = malloc (length);
  env->copy_string_contents (env, args[0], name, &length);

  sym = lt_dlsym (handle, name);
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
    {
      emacs_value args[2] = { type, type_descriptors[i].value };
      /* FIXME this would be nice as a module method.  */
      if (env->funcall (env, eq, 2, args))
	return type_descriptors[i].type;
    }
  return NULL;
}

/* (ffi--prep-cif return-type arg-types &optional n-fixed-args) */
static emacs_value
module_ffi_prep_cif (emacs_env *env, int nargs, emacs_value args[], void *ignore)
{
  unsigned int i;
  ffi_type *return_type;
  ptrdiff_t n_types;
  ffi_type **arg_types;
  Lisp_Object typevec = (Lisp_Object) /*FIXME*/ args[1];

  return_type = convert_type_from_lisp (env, args[0]);

  /* FIXME the module API should have some vector stuff */
  CHECK_VECTOR (typevec);
  n_types = ASIZE (typevec);
  arg_types = malloc (n_types * sizeof (ffi_type *));

  for (i = 0; i < n_types; ++i)
    {
      Lisp_Object this_type = AREF (typevec, i);
      arg_types[i] = convert_type_from_lisp (env, (emacs_value) this_type);
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

  /* FIXME error check status  */

  return env->make_user_ptr (env, free, cif);
}

static union holder
convert_from_lisp (emacs_env *env, emacs_value e_type, emacs_value ev)
{
  ffi_type *type = convert_type_from_lisp (env, e_type);
  union holder result;

#define MAYBE_NUMBER(ftype, field)			\
  else if (type == &ffi_type_ ## ftype)			\
    {							\
      result.field = env->fixnum_to_int (env, ev);	\
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
    result.f = env->float_to_c_double (env, ev);
  else if (type == &ffi_type_double)
    result.d = env->float_to_c_double (env, ev);
  else if (type == &ffi_type_pointer)
    result.p = env->get_user_ptr_ptr (env, ev);
  /* FIXME else error */

#undef MAYBE_NUMBER

  return result;
}

static emacs_value
convert_to_lisp (emacs_env *env, emacs_value lisp_type, union holder value)
{
  ffi_type *type = convert_type_from_lisp (env, lisp_type);
  emacs_value result;

#define MAYBE_NUMBER(ftype, field)		\
  else if (type == &ffi_type_ ## ftype)		\
    result = env->make_fixnum (env, value.field);

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
    result = env->make_float (env, value.f);
  else if (type == &ffi_type_double)
    result = env->make_float (env, value.d);
  else if (type == &ffi_type_pointer)
    result = env->make_user_ptr (env, null_finalizer, value.p);
  /* FIXME else error */

#undef MAYBE_NUMBER

  return result;
}

/* (ffi--call cif function return-type arg-types &rest args) */
static emacs_value
module_ffi_call (emacs_env *env, int nargs, emacs_value *args, void *ignore)
{
  ffi_cif *cif = env->get_user_ptr_ptr (env, args[0]);
  void *fn = env->get_user_ptr_ptr (env, args[1]);
  void **values;
  union holder *holders;
  // FIXME need the return type here, which is annoying
  // really we ought to capture it in the CIF
  emacs_value return_type = args[2];
  Lisp_Object arg_types = (Lisp_Object) /*FIXME*/ args[3];
  union holder result;

  CHECK_VECTOR (arg_types);

  nargs -= 4;
  args += 4;

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
	  emacs_value this_type = (emacs_value) /*FIXME*/ AREF (arg_types, i);
	  holders[i] = convert_from_lisp (env, this_type, args[i]);
	  values[i] = &holders[i];
	}
    }

  ffi_call (cif, fn, &result, values);

  free (values);
  free (holders);

  return convert_to_lisp (env, return_type, result);
}

/* (ffi--mem-ref POINTER SIZE) */
static emacs_value
module_ffi_mem_ref (emacs_env *env, int nargs, emacs_value *args, void *ignore)
{
  void *ptr = env->get_user_ptr_ptr (env, args[0]);
  int64_t len = env->fixnum_to_int (env, args[1]);
  return (emacs_value) make_unibyte_string (ptr, len);
}

/* (ffi--mem-set POINTER DATA) */
static emacs_value
module_ffi_mem_set (emacs_env *env, int nargs, emacs_value *args, void *ignore)
{
  void *ptr = env->get_user_ptr_ptr (env, args[0]);
  Lisp_Object str = (Lisp_Object) args[2];
  CHECK_STRING (str);
  memcpy (ptr, SDATA (str), SCHARS (str));
  return nil;
}

/* (ffi--pointer+ POINTER NUM) */
static emacs_value
module_ffi_pointer_plus (emacs_env *env, int nargs, emacs_value *args,
			 void *ignore)
{
  char *ptr;

  if (BOOL_VECTOR_P ((Lisp_Object) args[0]))
    {
      ptr = env->get_user_ptr_ptr (env, args[0]);
      ptr += env->fixnum_to_int (env, args[1]);
    }
  else
    {
      ptr = env->get_user_ptr_ptr (env, args[1]);
      ptr += env->fixnum_to_int (env, args[0]);
    }

  return env->make_user_ptr (env, null_finalizer, ptr);
}

/* (ffi-get-c-string POINTER) */
static emacs_value
module_ffi_get_c_string (emacs_env *env, int nargs, emacs_value *args,
			 void *ignore)
{
  char *ptr = env->get_user_ptr_ptr (env, args[0]);
  size_t len = strlen (ptr);
  return env->make_string (env, ptr, len);
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
  { "ffi--prep-cif", 1, -1, module_ffi_prep_cif },
  { "ffi--call", 4, -1, module_ffi_call },
  { "ffi--mem-ref", 2, 2, module_ffi_mem_ref },
  { "ffi--mem-set", 3, 3, module_ffi_mem_set },
  { "ffi-pointer+", 2, 2, module_ffi_pointer_plus },
  { "ffi-get-c-string", 1, 1, module_ffi_get_c_string }
};

int
emacs_module_init (struct emacs_runtime *runtime)
{
  unsigned int i;
  emacs_env *env = runtime->get_environment (runtime);

  lt_dlinit ();

  nil = env->make_global_ref (env, env->intern (env, "nil"));
  eq = env->make_global_ref (env, env->intern (env, "eq"));

  make_bool_vector = env->intern (env, "make-bool-vector");
  env->make_global_ref (env, make_bool_vector);

  emacs_value fset = env->intern (env, "fset");

  for (i = 0; i < ARRAY_SIZE (type_descriptors); ++i)
    {
      type_descriptors[i].value = env->intern (env, type_descriptors[i].name);
      env->make_global_ref (env, type_descriptors[i].value);
    }

  for (i = 0; i < ARRAY_SIZE (exports); ++i)
    {
      emacs_value func = env->make_function (env, exports[i].min,
					     exports[i].max, exports[i].subr,
					     NULL);
      emacs_value sym = env->intern (env, exports[i].name);

      emacs_value args[2] = { sym, func };
      env->funcall (env, fset, 2, args);
    }

  return 0;
}
