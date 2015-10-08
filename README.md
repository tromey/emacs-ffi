This is a simple FFI for Emacs.  It is based on libffi and relies on
the dynamic modules work to be loaded into Emacs.

This is very preliminary.  I'd appreciate your feedback, either via
email or issues on github.

# Types

Currently the library only supports primitive types for arguments and
return types.  Structure types will be supported some day.

Primitive types are described using keywords:

* `:void`. The void type.  This does not make sense as an argument
  type.

* `:int8`, `:uint8`, `:int16`, `:uint16`, `:int32`, `:uint32`,
  `:int64`, `:uint64`: signed or unsigned integers of the indicated size.

* `:float`, `:double`.  Self-explanatory.

* `:uchar`, `:char`, `:ushort`, `:short`, `:uint`, `:int`, `:ulong`,
  `:long`.  Signed or unsigned integers corresponding to the C type of
  the same name.

* `:pointer`.  An C pointer type.  Pointers currently aren't typed.

# Type Conversions

Currently all type conversions work the same in both directions.

* A function declared with a `:void` return type will always return
  `nil` to Lisp.

* A function returning any integer or character type will return a
  Lisp integer.  Note that this may result in the value being
  truncated; currently there is nothing that can be done about this.

* A C pointer will be returned as a bit-vector, as there is no way to
  define a new type from an Emacs module.  Hopefully this will change
  someday.

# Exported Functions

* `(define-ffi-library SYMBOL NAME)`.  Used to define a function that
  lazily loads a library.  Like:

```
  (define-ffi-library libwhatever "libwhatever.so")
```

  In the future more forms may be supported here.

* `(define-ffi-function NAME C-NAME RETURN-TYPE ARG-TYPES LIBRARY)`.

  NAME is the symbol to define.  C-NAME is a string, the name of the
  underlying C function.

  RETURN-TYPE describes the return type of the C function and
  ARG-TYPES describes the argument types.  ARG-TYPES may be a vector
  or a list.

  LIBRARY is the library where the C function should be found.  This
  is just a symbol, most usually defined with `define-ffi-library`.

  `define-ffi-function` defines a new Lisp function.  It takes as many
  arguments as were in ARG-TYPES.  (While there is internal support
  for varargs functions, it is not exposed by `define-ffi-function`.)

# Internal Functions

* `(ffi--dlopen STR)`.  A simple wrapper for `dlopen` (actually
  `lt_dlopen`).  This returns the library handle, a C pointer.

* `(ffi--dlsym STR HANDLE)`.  A simple wrapper for `dlsym` (actually
  `lt_dlsym`).  This finds the C symbol named STR in a library.
  HANDLE is a library handle, as returned by a function defined by
  `define-ffi-library`.

  This returns a C pointer to the indicated symbol, or `nil` if it
  can't be found.  These pointers need not be freed.

* `(ffi--prep-cif RETURN-TYPE ARG-TYPES &optional N-FIXED-ARGS)`.  A
  simple wrapper for libffi's `ffi_prep_cif` and `ffi_prep_cif_var`.
  RETURN-TYPE is the return type; ARG-TYPES is a vector of argument
  types.  If given, N-FIXED-ARGS is an integer holding the number of
  fixed args.  Its presence, even if 0, means that a varargs call is
  being made.  This function returns a C pointer, which should be
  freed using `ffi--free` when done.

* `(ffi--call CIF FUNCTION RETURN-TYPE ARG-TYPES &rest ARGS)`.  Make
  an FFI call.

  CIF is the return from `ffi--prep-cif`.

  FUNCTION is a C pointer to the function to call, normally from
  `ffi--dlsym`.

  RETURN-TYPE and ARG-TYPES are the same as for `ffi--prep-cif`.
  Maybe I will get rid of `ffi--prep-cif` entirely and put everything
  into `ffi--call`.

  ARGS are the arguments to pass to FUNCTION.

* `(ffi--free POINTER)`.  This calls `free` on the pointer.  This is
  defined explicitly because it is used by the Lisp code included in
  the package.

* `(ffi--mem-ref POINTER SIZE)`.  Read SIZE bytes of memory starting
  at POINTER.  The bytes are returned in a unibyte string.  This can
  be useful in conjunction with the `bindat` package for unpacking C
  structures.

  I may add a simple way to fetch C strings from memory.

* `(ffi--mem-set POINTER STRING)`.  Copy the contents of STRING to the
  memory at POINTER.

* `(ffi--pointer+ POINTER NUMBER)`.  Pointer math in Lisp.

# Partial To-Do List

* Add nice support for varargs calls.  The main issue is that the
  types have to be described at each call, and it would be good to
  have an easy way to describe this in Lisp.

* Structure types in arguments and returns.

* Array types.

* Union types.

* Make C functions from Lisp using the libffi closure API.

* An easy way to get a C string.

* Typed pointers.

* A way to deal with integer truncation.  Or just add bignums to
  Emacs.
