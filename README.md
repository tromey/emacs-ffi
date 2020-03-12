This is an FFI for Emacs.  It is based on libffi and relies on the
dynamic module support in order to be loaded into Emacs.  It is
relatively full-featured, but for the time being low-level.

I'd appreciate your feedback, either via email or issues on github.

# Types

Currently the library supports primitive and structure types for
arguments and return types.

Primitive types are described using keywords:

* `:void`. The void type.  This does not make sense as an argument
  type.

* `:int8`, `:uint8`, `:int16`, `:uint16`, `:int32`, `:uint32`,
  `:int64`, `:uint64`: signed or unsigned integers of the indicated size.

* `:float`, `:double`.  Self-explanatory.

* `:char`, `:uchar`, `:schar`, `:ushort`, `:short`, `:uint`, `:int`,
  `:ulong`, `:long`, `:ulonglong`, `:longlong`.  Signed or unsigned
  integers corresponding to the C type of the same name.  `:char` is
  treated specially because whether it is signed or unsigned is
  platform-dependent (and also command-line-argument-dependent, though
  normally this doesn't matter).

* `:pointer`.  A C pointer type.  Pointers currently aren't typed, in
  the sense that they aren't differentiated based on what they point
  to.

* `:size_t`, `:ssize_t`, `:ptrdiff_t`, `:wchar_t`.  These correspond
  to the C type of the same name and internally are just aliases for
  one of the other integral types.

* `:bool`.  Booleans are treated in a Lisp style.  As an argument
  type, `nil` is converted to a C `false` value, and other Lisp values
  are converted to `true`.  As a return type, `true` is converted to
  `t` and `false` is converted to `nil`.  Note that `0` is *not*
  converted to `false`.  If you want a "numeric" boolean type, you can
  use the size and alignment to find the corresponding primitive type
  and use that instead.


Structure types are represented by a user-pointer object that wraps an
`ffi_type`.  The best way to manipulate structures is to use
`define-ffi-struct`, which is a limited form of `cl-defstruct` that
works on foreign objects directly.

A structure object is also represented by a user-pointer object.  If a
function's return type is a structure type, then the object allocated
by the FFI will automatically be reclaimed by the garbage collector --
there is no need to explicitly free it.  (Contrast this with the
behavior of `ffi-make-c-string`, which requires an explicit free.)

# Type Conversions

Currently all type conversions work the same in both directions.

* A function declared with a `:void` return type will always return
  `nil` to Lisp.

* A function returning any integer or character type will return a
  Lisp integer.  Note that this may result in the value being
  truncated; currently there is nothing that can be done about this.

* A C pointer will be returned as a user-pointer (a new Lisp type
  introduced by the dynamic module patch).

* A structure is also represented as a user-pointer.  When a structure
  is returned by value from a foreign function, the resulting
  user-pointer will have a finalizer attached that will free the
  memory when the user-pointer is garbage collected.

# Exported Functions

* `(define-ffi-library SYMBOL NAME)`.  Used to define a function that
  lazily loads a library.  Like:

   ```
     (define-ffi-library libwhatever "libwhatever")
   ```

  ffi-module uses libltdl (from libtool), which will automatically
  supply the correct extension if none is specified, so it's generally
  best to leave off the `.so`.

* `(define-ffi-function NAME C-NAME RETURN-TYPE ARG-TYPES LIBRARY)`.

  A macro that defines a new Lisp function.  It takes as many
  arguments as were in ARG-TYPES.  (While there is internal support
  for varargs functions, it is not exposed by `define-ffi-function`.)

  NAME is the symbol to define.  C-NAME is a string, the name of the
  underlying C function.

  RETURN-TYPE describes the return type of the C function and
  ARG-TYPES describes the argument types.  ARG-TYPES may be a vector
  or a list.

  LIBRARY is the library where the C function should be found.  This
  is just a symbol, most usually defined with `define-ffi-library`.

* `(ffi-lambda FUNCTION RETURN-TYPE ARG-TYPES)`.  Take a C function
  pointer and a description of its type, and return a Lisp function.
  Unlike `define-ffi-function`, this is not a macro.  You may wish to
  cache these as each call to `ffi-lambda` makes a new CIF.

* `(ffi-make-closure CIF FUNCTION)`.  Make a C pointer to the Lisp
  function.  This pointer can then be passed to C functions that need
  a pointer-to-function argument, and FUNCTION will be called with
  whatever arguments are passed in.

  CIF is a CIF as returned by `ffi--prep-cif`.  It describes the
  function's type (as needed by C).

  This returns a C function pointer, wrapped in the usual way as a
  user-pointer object.

* `(ffi-get-c-string POINTER)`.  Assume the pointer points to a C
  string, and return a Lisp string with those contents.

* `(ffi-make-c-string STRING)`.  Allocate a C string with the same
  contents as the given Lisp string.  Note that the memory allocated
  by this must be freed by the caller.  It is done this way so that
  Lisp code has the option of transferring ownership of the pointer to
  some C code.

* `(define-ffi-struct NAME &rest SLOT...)`.  A limited form of
  `cl-defstruct` that works on foreign objects.  This defines a new
  foreign structure type named NAME.  Each SLOT is of the form
  `(SLOT-NAME :type TYPE)`.  Each TYPE must be a foreign type.

  `define-ffi-struct` makes accessors for each slot of the form
  `NAME-SLOT-NAME`.  `setf` works on these accessors.

* `(define-ffi-union NAME &rest SLOT...)`.  Like `define-ffi-struct`,
  but defines a union.

* `(ffi-pointer+ POINTER NUMBER)`.  Pointer math in Lisp.

* `(ffi-pointer-null-p POINTER)`.  Return `t` if the argument is a
  null pointer.  If the argument is not a pointer or is not null,
  return `nil`.

* `(ffi-pointer= POINTER1 POINTER2)`.  Return `t` if the two pointers
  are equal, `nil` if not.

* `(ffi-allocate TYPE-OR-NUMBER)`.  Allocate some memory.  If a type
  is given, allocates according to the type's size.  If a number is
  given, allocates that many bytes.  The returned memory will not be
  automatically reclaimed; you must use `ffi-free` for that.

* `(ffi-free POINTER)`.  Free some memory allocated with
  `ffi-allocate` or `ffi-make-c-string`.

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
  being made.  This function returns a C pointer wrapped in a Lisp
  object; the garbage collector will handle any needed finalization.

* `(ffi--call CIF FUNCTION &rest ARG...)`.  Make an FFI call.

  CIF is the return from `ffi--prep-cif`.

  FUNCTION is a C pointer to the function to call, normally from
  `ffi--dlsym`.

  ARGS are the arguments to pass to FUNCTION.

* `(ffi--mem-ref POINTER TYPE)`.  Read memory from POINTER and convert
  it, using the usual conversions, as the given type.  This is the
  Lisp equivalent of `*pointer` in C.

* `(ffi--mem-set POINTER TYPE VALUE)`.  Copy the Lisp value to the
  memory pointed at by the pointer, using the type to guide the
  conversion.  This is the Lisp equivalent of `*pointer = value` in C.

* `(ffi--type-size TYPE)`.  Return the size of TYPE.

* `(ffi--type-alignment TYPE)`.  Return the alignment needed by TYPE.

* `(ffi--define-struct &rest TYPE...)`.  Define a new foreign structure
  type, whose fields are the indicated types.

* `(ffi--define-union &rest TYPE...)`.  Define a new foreign union
  type, whose fields are the indicated types.

# To-Do List

* See the github issues.
