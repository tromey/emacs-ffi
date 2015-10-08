# Where your Emacs build lies.
EMACS_BUILDDIR = /home/tromey/Emacs/emacs

LDFLAGS = -shared
LIBS = -lffi -lltdl
CFLAGS += -g3 -Og -finline-small-functions -shared -fPIC -I$(EMACS_BUILDDIR)/src/ -I$(EMACS_BUILDDIR)/lib/

# Set this to debug make check.
#GDB = gdb --args

all: ffi-module.so

ffi-module.so: ffi-module.o
	$(CC) $(LDFLAGS) -o ffi-module.so ffi-module.o $(LIBS)

ffi-module.o: ffi-module.c

check: ffi-module.so test.so
	LD_LIBRARY_PATH=`pwd`:$$LD_LIBRARY_PATH; \
	export LD_LIBRARY_PATH; \
	$(GDB) $(EMACS_BUILDDIR)/src/emacs -batch -L `pwd` -l ert -l test.el \
	  -f ert-run-tests-batch-and-exit

test.so: test.o
	$(CC) $(LDFLAGS) -o test.so test.o

test.o: test.c

clean:
	-rm -f ffi-module.o ffi-module.so test.o test.so
