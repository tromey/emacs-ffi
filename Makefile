EMACS ?= emacs
# Where your dynamic-module-enabled Emacs build lies.
# EMACS = /home/tromey/Emacs/emacs/src/emacs
CASK ?= cask

LDFLAGS = -shared
LIBS = -lffi -lltdl
CFLAGS += -g3 -Og -finline-small-functions -shared -fPIC -I$(EMACS_BUILDDIR)/src/ -I$(EMACS_BUILDDIR)/lib/

# Set this to debug make check.
#GDB = gdb --args

all: ffi-module.so

test: check

ffi-module.so: ffi-module.o
	$(CC) $(LDFLAGS) -o ffi-module.so ffi-module.o $(LIBS)

ffi-module.o: ffi-module.c

check: ffi-module.so test/ffi-test.so
	LD_LIBRARY_PATH=`pwd`:$$LD_LIBRARY_PATH; \
	export LD_LIBRARY_PATH; \
	$(GDB) $(CASK) exec ert-runner

test/ffi-test.so: test/ffi-test.o
	$(CC) $(LDFLAGS) -o test/ffi-test.so test/ffi-test.o

test/ffi-test.o: test/ffi-test.c

clean:
	-rm -f ffi-module.o ffi-module.so test.o test.so
