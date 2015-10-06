# Where your Emacs build lies.
EMACS_BUILDDIR = /home/tromey/Emacs/emacs

LDFLAGS = -shared
LIBS = -lffi -lltdl
CFLAGS += -shared -fPIC -I$(EMACS_BUILDDIR)/src/ -I$(EMACS_BUILDDIR)/lib/

ffi-module.so: ffi-module.o
	$(CC) $(LDFLAGS) -o ffi-module.so ffi-module.o $(LIBS)

ffi-module.o: ffi-module.c

clean:
	-rm -f ffi-module.o ffi-module.so
