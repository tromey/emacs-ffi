# Makefile for FFI module.

# This is is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# This is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this.  If not, see <https://www.gnu.org/licenses/>.

-include config.mk

# Where your dynamic-module-enabled Emacs build lies.
EMACS_BUILDDIR ?= /home/tromey/Emacs/emacs

LDFLAGS = -shared
LIBS = -lffi -lltdl
CFLAGS += -g3 -Og -finline-small-functions -shared -fPIC \
  -I$(EMACS_BUILDDIR)/src/ -I$(EMACS_BUILDDIR)/lib/

# Set this to debug make check.
#GDB = gdb --args

all: module test-module

module: ffi-module.so

ffi-module.so: ffi-module.o
	$(CC) $(LDFLAGS) -o ffi-module.so ffi-module.o $(LIBS)

ffi-module.o: ffi-module.c

check: ffi-module.so test.so
	LD_LIBRARY_PATH=`pwd`:$$LD_LIBRARY_PATH; \
	  export LD_LIBRARY_PATH; \
	$(GDB) $(EMACS_BUILDDIR)/src/emacs -batch -L `pwd` -l ert -l test.el \
	  -f ert-run-tests-batch-and-exit

test-module: test.so

test.so: test.o
	$(CC) $(LDFLAGS) -o test.so test.o

test.o: test.c

clean:
	-rm -f ffi.elc ffi-autoloads.el ffi-module.o ffi-module.so
	-rm -f test.o test.so
