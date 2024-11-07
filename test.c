/* FFI module tests

This is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or (at
your option) any later version.

This is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this.  If not, see <https://www.gnu.org/licenses/>.  */

#include <stdbool.h>

int test_function (void)
{
  return 27;
}

char test_function_char (void)
{
  return 27;
}

bool test_not (bool x)
{
  return !x;
}

const char *test_c_string (void)
{
  return "hello";
}

int test_call_callback (int (*func) (int))
{
  return func (22);
}

int test_add (int x, int y)
{
  return x + y;
}

struct test_struct
{
  char *stringval;
  int intval;
};

struct test_struct test_get_struct (void)
{
  struct test_struct result;
  result.stringval = "string";
  result.intval = 23;
  return result;
}

int test_get_struct_int (struct test_struct ts)
{
  return ts.intval;
}

union test_union
{
  unsigned char cval;
  int ival;
};

union test_union test_get_union (void)
{
  union test_union u;
  u.ival = -1;
  return u;
}
