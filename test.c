int test_function (void)
{
  return 27;
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
