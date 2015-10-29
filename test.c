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
