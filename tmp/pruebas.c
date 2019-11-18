// Ejemplo con errores lexicos: 1 error
{
  int d = 3;
  bool e = false;
  int a[4];
  bool b[3];
  bool aa;
  struct {
    int as;
    bool bs;
  } str;
  str.bs = true;
  e = b[4];
}
