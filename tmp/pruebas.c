// Ejemplo con errores lexicos: 1 error
{
  int d = 1;
  bool e = false;
  int a[4];
  bool b[3];

  struct {
    int as;
    bool bs;
  } str;
  
  str.bs = e && e;
}
