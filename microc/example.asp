/* The GCD algorithm in MicroC */
int a;
int b;

int gcd(int a, int b) {
  while (a != b) {
    if (b < a) a = a - b;
    else b = b - a;
  }
  return a;
}

int main() {
  int x;
  int y;
  int i;
  a = 18;
  b = 9;
  x = 2;
  y = 14;
  print(gcd(x,y));
  print(gcd(3,15));
  print(gcd(99,121));
  print(gcd(a,b));

  for ( i = 0; i < 4; i = i + 1)
  {
     print(i);
  }

  return 0;
}