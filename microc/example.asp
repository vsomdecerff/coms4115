/* The GCD algorithm in MicroC */
int tmp (int a) {
	while(a >= 0) {
		print(a);
		a--;
	} 
	return 0;
}


int main() {
  int a;
  a = 5;
  a++;
  tmp(a);
  return 0;
}
