/* The GCD algorithm in MicroC */
int tmp2(int a) {
	do {
	 	print(a);
        a--;
	}
	while( a >= 0)

}

int tmp(int a) {
    while( a >= 0) {
        print(a);
        a--;
    }
}


int main() {
  int a;
  a = 5;
  tmp2(a);
 
  return 0;
}
