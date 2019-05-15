int fib(int n) 
{ 
   if (n <= 1) 
      return n; 
   return fib(n-1) + fib(n-2); 
} 
  


int main() {
	int n;
	int f;
	int sum;
	bool cont; 
	n = 0;
	sum = 0;
	cont = true;
	
	while(cont) {
		f = fib(n);
		if (f > 90) {
			cont = false;
		} else {
			if (f % 2 == 0) {
				sum +=f ;	
			}
		}
		n++;
	}

	print(sum);

	return 0;
}

