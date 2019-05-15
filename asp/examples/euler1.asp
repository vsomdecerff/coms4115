//Find the sum of all the multiples of 3 or 5 below 1000.

int main() {
	int i;
	int sum;
	sum = 0;
	i = 0;
	while (i < 1000) {
		if ( i % 15 == 0 ) 
			sum +=i;
		else {
			if (i % 3 == 0 or i % 5 == 0) 
				sum+=i;
		}
	  i++;
	}
	print(sum);
	return 0;
}
