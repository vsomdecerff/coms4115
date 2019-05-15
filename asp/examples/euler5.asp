int main() {
	int i;
	int j;
	bool contw;
	bool contf;
	i = 1;
	contw = true;
	contf = true;
	
	while (contw) {
		for(j = 1; j <= 10 and contf; j++) {
			if ( not (i % j == 0 )) {
				contf = false;
			}
		}
		if (contf) {
			contw = false;
			print(i);
		}
		else {
			i+=1;
			contf = true;
		}
	}

	return 0;
}
