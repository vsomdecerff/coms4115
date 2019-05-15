
int main() {
	float* a;
	int i;

	a = [0.0 ^ 5];

	i = 0;
	for (i=0; i < 5; i++) {
		a@i = (float) i*i;
	}

	i = 0;
	while (i < 5) {
		print(a@i);

		i+=1;
	}


	do {
		print(i);
	} while(i < 5)


return 0;
}
