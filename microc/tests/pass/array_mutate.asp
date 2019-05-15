
int main() {
	float* a;
	int i;

	a = [0.0 ^ 5];

	for (i=0; i < 5; i++) {
		a@i = (float) i*i;
	}

	for (i=0; i < 5; i++) {
		print(a@i);
	}


return 0;
}
