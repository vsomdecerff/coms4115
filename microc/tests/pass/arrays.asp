int z ;

int zup() {
	z+=1;
	return z;
}

int main() {
	int* a;
	int i;

	a = [0,1,2,3,4];

	for (i=0; i < 5; i++) {
		print(a@i);
	}

	a = [0 ^ 5];

	for (i=0; i < 5; i++) {
		print(a@i);
	}

	z = -1;
	a = [zup() ^ 5];

	for (i=0; i < 5; i++) {
		print(a@i);
	}

return 0;
}
