float incr(float a) {
	return a + 1;
}

int incr(int a) {
	return a + 1;
}

int main() {
	int a;
	float b;
	b = 4.0;
	a = 4;
	print(incr(a));
 	print(incr(b));
return 0;
}
