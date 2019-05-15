int main()
{
	int a;
	int b;

	for (a = 1; a <= 25; a++) {
		for ( b = 1; b <= 25; b++) {
			if (a*a + b*b == 25) {
				print(a);
				print(b);
				return 0;
			}
		}
	}


return 0;
}
