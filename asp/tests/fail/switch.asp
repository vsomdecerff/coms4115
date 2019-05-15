int z;

int eight(){
	return 8;
}

int zup () {
	z+=1;
	return z;
}


int main() {
	int b;
	b = 5;

	switch (b) {
	case (5) {
		print("b was 5");
	 	b+=3;
	}

	case (7) {
		print("b was 7");
	 	b+=1;
	}

	case (eight())
		print("b is 8");
	
	}

	switch (b) {
	case (eight() + 1)
		print("b is 8");
	
	default {
		print("b is something");
		}
	}


	// dangerous example
	b = 8;
	z = 8;
	switch (b) {
	case (zup())
		print("b is equal to z");
	
	default {
		print("b used to equal to z");
		}
	}
return 0;
}
