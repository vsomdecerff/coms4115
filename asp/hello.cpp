#include <stdio.h>

int x = 5;
   
    void bar() { 
        x = x+2; 
    }

    void foo() {
        int x = 8;
        bar();
        printf("%d\n", x);
    }

int main(int argc, char** argv) {

	foo();

	return 0;
}

