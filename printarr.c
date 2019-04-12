#include <stdio.h>

void printarr(int* a) {
	int* b = 0;
	int c = 0;
	printf("[ ");
	for(b = a; *b ; b++) {
		printf("%d ", *b);
	}
	printf("]\n");
}
