#include <stdio.h>

void printarr(int size, int* a) {
	int* b = 0;
	int c = 0;
	printf("[");
	for(b = a; c < size ; c++) {
		if(c < size - 1) {
			printf("%d, ", *b++);
		}
		else {
			printf("%d", *b);
		}
	}
	printf("]\n");
}
