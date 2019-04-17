#include <stdio.h>

void printfltarr(int size, double* a) {
	double* b = 0;
	int c = 0;
	printf("[");
	for(b = a; c < size; c++) {
		if(c < size - 1) {
			printf("%f, ", *b++);
		}
		else {
			printf("%f", *b);
		}
	}
	printf("]\n");
}
