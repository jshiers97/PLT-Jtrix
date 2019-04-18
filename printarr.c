#include <stdio.h>

void printarr(int size, int* a) {
	int* b = ++a;
	int c = 0;
	int d = *b++;
	printf("[");
	for(; c < d ; c++) {
		if(c < size - 1) {
			printf("%d, ", *b++);
		}
		else {
			printf("%d", *b);
		}
	}
	printf("]\n");
}
