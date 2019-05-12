#include <stdio.h>

void printarr(int* a) {
	int* b = ++a;
	int c = 0;
	int d = a[-1];
	printf("[");
	for(; c < d ; c++) {
		if(c < d - 1) {
			printf("%d, ", *b++);
		}
		else {
			printf("%d", *b);
		}
	}
	printf("]\n");
}
