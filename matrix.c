#include <stdio.h>
#include <stdlib.h>

void idx_check(int idx, int size) {
	if(idx < 0) {
		printf("Index must be greater than 0! Aborting...\n");
		exit(1);
	} else if(idx > size) {
		printf("Index out of bounds! Aborting...\n");
		exit(1);
	}
}

int** transpose_i(int** old, int** new) {
	int i = 0;

	for(; i < old[0][2]; i++) {
		for(int j = 0; j < old[0][1]; j++) {
			new[i + 1][j + 1] =
		}
	}

	return old;
}

int** switch_rows_i(int** mat, int x, int y) {
	idx_check(x, mat[0][1]);
	idx_check(y, mat[0][1]);

	int i = 0;
	for(; i < mat[0][2]; i++) {
		int temp = mat[x+1][i+1];
		mat[x+1][i+1] = mat[y+1][i+1];
		mat[y+1][i+1] = temp;
	}

	return mat;
}

float** switch_rows_f(float** mat, int x, int y) {
	idx_check(x, (int) mat[0][1]);
	idx_check(y, (int) mat[0][1]);

	int i = 0;
	for(; i < (int) mat[0][2]; i++) {
		float temp = mat[x+1][i+1];
		mat[x+1][i+1] = mat[y+1][i+1];
		mat[y+1][i+1] = temp;
	}

	return mat;
}
