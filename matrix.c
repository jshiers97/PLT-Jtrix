#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

void idx_check(int idx, int size) {
	if(idx < 0) {
		printf("Fatal error: exception Failure(\"Index must be greater than 0\")");
		exit(1);
	} else if(idx > size) {
		printf("Fatal error: exception Failure(\"Index out of bounds\")");
		exit(1);
	}
}

int** init_mat_i(int r, int c) {
	int** x = (int **)malloc(sizeof(int*)*(r + 1));
	assert(x != NULL);

	int* dim = (int*)malloc(sizeof(int)*3);
	assert(dim != NULL);
	dim[0] = 2;
	dim[1] = r;
	dim[2] = c;

	x[0] = dim;

	for(int i = 1; i < r + 1; i++) {
		int* t = (int *)malloc(sizeof(int)*(c+1));
		assert(t != NULL);
		x[i] = t;
	}
	return x;
}

double** init_mat_f(int r, int c) {
	double** x = (double**)malloc(sizeof(double*)*(r+1));
	assert(x != NULL);

	double* dim = (double*)malloc(sizeof(double)*3);
	assert(dim != NULL);
	dim[0] = 2.;
	dim[1] = (double) r;
	dim[2] = (double) c;

	x[0] = dim;

	for(int i = 1; i < r + 1; i++) {
		double* t = (double *)malloc(sizeof(double)*(c+1));
		assert(t != NULL);
		x[i] = t;
	}

	return x;
}

int* col_i(int** mat, int c) {
	idx_check(c+1, mat[0][2]);
	
	int*x = (int *)malloc(sizeof(int)*(mat[0][1]+1));
	assert(x != NULL);

	x[0] = mat[0][1];

	for(int i=1; i < mat[0][1] + 1; i++) {
		x[i] = mat[i][c + 1];
		printf("x[%d] = %d\n", i, x[i]);
	}

	return x;
}

double* col_f(double** mat, int c) {
	idx_check(c+1, (int) mat[0][2]);

	double* x = (double*)malloc(sizeof(double)*mat[0][1]+1);
	assert(x != NULL);

	x[0] = mat[0][1];

	for(int i=1; i < ((int) mat[0][1]) + 1; i++) {
		x[i] = mat[i][c + 1];
	}

	return x;
}

double** splice_col_f(double** old, int c) {
	idx_check(c+1, (int) old[0][2]);

	double** x = (double**)malloc(sizeof(int*)*(old[0][1]+1));
	assert(x != NULL);

	double* dim = (double*)malloc(sizeof(double)*3);
	dim[0] = 2;
	dim[1] = old[0][1];
	dim[2] = old[0][2] - 1;

	x[0] = dim;

	for(int i = 1; i < old[0][1] + 1; i++) {
		double* t = (double*)malloc(sizeof(double)*(old[0][2]));
		assert(t != NULL);
		for(int j = 0; j < old[0][1] + 1; j++) {
			if(j < c + 1) {
				if(j == 0) {
					t[j] = old[i][j] - 1;
				} 
				else {
					t[j] = old[i][j];
				}
			}
			else if(j > c + 1) {
				if(j == 0) {
					t[j - 1] = old[i][j] - 1;
				}
				else {
					t[j - 1] = old[i][j];
				}
			}
		}
		x[i] = t;
	}

	return x;
}

int** splice_col_i(int** old, int c) {
	idx_check(c+1, old[0][2]);

	int** x = (int**)malloc(sizeof(int*)*(old[0][1]+1));
	assert(x != NULL);

	int* dim = (int*)malloc(sizeof(int)*3);
	dim[0] = 2;
	dim[1] = old[0][1];
	dim[2] = old[0][2] - 1;

	x[0] = dim;

	for(int i = 1; i < old[0][1] + 1; i++) {
		int* t = (int*)malloc(sizeof(int)*(old[0][2]));
		assert(t != NULL);
		for(int j = 0; j < old[0][1] + 1; j++) {
			if(j < c + 1) {
				if(j == 0) {
					t[j] = old[i][j] - 1;
				} 
				else {
					t[j] = old[i][j];
				}
			}
			else if(j > c + 1) {
				if(j == 0) {
					t[j - 1] = old[i][j] - 1;
				}
				else {
					t[j - 1] = old[i][j];
				}
			}
		}
		x[i] = t;
	}

	return x;
}

int** splice_row_i(int** old, int r) {
	idx_check(r+1, old[0][1]);
	int ** x = (int **)malloc(sizeof(int*)*(old[0][1]));
	assert(x != NULL);
	for(int i = 0; i < r + 1; i++) {
		int* t = (int *)malloc(sizeof(int) * (old[0][2] +1));
		assert(t != NULL);
		for(int j = 0; j < old[0][2] + 1; j++) {
			t[j] = old[i][j];
		}
		x[i] = t;
	}

	for(int i = r + 2; i < old[0][1] + 1; i++) {
		int* t = (int *)malloc(sizeof(int) * (old[0][2] +1));
		assert(t != NULL);
		for(int j = 0; j < old[0][2] + 1; j++) {
			t[j] = old[i][j];
		}
		x[i-1] = t;
	}
	       	
	return x;
}

double** splice_row_f(double** old, int r) {
	idx_check(r+1, (int) old[0][1]);
	double** x = (double**)malloc(sizeof(double*)*(old[0][1]));
	assert(x != NULL);
	for(int i = 0; i < r + 1; i++) {
		double* t = (double*)malloc(sizeof(double) * (old[0][2] +1));
		assert(t != NULL);
		for(int j = 0; j < old[0][2] + 1; j++) {
			t[j] = old[i][j];
		}
		x[i] = t;
	}

	for(int i = r + 2; i < old[0][1] + 1; i++) {
		double* t = (double *)malloc(sizeof(double) * (old[0][2] +1));
		assert(t != NULL);
		for(int j = 0; j < old[0][2] + 1; j++) {
			t[j] = old[i][j];
		}
		x[i-1] = t;
	}
	       	
	return x;
}

int** transpose_i(int** old) {
	int i = 0;

	int** x = (int **)malloc(sizeof(int*)*(old[0][2]+1));
	assert(x != NULL);


	for(; i < old[0][1] + 1; i++) {
		int* t = (int *)malloc(sizeof(int) * (old[0][2] + 1));
		assert(t != NULL);
		for(int j = 0; j < old[0][2] + 1; j++) {
			t[j] = old[j][i];
		}
		x[i] = t;
	}

	return x;
}

double** transpose_f(double** old) {
	int i = 0;
	double** x = (double**)malloc(sizeof(double*)*(old[0][2]+1));
	assert(x != NULL);

	for(; i < ((int) old[0][1]) + 1; i++) {
		double* t = (double *)malloc(sizeof(double)*(((int) old[0][2])+1));
		assert(t != NULL);
		for(int j = 0; j < ((int) old[0][2]) + 1; j++) {
			t[j] = old[j][i];
		}
		x[i] = t;
	}

	return x;
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

double** switch_rows_f(double** mat, int x, int y) {
	idx_check(x, (int) mat[0][1]);
	idx_check(y, (int) mat[0][1]);
	
	int i = 0;
	for(; i < (int) mat[0][2]; i++) {
		double temp = mat[x+1][i+1];
		mat[x+1][i+1] = mat[y+1][i+1];
		mat[y+1][i+1] = temp;
	}

	return mat;
}
