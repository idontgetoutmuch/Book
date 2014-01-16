#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include "Matrix.h"
#include "Timing.h"

double mkBoundaryMask (int width, int height, int x, int y)
{
	int w 	= width  - 1;
	int h	= height - 1;

	if      (x == 0)			return 0;
	else if (y == 0)			return 0;
	else if (x >= w)			return 0;
	else if (y >= h)			return 0;
	else					return 1;
}

double mkBoundaryValue (int width, int height, int x, int y)
{
	int w 	= width  - 1;
	int h	= height - 1;
	double u, v;

	if 	(x == 0 && y > 0 && y < h) {
	  v = ((double) y) / ((double) h);
	  return v / (1 + v*v);
	}
	else if (y == 0 && x > 0 && x < w) {
	  return 0;
	}
	else if (x == w && y > 0 && y < h) {
	  v = ((double) y) / ((double) h);
	  return v / (4 + v*v);
	}
	else if (y == h && x > 0 && x < w) {
	  u = ((double) x) / ((double) w);
	  return 1 / ((1 + u)*(1 + u) + 1);
	}
	else {
	  return 0;
	}
}

void applyBoundary
	( Matrix* matDest
	, Matrix* matBoundMask
	, Matrix* matBoundValue)
{
	assert(matricesHaveSameShape(matDest, matBoundMask));
	assert(matricesHaveSameShape(matDest, matBoundValue));

	for (int y = 0; y < matDest->height; y++)
	for (int x = 0; x < matDest->width; x++) {
		matDest->data[y][x]
			= (matDest->data[y][x] * matBoundMask->data[y][x])
			+ matBoundValue->data[y][x];
	}
}

void relaxLaplace
	( Matrix* matDest
	, Matrix* matSrc)
{
	assert(matricesHaveSameShape(matDest, matSrc));

	for (int y = 1; y < matDest->height - 1; y++)
	for (int x = 1; x < matDest->width  - 1; x++) {
		double left	= matSrc->data[y]  [x-1];
		double right	= matSrc->data[y]  [x+1];
		double up	= matSrc->data[y+1][x];
		double down	= matSrc->data[y-1][x];

		matDest->data[y][x] = (left + right + up + down) / 4;
	}
}

Matrix* solve
	( int iterations
	, Matrix* matBoundMask
	, Matrix* matBoundValue
	, Matrix* matInitial
	, Matrix* matDest)
{
	assert(matricesHaveSameShape(matDest, matInitial));
	assert(matricesHaveSameShape(matDest, matBoundValue));
	assert(matricesHaveSameShape(matDest, matBoundMask));

	Matrix* matTmp	= 0;

	for (int i = 0; i < iterations; i++) {
		relaxLaplace  (matDest, matInitial);
		applyBoundary (matDest, matBoundMask, matBoundValue);

		matTmp		= matDest;
		matDest		= matInitial;
		matInitial	= matTmp;
	}
	return	matTmp;
}

int main(int argc, char** argv)
{
	if (argc != 4) {
		printf("Usage: laplace <width> <height> <iterations>\n");
		printf("  width, height  :: Int      The width and height of the matrix\n");
		printf("  iterations     :: Int      Number of iterations to use in the solver\n");
		exit(0);
	}
	int width	= 0;
	int height	= 0;
	int iterations	= 0;

	if(sscanf(argv[1], "%d", &width) != 1) {
		printf("laplace: can't parse matrix width\n");
		exit(1);
	}

	if(sscanf(argv[2], "%d", &height) != 1) {
		printf("laplace: can't parse matrix height\n");
		exit(1);
	}

	if(sscanf(argv[3], "%d", &iterations) != 1) {
		printf("laplace: can't parse iterations\n");
		exit(1);
	}

	char* fileName	= argv[4];

        printf("%i, %i, %i\n", width, height, iterations);

	Matrix*	matBoundMask	= createMatrix (width, height, mkBoundaryMask);
	Matrix*	matBoundValue	= createMatrix (width, height, mkBoundaryValue);
	Matrix*	matInitial	= createMatrix (width, height, mkBoundaryValue);
	Matrix* matDest		= createMatrix (width, height, mkBoundaryValue);

	struct benchtime *bt = bench_begin();

	Matrix* matFinal
		= solve ( iterations
			, matBoundMask, matBoundValue
			, matInitial, matDest);

	bench_done(bt);

        int i, j;

        for (i = 0; i < width; i++) {
          for (j = 0; j < height; j++) {
            printf("%16.10e\n", matFinal->data[i][j]);
          }
        }

	freeMatrix (matBoundMask);
	freeMatrix (matBoundValue);
	freeMatrix (matInitial);
	freeMatrix (matDest);
}
