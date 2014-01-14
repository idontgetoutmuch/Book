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

	if 	(x == 0 && y > 0 && y < h)	return 80;
	else if (y == 0 && x > 0 && x < w)	return 20;
	else if (x == w && y > 0 && y < h)	return 0;
	else if	(y == h && x > 0 && x < w)	return 180;
	else					return 0;
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
	, Matrix* matDest)	// Where to write the result of the first iteration.
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

	// Return result of last iteration.
	return	matTmp;
}

int main(int argc, char** argv)
{
	// Argument parsing
	if (argc != 5) {
		printf("Usage: laplace <width> <height> <iterations> <output file.ppm>\n");
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


	// Setup boundary condition matrices
	Matrix*	matBoundMask	= createMatrix (width, height, mkBoundaryMask);
	Matrix*	matBoundValue	= createMatrix (width, height, mkBoundaryValue);

	// Set the initial matrix to the same as the boundary conditions.
	Matrix*	matInitial	= createMatrix (width, height, mkBoundaryValue);

	// A destination buffer, to write the next iteration into.
	Matrix* matDest		= createMatrix (width, height, mkBoundaryValue);

	// Run the solver.
	//	The result is either the matInitial or matBuffer, depending
	//	on how many iterations we took.
	struct benchtime *bt = bench_begin();

	Matrix* matFinal
		= solve ( iterations
			, matBoundMask, matBoundValue
			, matInitial, matDest);

	bench_done(bt);

	// Write the output to a PPM file.
	writeMatrixAsPPM(fileName, matFinal);

	// Cleanup
	freeMatrix (matBoundMask);
	freeMatrix (matBoundValue);
	freeMatrix (matInitial);
	freeMatrix (matDest);
}
