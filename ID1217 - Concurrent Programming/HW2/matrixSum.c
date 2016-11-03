/* matrix summation using OpenMP

   usage with gcc (version 4.2 or higher required):
     gcc -O -fopenmp -o matrixSum-openmp matrixSum-openmp.c 
     ./matrixSum-openmp size numWorkers

     Written by : Vladimir Vlassov
     Edited by : Konstantin Sozinov

*/

#include <omp.h>

double start_time, end_time; // benchmark time

#include <stdio.h>
#define MAXSIZE 23000  /* maximum matrix size */
#define MAXWORKERS 8   /* maximum number of workers */

int numWorkers; // number of threads
int size; // size of the matrix
int matrix[MAXSIZE][MAXSIZE];
void *Worker(void *);

// Struct to keep track on max,min values and position
typedef struct MinMax {
  int value;
  int xIndex;
  int yIndex;
} MINMAX;

MINMAX min;
MINMAX max;

/* read command line, initialize, and create threads */
int main(int argc, char *argv[]) {
  int i, j, total=0;

  /* read command line args if any */
  size = (argc > 1)? atoi(argv[1]) : MAXSIZE;
  numWorkers = (argc > 2)? atoi(argv[2]) : MAXWORKERS;
  if (size > MAXSIZE) size = MAXSIZE;
  if (numWorkers > MAXWORKERS) numWorkers = MAXWORKERS;

  omp_set_num_threads(numWorkers);

  /* initialize the matrix */
  for (i = 0; i < size; i++) {
   //   printf("[ ");
	  for (j = 0; j < size; j++) {
      matrix[i][j] = rand()%99;
    //  	  printf(" %d", matrix[i][j]);
	  }
	  //	  printf(" ]\n");
  }

  min.value = matrix[0][0];
  max.value = matrix[0][0];


  start_time = omp_get_wtime();

// Parallel region begins here
#pragma omp parallel
{
  // pragma for for-loop, reduction on total in order to update the sum
  // pragma splits first for-loop and gives work to every thread.
  // every thread works on next for-loop in parallel.
  #pragma omp for reduction (+:total) private(j)
    for (i = 0; i < size; i++)
    for (j = 0; j < size; j++){
      total += matrix[i][j];
      if(matrix[i][j] > max.value) {
        // critical section in order to update max value
        #pragma omp critical(maxvalue)
        if(matrix[i][j] > max.value){
          max.value = matrix[i][j];
            max.yIndex = i;
            max.xIndex = j;
        }
      }
      if(matrix[i][j] < min.value){
        // critical section in order to update min value
        #pragma omp critical(minvalue)
        if(matrix[i][j] < min.value){
           min.value = matrix[i][j];
            min.yIndex = i;
            min.xIndex = j;
        }
      }
    }
  // let the master thread print the results
  #pragma omp master
  {
  end_time = omp_get_wtime();

  printf("The total is %d\n", total);
  printf("Max value : %d\n", max.value);
  printf("Max x position %d\n", max.xIndex);
  printf("Max y position %d\n", max.yIndex);
  printf("Min value : %d\n", min.value);
  printf("Min x position %d\n", min.xIndex);
  printf("Min y position %d\n", min.yIndex);
  printf("It took %g seconds\n", end_time - start_time);
  
  }
}
  
}
