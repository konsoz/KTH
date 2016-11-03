/* matrix summation using pthreads

   features: uses a barrier; the Worker[0] computes
             the total sum from partial sums computed by Workers
             and prints the total sum to the standard output

   usage under Linux:
     gcc matrixSum.c -lpthread
     a.out size numWorkers

     Written by Vlad Vlassov
     Edited by Konstantin Sozinov

*/
//#define DEBUG
#ifndef _REENTRANT 
#define _REENTRANT 
#endif 
#include <pthread.h>
#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <time.h>
#include <sys/time.h>
#define MAXSIZE 10000  /* maximum matrix size */
#define MAXWORKERS 10   /* maximum number of workers */

pthread_mutex_t barrier;      /* mutex lock for the barrier */
pthread_mutex_t minBarrier;   /* mutex lock for min */
pthread_mutex_t maxBarrier;   /* mutex lock for max */  
pthread_cond_t go;            /* condition variable for leaving */
int numWorkers;               /* number of workers */ 
int numArrived = 0;           /* number who have arrived */

// Struct for min or max value and position in matrix

typedef struct MinMax {
  int value;
  int xIndex;
  int yIndex;
} MINMAX;

MINMAX min;
MINMAX max;

/* a reusable counter barrier */
void Barrier() {
  pthread_mutex_lock(&barrier);
  numArrived++;
  if (numArrived == numWorkers) {
    numArrived = 0;
    pthread_cond_broadcast(&go);
  } else
    pthread_cond_wait(&go, &barrier);
  pthread_mutex_unlock(&barrier);
}

/* timer */
double read_timer() {
    static bool initialized = false;
    static struct timeval start;
    struct timeval end;
    if( !initialized )
    {
        gettimeofday( &start, NULL );
        initialized = true;
    }
    gettimeofday( &end, NULL );
    return (end.tv_sec - start.tv_sec) + 1.0e-6 * (end.tv_usec - start.tv_usec);
}

double start_time, end_time; /* start and end times */
int size, stripSize;  /* assume size is multiple of numWorkers */
int sums[MAXWORKERS]; /* partial sums */
int matrix[MAXSIZE][MAXSIZE]; /* matrix */

void *worker(void *);

/* read command line, initialize, and create threads */
int main(int argc, char *argv[]) {

  int i, j;
  long l; /* use long in case of a 64-bit system */
  pthread_attr_t attr;
  pthread_t workerid[MAXWORKERS];

  /* set global thread attributes */
  pthread_attr_init(&attr);
  pthread_attr_setscope(&attr, PTHREAD_SCOPE_SYSTEM);

  /* initialize mutex and condition variable */
  pthread_mutex_init(&barrier, NULL);
  pthread_mutex_init(&minBarrier,NULL);
  pthread_mutex_init(&maxBarrier,NULL);
  pthread_cond_init(&go, NULL);

  /* read command line args if any */
  size = (argc > 1)? atoi(argv[1]) : MAXSIZE;
  numWorkers = (argc > 2)? atoi(argv[2]) : MAXWORKERS;
  if (size > MAXSIZE) size = MAXSIZE;
  if (numWorkers > MAXWORKERS) numWorkers = MAXWORKERS;
  stripSize = size/numWorkers;

  /* initialize the matrix */
  for (i = 0; i < size; i++) {
	  for (j = 0; j < size; j++) {
          matrix[i][j] = rand()%99;
	  }
  }

  min.value = matrix[0][0];
  max.value = matrix[0][0];
  
  /* print the matrix */
#ifdef DEBUG
  for (i = 0; i < size; i++) {
	  printf("[ ");
	  for (j = 0; j < size; j++) {
	    printf(" %d", matrix[i][j]);
	  }
	  printf(" ]\n");
  }
  
#endif

  /* do the parallel work: create the workers */
  start_time = read_timer();
  for (l = 0; l < numWorkers; l++)
    pthread_create(&workerid[l], &attr, worker, (void *) l);
  pthread_exit(NULL);
}

/* Each worker sums the values in one strip of the matrix.
   After a barrier, worker(0) computes and prints the total */
void * worker(void *arg) {
  long myid = (long) arg;
  int total, i, j, first, last;

#ifdef DEBUG
  printf("worker %d (pthread id %d) has started\n", myid, pthread_self());
#endif

  /* determine first and last rows of my strip */
  first = myid*stripSize;
  last = (myid == numWorkers - 1) ? (size - 1) : (first + stripSize - 1);

  /* sum values in my strip */
  total = 0;
  for (i = first; i <= last; i++)
    for (j = 0; j < size; j++) {
      // Find maximum and minimum values in matrix
      // Use mutual exclusion to avoid race condition for global variables
      if(matrix[i][j] > max.value) {
          pthread_mutex_lock(&maxBarrier);
          if(matrix[i][j] > max.value) {
            max.value = matrix[i][j];
            max.yIndex = i;
            max.xIndex = j;
          } 
          pthread_mutex_unlock(&maxBarrier);
      }
          if(matrix[i][j] < min.value) {
          pthread_mutex_lock(&minBarrier);
          if(matrix[i][j] < min.value){
            min.value = matrix[i][j];
            min.yIndex = i;
            min.xIndex = j;
          } 
          pthread_mutex_unlock(&minBarrier);
      }
      total += matrix[i][j];
    }
  sums[myid] = total;
  Barrier();
  if (myid == 0) {
    total = 0;
    for (i = 0; i < numWorkers; i++)
      total += sums[i];
    /* get end time */
    end_time = read_timer();
    /* print results */
  printf("The total is %d\n", total);
  printf("Max value : %d\n", max.value);
  printf("Max x position %d\n", max.xIndex);
  printf("Max y position %d\n", max.yIndex);
  printf("Min value : %d\n", min.value);
  printf("Min x position %d\n", min.xIndex);
  printf("Min y position %d\n", min.yIndex);
  printf("The execution time is %g sec\n", end_time - start_time);
  }
}
