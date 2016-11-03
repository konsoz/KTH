/*
  
  This program is a paralell version of linux diff command.
  It uses producer/consumer parallel paradigm.
  Producer produces two lines and consumer compares them.

  Written by Konstantin Sozinov.

*/
#ifndef _REENTRANT 
#define _REENTRANT 
#endif 
#include <pthread.h>
#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <time.h>
#include <sys/time.h>
#include <semaphore.h>
#include <string.h>
#define SHARED 1

sem_t empty, full;            // Semaphores
bool done;                    // Flag
double start_time, end_time; /* start and end times */

// Struct for 2 lines from 2 files
typedef struct Line {
    char line1[1000], line2[1000];
} Line;

Line *buffer = NULL; 

// Pointers to files
typedef struct Files {
   FILE * file1;
   FILE * file2;
} Files;

Files files;

/* timer */
double read_timer() {
    static bool initialized = false;
    static struct timeval start;
    struct timeval end;
    if( !initialized ) {
        gettimeofday( &start, NULL );
        initialized = true;
    }
    gettimeofday( &end, NULL );
    return (end.tv_sec - start.tv_sec) + 1.0e-6 * (end.tv_usec - start.tv_usec);
}

// Producer thread

void *producer() {
  char * line1 = NULL, * line2 = NULL;
  size_t len1 = 0, len2 = 0;
  ssize_t read1, read2;
  while(true) {
      read1 = getline(&line1, &len1, files.file1);
      read2 = getline(&line2, &len2, files.file2);

      // If there are no more content in files
      if(read1 == -1 && read2 == -1) break;

      // If file 1 is done
      if(read1 == -1) {                     
        printf("%s\n",line2);
      // If file 2 is done  
      } else if (read2 == -1) {             
        printf("%s\n",line1);
      } else {
        // Wait if buffer is full
        sem_wait(&empty);
        buffer = malloc(sizeof *buffer);
        strcpy(buffer->line1, line1);
        strcpy(buffer->line2, line2);
        // Signal that buffer is full
        sem_post(&full);
      }
    }
    sem_wait(&empty);
    buffer = NULL;
    sem_post(&full);
}

// Consumer thread

void *consumer() {
    while(true) {
        // Wait if buffer is empty
        sem_wait(&full);
        if(buffer == NULL)
            break;
        char *line1 = buffer->line1;
        char *line2 = buffer->line2;
        if (strcmp(line1, line2) == 0) {
        }
        else {
        // not matched 
        printf("Consumer found difference in those lines '%s' to '%s'\n", line1, line2);
        }
        free(buffer);
        // Signal that buffer is empty
        sem_post(&empty);
    }
}

int main(int argc, char *argv[]) {
    pthread_t consumerID;
    pthread_t producerID;

    /* initiate sempathor */
    sem_init(&empty, SHARED, 1);
    sem_init(&full, SHARED, 0);

    /* open files */
    FILE * fp1, * fp2;

    fp1 = fopen(argv[1], "r");
    if (fp1 == NULL)
        exit(EXIT_FAILURE);
    fp2 = fopen(argv[2], "r");
    if (fp2 == NULL)
        exit(EXIT_FAILURE);

    // Global pointers to files
    files.file1=fp1;
    files.file2=fp2;

    // Start timer and create threads
    start_time = read_timer();

    pthread_create(&consumerID, NULL, consumer, NULL);
    pthread_create(&producerID, NULL, producer, NULL);

    pthread_join(producerID, NULL);
    pthread_join(consumerID, NULL);
    
    end_time = read_timer();

    printf("The execution time is %g sec\n", end_time - start_time);

    fclose(fp1);
    fclose(fp2);
  
    exit(EXIT_SUCCESS);
}