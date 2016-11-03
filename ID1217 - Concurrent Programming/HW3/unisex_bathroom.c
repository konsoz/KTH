/**
  Suppose there is only one bathroom in your department. 
  It can be used by any number of men or any number of women, but not at the same time.
  Men and women are represented as threads that repeatedly work (sleep for a random amount of time) and use the bathroom. 
  Solution ensures that any person (man or woman) which is waiting to enter the bathroom eventually gets to do so.
  Synchronization is done only by semophores.

  Written by Konstantin Sozinov.

**/
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
#define DELAY 2500


int manCount;   // Amount of man currently in bathroom
int womanCount; // Amount of women currently in bathroom
sem_t bathroomAccess; // Mutex exclusion for semaphore
sem_t manCountAccess; // Mutex exclusion for men count
sem_t queue;          // global queue between women and men
sem_t womanCountAccess; // Mutex exclusion for women count   

int amountOfWomanThreads;
int amountOfManThreads;

// Bathroom struct
typedef struct bathroom {
    unsigned int    males;
    unsigned int    females;
} BATHROOM;


BATHROOM bath;


// Millisleep function for a thread
void millisleep(unsigned int ms)
{
  struct timespec req = {0}, rem = {0};
  req.tv_sec = (time_t)(ms / 1000);
  req.tv_nsec=((long)ms % 1000)*1000000L;

  nanosleep(&req, &rem);
}


// Woman thread

void *woman(void *id) {
  while(1){
    long threadId = (long)id;

    // Work some time
    millisleep(rand () % DELAY);

    // Wait on the queue
    sem_wait(&queue);

    //Mutex for woman count
    sem_wait(&womanCountAccess);
    printf("Woman number #%ld is need to use the bathroom NOW\n",threadId);

    // If it is first woman, then try to get into bathroom
    if(womanCount==0) {
    sem_wait(&bathroomAccess);
    }

    // Update amount of woman 
    womanCount++;

    // Unlock the queue and count
    sem_post(&queue);
    sem_post(&womanCountAccess);

    printf("Woman number #%ld is visiting bathroom now\n",threadId);

    //Update bathroom and print status
    bath.females++;
    millisleep(rand () % DELAY);
    printf("####Bathroom status#### \n ");
    printf("Males : %d ",bath.males);
    printf("Females : %d \n",bath.females);
    bath.females--;

    // Try to lock woman count
    sem_wait(&womanCountAccess);
    womanCount--;

    // If there are no more women currently in bathroom, unlock the bathroom
    if(womanCount==0) sem_post(&bathroomAccess);
    printf("Woman number #%ld is leaving bathroom now \n", threadId);
    sem_post(&womanCountAccess);
  }
}

// Man thread, mirror image of woman thread code

void *man(void *id) {
  while(1){
  long threadId = (long)id;

  millisleep(rand() % DELAY);
  sem_wait(&queue);
  sem_wait(&manCountAccess);
  printf("Man number #%ld is need to use the bathroom NOW\n",threadId);
  if(manCount==0) {
    sem_wait(&bathroomAccess);
  } 
  manCount++;
  sem_post(&queue);
  sem_post(&manCountAccess);

  printf("Man number #%ld is visiting bathroom now\n",threadId);
  bath.males++;
  millisleep(rand () % DELAY);
   printf("####Bathroom status#### \n ");
  printf("Males : %d ",bath.males);
  printf("Females : %d \n",bath.females);
  bath.males--;

  sem_wait(&manCountAccess);
  manCount--;
  if(manCount==0) sem_post(&bathroomAccess);
  printf("Man number #%ld is leaving bathroom now \n", threadId);
  sem_post(&manCountAccess);
  }
}

int main(int argc, char *argv[]) {
    long j;
    long i;

    amountOfManThreads = atoi(argv[1]);
    amountOfWomanThreads = atoi(argv[2]);

    // Initialize semaphores
    sem_init(&bathroomAccess, SHARED, 1);
    sem_init(&manCountAccess, SHARED, 1);
    sem_init(&queue, SHARED, 1);
    sem_init(&womanCountAccess, SHARED,1);

    // Initialize threads
    pthread_t writersIDs[amountOfWomanThreads];
    pthread_t readersIDs[amountOfManThreads];

    pthread_attr_t attr;

    pthread_attr_init(&attr);
    pthread_attr_setscope(&attr, PTHREAD_SCOPE_SYSTEM);

    for(j = 0; j<amountOfWomanThreads; j++)   
    pthread_create(&writersIDs[j], &attr, woman, (void *) j);

    for (i = 0; i<amountOfManThreads; i++)
    pthread_create(&readersIDs[i], &attr, man, (void *) i);
       
    for (j=0;j<amountOfWomanThreads;j++)
    pthread_join(writersIDs[j], NULL);
    
    for(i=0;i<amountOfManThreads;i++)
    pthread_join(readersIDs[i],NULL);
    

    exit(EXIT_SUCCESS);
}