/*
  
  This program illustrates one producer - multiple consumers problem.
  Synchronization is done by semaphores only. 

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
#define MAXBUFFERCAPACITY 100

int worms;                    // Size of the buffer
sem_t wake;
sem_t mutex_take;
sem_t amountWorms;
sem_t flag;
double start_time, end_time;  // start and end times
int amountOfBabyBirds;        // Amount of baby birds threads


void millisleep(unsigned int ms)
{
  struct timespec req = {0}, rem = {0};
  req.tv_sec = (time_t)(ms / 1000);
  req.tv_nsec=((long)ms % 1000)*1000000L;

  nanosleep(&req, &rem);
}



// Producer thread (aka parent bird)

void *producer() {
  while(1) {
    // Wait until my childs are hungry
    sem_wait(&wake);
    printf("Tweet! My babies need food! \n");
    int found = worms;
    millisleep(2000);
    printf("Tweet! I found %d worms!\n", found);
    // Take down the flag
    sem_wait(&flag);
    // Put worms in the dish 
    while(found --)
    {
      sem_post(&amountWorms);
    }
    // Take up the flag
    sem_post(&flag); 
    printf("Tweet! Now I'm so tired. Zzzz...\n");
    }
}

// Consumer thread (aka baby bird)

void *consumer(void *id) {
  long num = (long)id;

  while(1) {
      printf("Bird #%ld plays around for a bit.\n", num);

      millisleep(2000);

      printf("Bird #%ld - I'm starving, I should go and get some food!\n", num);

      // Mutual exclusion for the dish
      sem_wait(&mutex_take);
      int count;
      int flagNumber;
      sem_getvalue(&amountWorms,&count);

      // If there are no more worms
      if(count == 0){
        printf("Bird #%ld - The bowl is empty. Hold on guys! -- CHIIIIIRP!!\n", num);
        sem_post(&wake); 
      } else {
        printf("Bird #%ld - Sweet! Looks like there are some worms here!\n", num);
      }
        
      // Check the flag, if flag is set and there are worms left, eat them 
      sem_getvalue(&flag,&flagNumber);

      if(flagNumber == 1) {
         printf("Bird #%ld - Mmmm, that's a tasty worm!\n", num);
         millisleep(2000);
         sem_wait(&amountWorms);
      } 
      
      sem_post(&mutex_take);
    }
}

int main(int argc, char *argv[]) {
    int j;
    long i;

    amountOfBabyBirds = atoi(argv[1]);
    worms = atoi(argv[2]);

    // Initialize semaphores
    sem_init(&amountWorms, 0, worms);
    sem_init(&wake, 0, 0);
    sem_init(&mutex_take, 0, 1);
    sem_init(&flag, 0, 1);

    // Initialize threads
    pthread_t producerID;
    pthread_t consumerIDs[amountOfBabyBirds];

    pthread_attr_t attr;

    pthread_attr_init(&attr);
    pthread_attr_setscope(&attr, PTHREAD_SCOPE_SYSTEM);

    pthread_create(&producerID, &attr, producer, NULL);

    for (i = 0; i < amountOfBabyBirds; i++)
    pthread_create(&consumerIDs[i], &attr, consumer, (void *) i);
       
    for (i=0;i<amountOfBabyBirds;i++){
      pthread_join(consumerIDs[i], NULL);
    }

    pthread_join(producerID, NULL);

    exit(EXIT_SUCCESS);
}