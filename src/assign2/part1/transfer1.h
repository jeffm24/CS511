#ifndef _TRANSFER_H_
#define _TRANSFER_H_
#ifndef NULL
#define NULL ((void*)0)
#endif

#define _GNU_SOURCE
#include <stdlib.h>
#include <unistd.h>
#include <stdio.h>
#include <errno.h>
#include <string.h>
#include <pthread.h>
#include <semaphore.h>
#include "cbuf.h"

/*
  Author: Jeff Mariconda
  Class: CS-511


*/

typedef struct s_glenv {
    char *in_name, *out_name;       /* names of the input and output files */
    sem_t lock;                     /* lock semaphore for controlling access to critical section */
    useconds_t drain_sleep;         /* sleep time of the drain thread */
    useconds_t fill_sleep;          /* sleep time of the fill thread */
} t_glenv;

t_glenv env;

/* function that the fill buffer thread will run */
void *fill_buffer(void *p);

/* function that the drain buffer thread will run */
void *drain_buffer(void *p);

#endif
