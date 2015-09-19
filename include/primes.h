#ifndef _PRIMES_H_
#define _PRIMES_H_

#ifndef NULL
#define NULL ((void*)0)
#endif

#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/select.h>
#include <sys/wait.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <errno.h>
#include <string.h>

/*
  Author: Jeff Mariconda
  Class: cs511

  Program that uses child processes and IPC to find all of the primes in various ranges of numbers.
*/

//Finds all primes in the given range (inclusive)
int find_primes(int start, int end, int fd);

#endif
