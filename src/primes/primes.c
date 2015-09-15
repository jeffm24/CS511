#include "primes.h"

/*
 *
 *
 */
int *find_primes(int start, int end) {
    return NULL;
}

int main(int argc, char **argv) {
    int pid;                        /* pid of the parent/child processes */
    int nums[argc - 1];             /* array to hold the integer command line args */
    int i;

    /* check if enough command line args have been supplied and exit on error if not */
    if (argc <= 1) {
        printf("usage: ./primes <increasing positive integers>\n");
        exit(-1);
    }

    /* get the integers that the user entered */
    for (i = 1 ; i < argc ; i++) {
        nums[i - 1] = atoi(argv[i]);
    }



    return 0;
}
