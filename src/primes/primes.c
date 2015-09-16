#include "primes.h"

/*
 *
 *
 */
void find_primes(int start, int end) {
    int i, j, flag, temp, count = 0;
    
    if (end < 2) {
	printf("There are no primes upto %d\n", end);
	exit(0);
    }
    printf("Prime numbers are \n");
    temp = start;
    if (start % 2 == 0) {
	start++;
    }
    for (i = start ; i <= end ; i += 2) {
	flag = 0;
	for (j = 2 ; j <= i / 2 ; j++) {
	    if ((i % j) == 0) {
		flag = 1;
		break;
	    }
	}
	if (flag == 0) {
	    printf("%d\n", i);
	    count++;
	}
    }
    printf("Number of primes between %d & %d = %d\n", temp, end, count);
}

int main(int argc, char **argv) {
    pid_t pid;                      /* pid of the parent/child processes */
    int nums[argc - 1];             /* array to hold the integer command line args */
    int fd[2];                      /* file descriptor array for pipe  */
    int i;
    int bottom;
    int top;
  
    /* check if enough command line args have been supplied and exit on error if not */
    if (argc <= 1) {
	printf("usage: ./primes <increasing positive integers>\n");
	exit(-1);
    }
    
    /* get the integers that the user entered */
    for (i = 1 ; i < argc ; i++) {
	nums[i - 1] = atoi(argv[i]);
    }
    
    for (i = 0 ; i < argc - 1 ; i++) {
	
	/* even child case - use pipe  */
	if ((i + 1) % 2 == 0) { 
	    
	    if ((pid = fork()) < 0) {
		/* error */
		printf("\nERROR: Unable to fork.\n");
		exit(-1);
	    } else if (pid == 0) {
		/* child */
		
		if (i == 0) {
		    bottom = 2;
		} else {
		    bottom = nums[i - 1] + 1;
		}
		top = nums[i];
		
		printf("child %d using pipe: bottom=%d, top=%d\n", (int)getpid(), bottom, top); 
		find_primes(bottom, top);
		exit(0);
		
	    } else {
		/* parent */
		
		
		
	    }
	    
	    /* odd child case - use FIFO */
	} else {
	    
	    if ((pid = fork()) < 0) {
		/* error */
		printf("\nERROR: Unable to fork.\n");
		exit(-1);
	    } else if (pid == 0) {
		/* child */
		
		if (i == 0) {
		    bottom = 2;
		} else {
		    bottom = nums[i - 1] + 1;
		}
		top = nums[i];
		
		printf("child %d using FIFO: bottom=%d, top=%d\n", (int)getpid(), bottom, top); 
		find_primes(bottom, top);
		exit(0);
		
	    } else {
		/* parent */
		
		
		
	    }
	    
	}
    }
    
    return 0;
}
