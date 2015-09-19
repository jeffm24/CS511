#include "primes.h"

/*
 * Find the primes in the given range and pass them to parent process to print.
 * start - bottom of range to search
 * end - top of range to search
 * fd - file descriptor to write primes to
 */
int find_primes(int start, int end, int fd) {
    int i, j, flag, count = 0;

    printf("child %d: bottom=%d, top=%d\n", (int)getpid(), start, end);

    if (end < 2) {
    	return 0;
    }

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

            /* write prime to parent */
            if (write(fd, &i, sizeof(int)) < 0) {
                printf("\nERROR: write from child %d failed\n", (int)getpid());
                exit(-1);
            }
    	    count++;
    	}
    }

    return count;
}

/*
 * Run multi-process prime finder program.
 */
int main(int argc, char **argv) {

    /* check if enough command line args have been supplied and exit on error if not */
    if (argc <= 1) {
    	printf("usage: ./primes <increasing positive integers>\n");
    	exit(-1);
    }

    int num_args = argc - 1;        /* number of arguements provided (number of numbers provided to check) */
    int nums[num_args];             /* array to hold the integer command line args */
    int fds[num_args][2];           /* file descriptor arrays for pipe */
    int read_fds[num_args];         /* file descriptors to read from */
    int max_fd;                     /* max fd to select */
    int bottom;                     /* bottom of range */
    int top;                        /* top of range */
    int child_count;                /* number of children for parent to keep track of */
    int i;
    pid_t pid;                      /* pid of the parent/child processes */
    pid_t pid_array[num_args];      /* array of child pids */
    char fifo_name[20];             /* buffer for creating the names for the FIFO's */

    /* get the integers that the user entered */
    for (i = 1 ; i < argc ; i++) {
        nums[i - 1] = atoi(argv[i]);
    }

    /* create pipes and FIFO's */
    for (i = 0 ; i < num_args ; i++) {
        if ((i + 1) % 2 != 0) {
            /* create pipes for odd children */

            if (pipe(fds[i]) < 0) {
                /* pipe error */
                perror("pipe");
                exit(-1);
            }

            /* load file descriptor to read from into read_fds */
            read_fds[i] = fds[i][0];

        } else {
            /* create FIFO's for even children */

            memset(fifo_name, '\0', 20 * sizeof(char));
            sprintf(fifo_name, "fifo%d", i);

            if (mkfifo(fifo_name, 0666) < 0) {
                /* fifo error */
                perror("mkfifo");
                exit(-1);
            }

            /* open fifo on parent end and load file descriptor to read from into read_fds */
            if ((read_fds[i] = open(fifo_name, O_RDONLY | O_NONBLOCK)) < 0) {
                perror("open");
                exit(-1);
            }
        }
    }

    /* create child processes and setup pipes/FIFO */
    for (i = 0, child_count = 0 ; i < num_args ; i++) {

    	/* odd child case - use pipe */
    	if ((i + 1) % 2 != 0) {

    	    if ((pid = fork()) < 0) {
        		/* fork error */
        		printf("\nERROR: Unable to fork.\n");
        		exit(-1);
    	    } else if (pid == 0) {
        		/* child */

                /* close read side of pipe for child */
                if (close(fds[i][0]) < 0) {
                    printf("\nERROR: child unable to close fd %d\n", fds[i][0]);
                    exit(-1);
                }

        		if (i == 0) {
        		    bottom = 2;
        		} else {
        		    bottom = nums[i - 1] + 1;
        		}
        		top = nums[i];

        		int num_primes = find_primes(bottom, top, fds[i][1]);

                /* printf("child %d: exiting with %d\n", (int)getpid(), num_primes); */

                /* exit with number of primes found */
        		exit(num_primes);

    	    } else {
                /* parent */
                pid_array[i] = pid;
                child_count++;

                /* close write side of pipe for parent */
                if (close(fds[i][1]) < 0) {
                    printf("\nERROR: parent unable to close fd %d\n", fds[i][1]);
                    exit(-1);
                }
    	    }

        /* even child case - use FIFO */
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

                memset(fifo_name, '\0', 20 * sizeof(char));
                sprintf(fifo_name, "fifo%d", i);

                /* open fifo on child end and get the fd to write to */
                int write_fd;
                if ((write_fd = open(fifo_name, O_APPEND | O_WRONLY | O_NONBLOCK)) < 0) {
                    perror("open");
                    exit(-1);
                }

        		int num_primes = find_primes(bottom, top, write_fd);

                /* printf("child %d: exiting with %d\n", (int)getpid(), num_primes); */

                /* exit with number of primes found */
        		exit(num_primes);

    	    } else {
        		/* parent */
                pid_array[i] = pid;
                child_count++;

    	    }

    	}

    }

    /* set max fd to read from */
    for (i = 0, max_fd = 0 ; i < num_args ; i++) {
        if (max_fd < read_fds[i])
            max_fd = read_fds[i];
    }

    /* setup select */
    fd_set read_set;
    struct timeval timeout;

    timeout.tv_sec = 5;
    timeout.tv_usec = 0;

    int ret;
    pid_t exited_child;
    int exit_status;

    while (child_count) {

        FD_ZERO(&read_set);
        for (i = 0 ; i < num_args ; i++) {
            /*
            if (read_fds[i] != -1) {

            }
            */
            FD_SET(read_fds[i], &read_set);
        }

        ret = select(max_fd + 1, &read_set, NULL, NULL, &timeout);

        /* no activity */
        if (ret == 0) {
            continue;
        }
        /* error */
        else if (ret < 0) {
            perror("select");
            exit(-1);
        }
        /* activity on some fds */
        else {
            /* check which fds were written to */
            for (i = 0 ; i < num_args; i++) {

                if (pid_array[i] == 0)
                    continue;

                /* printf("checking if child %d has exited\n", pid_array[i]); */

                /* check if child has exited if it hasn't already been confirmed exited */
                if ((exited_child = waitpid(pid_array[i], &exit_status, WNOHANG)) != 0) {
                    /* if it has exited, check for errors and, if none, print primes and get exit status */

                    if (exited_child < 0) {
                        perror("waitpid");
                        exit(-1);
                    } else {
                        int read_buff;
                        int n;

                        /* read all primes from exited_child and print to screen if there are any */
                        while (1) {
                            if ((n = read(read_fds[i], &read_buff, sizeof(int))) < 0) {
                                perror("read");
                                exit(-1);
                            } else if (n > 0) {
                                printf("%d is prime from %d\n", read_buff, pid_array[i]);
                            } else {
                                break;
                            }
                        }

                        /* remove fifo file if even child */
                        if ((i + 1) % 2 == 0) {
                            memset(fifo_name, '\0', 20 * sizeof(char));
                            sprintf(fifo_name, "fifo%d", i);

                            if (remove((const char*)fifo_name) < 0) {
                                perror("remove");
                                exit(-1);
                            }
                        }

                        /* decrement child count, "remove" child's associated file descriptor from the read_fds array, and print exit status */
                        child_count--;
                        /* read_fds[i] = -1; */
                        pid_array[i] = 0;
                        printf("child %d exited correctly and found %d primes\n", (int)exited_child, WEXITSTATUS(exit_status));
                    }

                }
            }
        }
    }

    return 0;
}
