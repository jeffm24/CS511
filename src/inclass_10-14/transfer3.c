#include "transfer3.h"

/*
 *  Reads input file and writes each line to the buffer until no lines are left
 */
void *fill_buffer(void *p) {
    FILE *in_fp;                /* file pointer to input file */
    char *temp_buff = NULL;     /* temporary buffer to read lines into before writing them into the shared buffer */
    size_t in_len = 0;          /* length of the input buffer */
    ssize_t read;               /* ssize_t struct for getting return of getline */
    int done = 0;               /* int to signal the thread to stop */
    int ret = 0;                /* return code of the thread (0 by default) */

    /* try to open input file for reading */
    if ((in_fp = fopen(env.in_name, "r")) == NULL) {
        perror("fopen input");
        ret = -1;
        pthread_exit(&ret);
    }

    while (1) {
        if (usleep(env.fill_sleep) < 0) {
            perror("usleep fill_thread");
            ret = -1;
            break;
        }

        /* only read next line if temp_buff is set to NULL */
        if (temp_buff == NULL) {
            /* read line from input file into temporary buffer */
            read = getline(&temp_buff, &in_len, in_fp);

            /* error checking */
            if (read == -1 && errno > 0) {
                perror("getline");
                ret = -1;
                break;
            } else if (read == -1 && errno == 0) {
                /* finished with input file */
                free(temp_buff);
                temp_buff = "QUIT";
                read = 4;
                done = 1;
            }
        }

        /* wait for mutex lock */
        if (pthread_mutex_lock(&env.lock) != 0) {
            perror("pthread_mutex_lock fill_thread");
            ret = -1;
            break;
        }

        /* wait on space_avail condition variable if there is no available space in shared buffer*/
        if (!cbuf_space_available()) {
            if (pthread_cond_wait(&env.space_avail, &env.lock) != 0) {
                perror("pthread_cond_wait fill_thread");
                ret = -1;
                break;
            }
        }
        /* --------------------------CRITICAL SECTION-------------------------- */

        /* check if there is enough free space in shared buffer */
        if (cbuf_space_available() >= read + 1) {
            /* load the string in from the temp_buff */
            cbuf_copy_in(temp_buff);
        } else {
            printf("fill thread: could not write [%s] -- not enough space (%d)\n", temp_buff, cbuf_space_available());
        }

        /* --------------------------CRITICAL SECTION-------------------------- */
        /* signal to thread waiting on data_avail condition variable */
        if (pthread_cond_signal(&env.data_avail) != 0) {
            perror("pthread_cond_signal fill_thread");
            ret = -1;
            break;
        }

        /* release mutex lock */
        if (pthread_mutex_unlock(&env.lock) != 0) {
            perror("pthread_mutex_unlock fill_thread");
            ret = -1;
            break;
        }

        printf("fill thread: wrote [%s] to buffer (nwritten=%d)\n", temp_buff, (int)(read + 1));

        /* if not done ready temp_buff to read another line from the input file (or break if finished reading) */
        if (!done) {
            free(temp_buff);
            temp_buff = NULL;
        } else {
            break;
        }
    }

    printf("fill thread: exiting\n");

    fclose(in_fp);
    pthread_exit(&ret);
}

/*
 *  Drains the buffer and writes each line to output file until "QUIT" is read
 */
void *drain_buffer(void *p) {
    FILE *out_fp;                                   /* file pointer to output file */
    size_t len = 32 * sizeof(char);                 /* length of temp_buff */
    size_t read_ret;                                /* number of bytes copied out of the shared buffer */
    char *temp_buff = (char*)malloc(len);           /* temp buffer for reading from shared buffer */
    int done = 0;                                   /* int to signal the thread to stop */
    int ret = 0;                                    /* return code of the thread * (0 by default) */

    /* try to open output file for writing */
    if ((out_fp = fopen(env.out_name, "w")) == NULL) {
        perror("fopen output");
        ret = -1;
        pthread_exit(&ret);
    }

    while (1) {
        if (usleep(env.drain_sleep) < 0) {
            perror("usleep drain_thread");
            ret = -1;
            break;
        }

        /* wait for mutex lock */
        if (pthread_mutex_lock(&env.lock) != 0) {
            perror("pthread_mutex_lock drain_thread");
            ret = -1;
            break;
        }

        /* wait on data_avail condition variable if there is no data available */
        if (!cbuf_data_is_available()) {
            if (pthread_cond_wait(&env.data_avail, &env.lock) != 0) {
                perror("pthread_cond_wait drain_thread");
                ret = -1;
                break;
            }
        }
        /* --------------------------CRITICAL SECTION-------------------------- */

        /* read string out of shared buffer */
        read_ret = cbuf_copy_out(temp_buff);

        printf("drain thread: read [%s] from buffer (nread=%d)\n", temp_buff, (int)read_ret);

        /* if drain thread reads "QUIT" break and exit */
        if (strcmp(temp_buff, "QUIT") == 0) {
            done = 1;
        } else {
            /* write from temp buffer to output file */
            if (fwrite(temp_buff, strlen(temp_buff), 1, out_fp) <= 0) {
                perror("fwrite");
                ret = -1;
                break;
            }
        }

        /* --------------------------CRITICAL SECTION-------------------------- */
        /* signal to thread waiting on space_avail condition variable */
        if (pthread_cond_signal(&env.space_avail) != 0) {
            perror("pthread_cond_signal drain_thread");
            ret = -1;
            break;
        }

        /* release mutex lock */
        if (pthread_mutex_unlock(&env.lock) != 0) {
            perror("pthread_mutex_unlock drain_thread");
            ret = -1;
            break;
        }

        if (done) {
            break;
        }
    }

    printf("drain thread: exiting\n");

    fclose(out_fp);
    pthread_exit(&ret);
}

/*
 *  Runs the transfer3 program.
 */
int main(int argc, char **argv)
{
    pthread_t fill_thread, drain_thread;

    /* check if corrent number of command line args given */
    if (argc < 5) {
        printf("\nERROR: Invalid arguments. Use: rw [input file name] [output file name]\n");
        exit(-1);
    }

    /* get command line args */
    env.in_name = argv[1];
    env.out_name = argv[2];
    env.fill_sleep = atol(argv[3]);
    env.drain_sleep = atol(argv[4]);

    printf("fill sleep: %lu\ndrain sleep: %lu\n", (long int)env.fill_sleep, (long int)env.drain_sleep);

    /* initialize shared buffer */
    cbuf_init();

    /* initialize mutex and condition vars */
    if (pthread_mutex_init(&env.lock, NULL) != 0) {
        perror("sem_init lock");
        exit(-1);
    }
    if (pthread_cond_init(&env.data_avail, NULL) != 0) {
        perror("sem_init data_avail");
        exit(-1);
    }
    if (pthread_cond_init(&env.space_avail, NULL) != 0) {
        perror("sem_init space_avail");
        exit(-1);
    }

    /* create threads */
    pthread_create(&fill_thread, NULL, fill_buffer, NULL);
    pthread_create(&drain_thread, NULL, drain_buffer, NULL);

    /* join threads */
    if (pthread_join(fill_thread, NULL) != 0) {
        perror("pthread_join fill_thread");
        exit(-1);
    }
    if (pthread_join(drain_thread, NULL) != 0) {
        perror("pthread_join drain_thread");
        exit(-1);
    }

    /* destroy mutex and condition vars once threads are finished */
    if (pthread_mutex_destroy(&env.lock) != 0) {
        perror("sem_destroy lock");
        exit(-1);
    }
    if (pthread_cond_destroy(&env.data_avail) != 0) {
        perror("sem_destroy data_avail");
        exit(-1);
    }
    if (pthread_cond_destroy(&env.space_avail) != 0) {
        perror("sem_destroy space_avail");
        exit(-1);
    }

    /* free cbuf */
    cbuf_terminate();

    return 0;
}
