#ifndef NULL
#define NULL ((void*)0)
#endif

#define _GNU_SOURCE
#include <stdlib.h>
#include <stdio.h>
#include <errno.h>
#include <string.h>

/*
  Author: Jeff Mariconda
  Class: CS-511

  Program that reads from an input file into a buffer and then writes from that
  buffer into an output file.
*/

int main(int argc, char **argv)
{
    char *in_name, *out_name;                       /* names of the input and output files */
    FILE *in_fp, *out_fp;                           /* file pointers to input/output files */
    size_t in_len = 32 * sizeof(char);              /* length of the input buffer */
    char *buff = (char*)malloc(in_len);             /* buffer for storing lines from file */
    ssize_t read;

    /* check if corrent number of command line args given */
    if (argc < 3) {
        printf("\nERROR: Invalid arguments. Use: rw [input file name] [output file name]\n");
        exit(-1);
    }

    in_name = argv[1];
    out_name = argv[2];

    /* try to open input file for reading */
    if ((in_fp = fopen(in_name, "r")) == NULL) {
        perror("fopen input");
        exit(-1);
    }

    /* try to open output file for writing */
    if ((out_fp = fopen(out_name, "w")) == NULL) {
        perror("fopen output");
        exit(-1);
    }

    /* read lines from input file into buffer and write them to output file */
    while ((read = getline(&buff, &in_len, in_fp)) != -1) {
        if (fwrite(buff, strlen(buff), 1, out_fp) < 0) {
            perror("fwrite");
            exit(-1);
        }
    }

    /* check if error was thrown from getline */
    if (errno > 0 && read == -1) {
        perror("getline");
        exit(-1);
    }

    free(buff);
    fclose(in_fp);
    fclose(out_fp);

    return 0;
}
