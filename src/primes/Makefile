#Makefile for primes
NAME = primes
SRC = primes.c
OBJ = $(SRC:.c=.o)
RM = rm -fv

all:
	gcc -ggdb -I../../include -Wall -std=c99 -pedantic-errors $(SRC) -o $(NAME)

clean:
	-$(RM) *~
	-$(RM) \#*
	-$(RM) *.o
	-$(RM) *.core

fclean: clean
	-$(RM) $(NAME)

re: fclean all
