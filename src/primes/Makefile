#Makefile for primes
NAME = primes
SRC = primes.c
OBJ = $(SRC:.c=.o)
RM = rm -fv

all:
	gcc -I../../include -Wall -pedantic-errors $(SRC) -o $(NAME)

clean:
	-$(RM) *~
	-$(RM) \#*
	-$(RM) *.o
	-$(RM) *.core

fclean: clean
	-$(RM) $(NAME)

re: fclean all
