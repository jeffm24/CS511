#Makefile for primes
NAME = primes
SRC = primes.c
OBJ = $(SRC:.c=.o)
RM = rm -fv

all:
	cc -g -I../../include $(SRC) -o $(NAME)

clean:
	-$(RM) *~
	-$(RM) \#*
	-$(RM) *.o
	-$(RM) *.core

fclean: clean
	-$(RM) $(NAME)

re: fclean all
