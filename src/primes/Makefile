#Makefile for primes
NAME = primes
TARBALL = jmariconda_$(NAME).tar.gz
SRC = primes.c
INC = ../../include
OBJ = $(SRC:.c=.o)
RM = rm -fv

all:
	gcc -ggdb -I$(INC) -Wall -std=c99 -pedantic-errors $(SRC) -o $(NAME)

clean:
	-$(RM) *~
	-$(RM) \#*
	-$(RM) *.o
	-$(RM) *.core

fclean: clean
	-$(RM) $(NAME)

re: fclean all

submit: fclean
	tar cvzf $(TARBALL) *
