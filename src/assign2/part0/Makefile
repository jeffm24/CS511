#Makefile for rw
NAME = rw
TARBALL = jmariconda_$(NAME).tar.gz
SRC = rw.c
OBJ = $(SRC:.c=.o)
RM = rm -fv

all:
	gcc -Wall -std=c99 -pedantic-errors $(SRC) -o $(NAME)

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
