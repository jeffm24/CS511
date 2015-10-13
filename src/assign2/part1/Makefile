#Makefile for transfer1
NAME = transfer1
TARBALL = jmariconda_$(NAME).tar.gz
SRC = transfer1.c cbuf.c
OBJ = $(SRC:.c=.o)
RM = rm -fv

all:
	gcc -g -Wall -std=c99 -pedantic-errors $(SRC) -o $(NAME) -lpthread

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
