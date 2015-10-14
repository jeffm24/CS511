#Makefile for transfer3
NAME = transfer3
TARBALL = jmariconda_$(NAME).tar.gz
SRC = transfer3.c cbuf.c
OBJ = $(SRC:.c=.o)
RM = rm -fv

all:
	gcc -Wall -std=c99 -pedantic-errors $(SRC) -o $(NAME) -lpthread

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
