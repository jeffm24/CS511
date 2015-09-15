#Makefile for notsominishell
NAME = notsominishell
SRC = my_notsominishell.c init_caps.c init_terminal.c archive_buff.c check.c getout.c clear.c \
	term_get_cap.c restore_terminal.c my_termprint.c refresh_buff.c culeft.c curight.c \
	up.c down.c backspace.c delete.c move_start.c move_end.c cut_eol.c cut_prev.c \
	paste.c
OBJ = $(SRC:.c=.o)
RM = rm -fv

all:
	cc -g -L../../lib -I../../include $(SRC) -ltermcap -lmylist -lmy -o $(NAME)

clean:
	-$(RM) *~
	-$(RM) \#*
	-$(RM) *.o
	-$(RM) *.core

fclean: clean
	-$(RM) $(NAME)

re: fclean all
