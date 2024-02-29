##
## EPITECH PROJECT, 2022
## Makefile
## File description:
## To compile all the functions
##

SRC	=	Main.hs

OBJ	=	$(SRC:.c=.o)

NAME	= 	main

all:	$(NAME)

$(NAME):	$(OBJ)
		ghc -o $(NAME) $(OBJ)

clean:
	rm -f src/*.o
	rm -f src/*.hi

fclean:	clean
	rm -f src/*.hi
	rm -f src/*.o
	rm -f $(NAME)

re:	fclean all

.PHONY: all clean fclean re