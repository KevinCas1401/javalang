CC = gcc
LEX = flex
YACC = bison
CFLAGS = -Wall -Wextra -O2
LDFLAGS = -lfl

all: parse gui

parse: parser.tab.c lex.yy.c parse_main.c
	$(CC) $(CFLAGS) -o $@ parser.tab.c lex.yy.c parse_main.c $(LDFLAGS)

lexdemo: parser.tab.c lex.yy.c lexdemo.c
	$(CC) $(CFLAGS) -o $@ parser.tab.c lex.yy.c lexdemo.c $(LDFLAGS)

gui: parser.tab.c lex.yy.c gui.c
	$(CC) $(CFLAGS) -o $@ parser.tab.c lex.yy.c gui.c $(LDFLAGS) `pkg-config --cflags --libs gtk+-3.0`

parser.tab.c parser.tab.h: parser.y
	$(YACC) -d -v parser.y

lex.yy.c: lexer.l parser.tab.h
	LC_ALL=C $(LEX) lexer.l

clean:
	rm -f parse lexdemo gui compi lex.yy.c parser.tab.c parser.tab.h parser.output
