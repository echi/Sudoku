CC = gcc
CCFLAGS = -arch x86_64 -fno-common -g -O2 -c
CLFLAGS = -arch x86_64

all: sudoku

sudoku: sudoku.o backtrack.o
	$(CC) $(CLFLAGS) sudoku.o backtrack.o -o sudoku

backtrack.o: backtrack.c
	$(CC) $(CCFLAGS) backtrack.c

sudoku.o: sudoku.c
	$(CC) $(CCFLAGS) sudoku.c

clean:
	rm -rf *~ *.o sudoku