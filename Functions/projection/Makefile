FC = ifort
FCFLAGS = -arch x86_64 -fno-common -g -O2 -c
FLFLAGS = -arch x86_64 -dynamiclib -Wl,-headerpad_max_install_names -undefined dynamic_lookup -single_module -multiply_defined suppress -L/usr/local/lib -F/Library/Frameworks/R.framework/.. -framework R -Wl,-framework -Wl,CoreFoundation

all: sudoku_by_projection.so

sudoku_by_projection.so: sudoku_by_projection.o
	$(FC) $(FLFLAGS) -o sudoku_by_projection.so sudoku_by_projection.o


sudoku_by_projection.o: sudoku_by_projection.f90
	$(FC) $(FCFLAGS) sudoku_by_projection.f90 -o sudoku_by_projection.o

clean:
	rm -rf *~ *.o sudoku_by_projection.so