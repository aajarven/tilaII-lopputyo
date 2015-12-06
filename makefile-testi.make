testi : avlpuu.o avlpuu.mod testi.o
	gfortran -o bin/testi src/testi.f90 avlpuu.o

avlpuu.o : src/avlpuu.f90
	gfortran -c src/avlpuu.f90

avlpuu.mod : src/avlpuu.f90
	@true

testi.o : src/testi.f90
	gfortran -c src/testi.f90

clean:
	/bin/rm *.o avlpuu.mod
