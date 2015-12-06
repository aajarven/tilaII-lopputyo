testi : avlpuu.o avlpuu.mod testi.o
	gfortran -o bin/testi testi.f90 avlpuu.o

avlpuu.o : avlpuu.f90
	gfortran -c avlpuu.f90

avlpuu.mod : avlpuu.f90
	@true

testi.o : testi.f90
	gfortran -c testi.f90

clean:
	/bin/rm *.o avlpuu.mod
