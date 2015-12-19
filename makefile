sanalaskuri: tiedostonluku.o tiedostonluku.mod  avlpuu.o avlpuu.mod sanalaskuri.o
	gfortran -o bin/sanalaskuri src/sanalaskuri.f90 tiedostonluku.o avlpuu.o

avlpuu.o : src/avlpuu.f90
	gfortran -c src/avlpuu.f90

avlpuu.mod : src/avlpuu.f90
	@true

tiedostonluku.o : src/tiedostonluku.f90
	gfortran -c src/tiedostonluku.f90

tiedostonluku.mod : src/tiedostonluku.f90
	@true

sanalaskuri.o : src/sanalaskuri.f90
	gfortran -c src/sanalaskuri.f90

clean:
	/bin/rm *.o tiedostonluku.mod avlpuu.mod

