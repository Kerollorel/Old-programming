FC = gfortran
FFLAGS = -Wall -Wextra -g -fbounds-check -fimplicit-none

ALL_objs := types.o sortnumbers.o

.PHONY: all clean

all: main.o $(ALL_objs)
	$(FC) $(FFLAGS) -o myexecutable main.o $(ALL_objs)

types.o: types.f90
	$(FC) $(FFLAGS) -c types.f90

sortnumbers.o: types.o sortnumbers.f90
	$(FC) $(FFLAGS) -c sortnumbers.f90

main.o: $(ALL_objs) main.f90
	$(FC) $(FFLAGS) -c main.f90

clean:
	rm -rf *.o *.mod myexecutable
