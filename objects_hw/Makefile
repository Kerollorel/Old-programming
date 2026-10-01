FC = gfortran
FFLAGS = -std=f2008 -O2 -Wall

SRC = kinds.f90 vectors.f90 shapes.f90 polygons.f90 solids.f90 main.f90
TARGET = homework

all: $(TARGET)

$(TARGET): $(SRC)
	$(FC) $(FFLAGS) $(SRC) -o $(TARGET)

clean:
	rm -f $(TARGET) *.o *.mod out.txt
