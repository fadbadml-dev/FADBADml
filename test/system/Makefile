include ../../config

CC = g++
CFLAGS = -Ifadbad++
INCLUDES += -I ../../src/ fadbad.cma unix.cma

TARGET = fad bad tad

all: $(TARGET:=_cpp) $(TARGET:=_ml)

%.cpp_out: %_cpp
	./$< > $@
%.ml_out: %_ml
	./$< > $@

clean:
	@rm -rf *.cm[iox] *.byte
cleanall: clean
	@rm -rf $(TARGET:=_ml) $(TARGET:=_cpp) $(TARGET:=.ml_out) $(TARGET:=.cpp_out)

.PRECIOUS : $(TARGET:_ml) $(TARGET:_cpp)

# comparisons

compare_fad_bad: fad_cpp fad_ml bad_cpp bad_ml
	./compare_fad_bad.py -n 100

# dependencies
mldeps = common.cmo brusselator.cmo

fad.cmo bad.cmo tad.cmo: $(mldeps)
fad.byte bad.byte tad.byte: INCLUDES += $(mldeps)

# implicit rules

.cpp.o:
	$(CC) $(CFLAGS) -c $<

%_cpp:%.o
	$(CC) -o $@ $<

%_ml:%.byte
	mv $< $@
