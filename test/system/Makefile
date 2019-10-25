include ../../config

CC = g++
CFLAGS = -Ifadbad++
INCLUDES += -I ../../src/ fadbad.cma unix.cma

TARGET = fad bad

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

# dependencies
mldeps = common.cmo brusselator.cmo

fad.cmo bad.cmo: $(mldeps)
fad.byte bad.byte: INCLUDES += $(mldeps)

# implicit rules

.cpp.o:
	$(CC) $(CFLAGS) -c $<

%_cpp:%.o
	$(CC) -o $@ $<

%_ml:%.byte
	mv $< $@
