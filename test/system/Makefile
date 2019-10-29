include ../../config

CC = g++
CFLAGS = -Ifadbad++
INCLUDES += -I ../../src/ fadbad.cma unix.cma

TARGET = fad bad tad

all: cpp ml

cpp: $(TARGET:=_cpp)
ml: $(TARGET:=_ml)

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

$(TARGET:%=compare_%): cpp ml
	./compare.py -prog $(@:compare_%=%) -n 100

compare_fad_bad: fad_cpp fad_ml bad_cpp bad_ml
	./compare.py -prog fad_bad -n 100

test: compare_fad_bad compare_tad

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
