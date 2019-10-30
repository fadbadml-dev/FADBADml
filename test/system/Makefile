include ../../config

CC = g++
CFLAGS = -Ifadbad++
INCLUDES += -I ../../src/ fadbad.cma unix.cma

TARGET = fad bad tad

all: cpp opt

cpp: $(TARGET:=cpp)
byte: $(TARGET:=mlbyte)
opt: $(TARGET:=mlopt)


clean:
	@rm -rf *.cm[iox] *.byte *.opt *.o
cleanall: clean
	@rm -rf $(TARGET:=ml) $(TARGET:=cpp)

.PRECIOUS : $(TARGET:ml) $(TARGET:cpp)

# comparisons

$(TARGET:%=compare_%): cpp ml
	./compare.py -prog $(@:compare_%=%) -n 100

compare_fad_bad: fadcpp fadml badcpp badml
	./compare.py -prog fad_bad -n 100

test: compare_fad_bad compare_tad

# dependencies
mldeps = common.cmo brusselator.cmo

$(TARGET:=ml.cmo): $(mldeps)
$(TARGET:=ml.cmx): $(mldeps:.cmo=.cmx)
$(TARGET:=ml.byte): INCLUDES += $(mldeps)
$(TARGET:=ml.opt): INCLUDES += $(mldeps:.cmo=.cmx)

# implicit rules

.cpp.o:
	$(CC) $(CFLAGS) -c $<

%cpp:%cpp.o
	$(CC) -o $@ $<

%mlbyte:%ml.byte
	mv $< $(@:byte=)
%mlopt:%ml.opt
	mv $< $(@:opt=)
