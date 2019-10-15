all: main.byte

myOp.zci:myOp.zli
	zeluc myOp.zli
myOp.zli:
	echo "type t val make : float -> t val ( * ) : t -> t -> t" > myOp.zli
myOp.cmo:myOp.ml
	ocamlc -c -o $@ -I ../../lib fadbad.cma myOp.ml
myOp.ml:
	echo "include Fadbad.F(Fadbad.OpFloat)" > myOp.ml

func.ml:myOp.zci
	zeluc func.zls
func.cmo:myOp.cmo func.ml
	ocamlc -c -o $@ -I `zeluc -where` -I ../../lib zllib.cma fadbad.cma myOp.cmo func.ml

main.byte:func.cmo main.ml
	ocamlc -o $@ -I `zeluc -where` -I ../../lib zllib.cma fadbad.cma myOp.cmo func.cmo main.ml

clean:
	rm -rf *.z[lc]i
	rm -rf *.cm[io] *.byte
	rm -rf myOp.ml func.ml