[Fadbad.B]: doc/Fadbad.B.html

## Backward automatic differentiation (BAD)

See [Fadbad.B]

In backward mode, FADBADml constructs a graph of the expressions while computing their values, then it propagates the derivatives in this graph starting from the roots (wheras FAD would have started from the leaves if it did construct the graph explicitly).
The type `B(Op).t` is the following record:

~~~ocaml
(* Op is the argument given to the functor B *)
(* op is a sum type listing all available operators *)
(* D.t represents an array of values of type Op.t, it is built
   using the functor Badiff.Derivatives *)
type t = {
  mutable operator : op;
  mutable operands : t array;
  mutable value : Op.t;
  derivatives : D.t;
  mutable rc : int; (* this field is used to avoid propagating the same derivatives twice *)
}
~~~

This time, a node stores its operator and operands (this is how the graph is stored in memory), the value of the sub-expression it represents and the derivatives of the root node wrt. the sub-expression it represents.
<a name="note1"></a>**Note** that in forward mode, the derivatives stored in a node are the derivatives of the sub-expression represented by said node wrt. the leaves (ie. the variables on which we called diff) of the graph, it's reversed compared to FAD !

For example, the function `f(x,y) = 2*x + cos(y)` would be implemented as

~~~ocaml
let f x y =
  let open B in
  (scale x 2.) + (cos y)
~~~

and it would be used like this:

~~~ocaml
...
let x = B.make 1. in
let y = B.make 2. in
let z = f x y in
...
~~~

and it would build a graph that looks like:

~~~ocaml
{ (* 2 * x + cos y aka. z*)
  op: PLUS;
  operands: [
    { (* scale x 2. *)
      op: SCALE 2.;
      operands: [{ (* x *)
                   op: CONST;
                   operands: [];
                   ...
                 }];
      ...
    };
    { (* cos y *)
      op: COS;
      operands: [{ (* y *)
                   op: CONST;
                   operands: [];
                   ...
                 }];
      ...
    }
  ];
  ...
}
~~~

At this point, the array of derivatives is empty for all nodes. In order to compute the derivative we need to tell the library what nodes we want to derivate:

~~~ocaml
...
diff z 0 1;
...
~~~

**Note** that we call `diff` on the root node in BAD whereas we were calling it on the leaves with FAD. This is due to what we discussed [in a previous note](#note1).
It behaves the same as in FAD: it builds the array of derivatives of z. In this example this array would simply be `[|&nbsp;1.&nbsp;|]`.
Then we can compute and retrieve the derivatives:

~~~ocaml
...
B.compute f;
let dfdx = B.d x 0; (* contains df/dx *)
let dfdy = B.d y 0; (* contains df/dy *)
...
~~~

There are several things going on here:
- first, note that the function `d` is called on the leaves in BAD whereas it was called on the root in FAD. This is due to what we discussed [in a previous note](#note1).
- second, there is no `compute` function in FADBAD++, we added this to solve what we think is a bug in the C++ implementation (refer to [this](not-done-yet)).
Countrary to what its name suggests, this function does not compute anything, it increments the reference counter (`rc` field) of the nodes in the subtree starting at `f` so that we know when to propagate during the actual computation (that is performed by `d`).

Find a similar code [here](https://github.com/fadbadml-dev/FADBADml/blob/master/example/exampleBAD.ml).
