## Test AD on synchronous programs

- As I expected, we can differentiate the result of a program across different steps with F (or B or T).

Example : (run `make` then `./main.byte`)
with `node f x = let init x0 = x in let rec y = 2. -> (pre y * x0)`, we have :
```
step: 0
x0 = 5.
y_0 = 2.
dy_0/dx0 = 0.

step: 1
x0 = 5.
y_1 = 10.
dy_1/dx0 = 2.

step: 2
x0 = 5.
y_2 = 50.
dy_2/dx0 = 20.

step: 3
x0 = 5.
y_3 = 250.
dy_3/dx0 = 150.

step: 4
x0 = 5.
y_4 = 1250.
dy_4/dx0 = 1000.
```


- We cannot use AD with `if` statements, the heater's relay function is a good example to illustrate this :
```
let node relay (low, high, temp) = 
    let rec res = 
        if temp > high then true 
        else if temp < low then false 
        else pre res
    in res
```
There is no relationship between `low`, `high` and `res`. Afterwards, in the main function :
```
let thresh = ...
let node plant u = ... 

let node heater reference = temp where
    rec u = relay (reference - tresh, reference + tresh, temp)
    and temp = plant u
```
we have no relationship between `reference` and `temp`


- What about AD in automata ? I suspect that we will have the same issues then `if` statements with state transitions. Is it a problem ?