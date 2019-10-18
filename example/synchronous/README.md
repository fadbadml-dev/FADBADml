## Test AD on synchronous programs

- As I expected, we can differentiate the result of a program across different steps with F (or B or T).

    Example : (run `make` then `./main.byte`)
    with `node f x = let init x = x in let rec y = 2. -> (pre y * x0)`, we have :
    ```
    step: 0
    x = 5.
    y_0 = 2.
    dy_0/dx = 0.

    step: 1
    x = 5.
    y_1 = 10.
    dy_1/dx = 2.

    step: 2
    x = 5.
    y_2 = 50.
    dy_2/dx = 20.

    step: 3
    x = 5.
    y_3 = 250.
    dy_3/dx = 150.

    step: 4
    x = 5.
    y_4 = 1250.
    dy_4/dx = 1000.
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


- What about AD in automata ?
    ```
    let node aut x =
        automaton
        | I -> let rec t = 0. fby t +. 1. in do f x until (t > MyOp.get x) then F
        | F -> do f x done
    ```

    gives the result
    ```
    step: 0
    x = 5.
    y_0 = 2.
    dy_0/dx = 0.

    step: 1
    x = 5.
    y_1 = 10.
    dy_1/dx = 2.

    step: 2
    x = 5.
    y_2 = 50.
    dy_2/dx = 20.

    step: 3
    x = 5.
    y_3 = 250.
    dy_3/dx = 150.

    step: 4
    x = 5.
    y_4 = 1250.
    dy_4/dx = 1000.

    step: 5
    x = 5.
    y_5 = 6250.
    dy_5/dx = 6250.

    step: 6
    x = 5.
    y_6 = 2.
    dy_6/dx = 0.

    step: 7
    x = 5.
    y_7 = 10.
    dy_7/dx = 2.

    step: 8
    x = 5.
    y_8 = 50.
    dy_8/dx = 20.
    ```
