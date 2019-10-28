#!/usr/bin/env python3

from subprocess import check_output
from random import random, randint
from compare_json import eq_str_json_list
from multiprocessing import Pool, Array
from time import sleep, time

exe = ["fad_cpp", "fad_ml", "bad_cpp", "bad_ml"]
global_env = {}

def make_cmd(exe, *args, **kwargs):
    res = ["./%s" % exe]
    for arg in args:
        res += [arg]
    for k,v in kwargs.items():
        res += ["-%s" % k, str(v)]
    return res

def get_str_jsons(nsteps, dt):
    cmds = map(lambda exe: make_cmd(exe, n=nsteps, dt=dt), exe)
    outs = map(check_output, cmds)
    return map(lambda b: b.decode("utf8"), outs)

def compare_once(nsteps, dt):
    jsons = list(get_str_jsons(nsteps, dt))
    ok = eq_str_json_list(jsons, ["exec_time"])

    return jsons, ok

def compare_(id, n, verbose=False):
    starttime = time()
    padsize = len(str(n))
    for i in range(n):
        global_env["progress"][id] = i
        if verbose:
            cur_time = time() - starttime;
            print("testing: | %*d/%d | (approx time: %.2fs)" %\
                (padsize, i, n, cur_time), end="\r")
        nsteps = randint(0, 2)
        dt = random()

        jsons, ok = compare_once(nsteps, dt)

        if (not ok):
            if verbose:
                print()
                print("Failure with JSONs:")
                for js in jsons:
                    print("\t%s" % js)
            return jsons, False
    if verbose: print()
    return [], True

def compare(n): return compare_(0, n, verbose=True)

def compare_parallel(n, nprocess=4):
    quotient = int(n / nprocess)
    remainder = n % nprocess
    total_runs = [quotient]*nprocess
    for i in range(remainder):
        total_runs[i] += 1
    padsize = len(str(total_runs[0]))

    with Pool(nprocess) as p:
        args = [(i, total_runs[i]) for i in range(nprocess)]
        res = p.starmap_async(compare_, args)
        count = 0
        while True:
            res.wait(0)
            if (res.ready()):
                break
            else:
                cur_count = ""
                for i in range(nprocess):
                    cur_count += "%*d/%d | " %\
                        (padsize, global_env["progress"][i], args[i][1])
                print("testing: |", cur_count, "(approx time: %.2fs)" %\
                        (count / 10), end="\r")
            count += 1
            sleep(0.1)
        p.close()
        p.join()
    for (jsons, ok) in res.get():
        if (not ok):
            print()
            print("Failure with JSONs:")
            for js in jsons:
                print("\t%s" % js)
            return jsons, False
    print()
    return [], True



if __name__ == "__main__":
    import argparse

    parser = argparse.ArgumentParser(description="check that all the given \
    json structures are equal, except for ignored fields")
    parser.add_argument('-n', type=int, default=50,
                        help='number of tests to run')
    parser.add_argument('-j', type=int, default=8,
                        help='number of processes to run')
    args = parser.parse_args();

    global_env["progress"] = Array('i', args.j)

    if args.j == 1:
        _, ok = compare(args.n)
    else:
        _, ok = compare_parallel(args.n, nprocess=args.j)
    if (ok):
        print("OK")
        exit(0)
    else:
        print("NOT OK")
        exit(1)
