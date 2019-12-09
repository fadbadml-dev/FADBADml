#!/usr/bin/env python3

from copy import deepcopy

class Particle:

    @staticmethod
    def fromPrevious(previous, new_id):
        self = Particle(new_id)
        self.values = deepcopy(previous.values)
        return self

    def __init__(self, id):
        self.id = id
        self.values = {}

    def getValue(self, name):
        return self.values[name]

    def getNames(self):
        return self.values.keys()

    def addValue(self, name, value):
        if name not in self.values.keys():
            self.values[name] = []
        self.values[name].append(value)

    def addValues(self, dic):
        for key,value in dic.items():
            if key != "id" and key != "lastid":
                self.addValue(key, value)

# MAIN

import argparse

parser = argparse.ArgumentParser()
parser.add_argument("-f", metavar="PATH", dest="logfile",
                          default="", help="path to the log file")
args = parser.parse_args()

def get_name(line): return line.split(':')[0].strip()
def get_value(line): return line.split(':')[1].strip()

def file_to_gen(f):
    for line in f:
        yield line.strip()

from matplotlib import pyplot as plt
from time import sleep

def draw_particles(particles, todraw, savepath):
    plt.close
    plt.cla()
    plt.clf()

    for id in todraw:
        particle = particles[id]
        time = particle.getValue("t")

        names = [name for name in particle.getNames() if name != "t"]
        n_plots = len(names)

        for i in range(n_plots):
            plt.subplot(n_plots, 1, i+1)
            plt.plot(time, particle.getValue(names[i]))
            plt.title(names[i])

    # plt.show()
    # print("Showed figure %s" % savepath)

    path = 'figures/%s.png' % savepath
    plt.savefig(path, dpi=150)
    print("Saved figure %s" % path)

# From file (-f option)
def open_file():
    return open(args.logfile, "r")
def read_file(f):
    return file_to_gen(f)
def read_line_file(file):
    return next(file)
def close_file(file):
    file.close()

import sys
# From stdin
def open_stdin(): pass
def read_stdin(arg): pass
def read_line_stdin(arg):
    return sys.stdin.readline().strip()
def close_stdin(arg): pass

if __name__ == "__main__":
    if args.logfile == "":
        print("Got empty path, using stdin")
        open_input = open_stdin
        read_input = read_stdin
        read_line_input = read_line_stdin
        close_input = close_stdin
    else:
        print("using %s" % args.logfile)
        open_input = open_file
        read_input = read_file
        read_line_input = read_line_file
        close_input = close_file

    file = open_input()
    lines = read_input(file)

    n_particles = 0
    particles = {}
    new_particles = []

    fig_count = 0

    try:
        while True:
            line = read_line_input(lines)
            if line == "" or line[0] == "#":
                pass # ignore empty lines or lines starting with #
            elif get_name(line) == "n":
                n_particles = int(get_value(line))
                print("n =", n_particles)

            elif line == "{":
                new_input = {}
                line = read_line_input(lines)
                while line != "}":
                    name = get_name(line)
                    if name == "id" or name == "lastid":
                        new_input[name] = int(get_value(line))
                    else:
                        new_input[name] = float(get_value(line))
                    line = read_line_input(lines)

                # print("New input:", new_input)

                if new_input["id"] not in particles.keys():
                    if new_input["lastid"] == 0:
                        particles[new_input["id"]] = Particle(new_input["id"])
                    else:
                        particles[new_input["id"]] = Particle.fromPrevious(particles[new_input["lastid"]], new_input["id"])
                    new_particles.append(new_input["id"])

                particles[new_input["id"]].addValues(new_input)

            elif line == "Resample":
                img = draw_particles(particles, new_particles, "fig_%d" % fig_count)
                new_particles = []
                fig_count += 1
            else:
                print("Ignoring line: %s" % line)

    except StopIteration:
        print("Reached EOF")

    close_input(file)
