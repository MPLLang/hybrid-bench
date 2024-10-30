#!/usr/bin/env python3
#
# Run this while the current directory contains an already compiled main.mpl.bin.
#
# Usage:
#
# $ ../../autotune.py <mandelbrot|raytracer|...> --procs=N
#
# Useful options:
#
# --stop-after=SECONDS
#
#   Stop tuning after this many seconds. You probably want to use
#   this, as opentuner is not great at detecting convergence.

from __future__ import print_function

import sys
import re
import opentuner
import logging
from opentuner import ConfigurationManipulator
from opentuner.search.manipulator import FloatParameter, IntegerParameter, PowerOfTwoParameter
from opentuner import MeasurementInterface
from opentuner import Result

def get_times(s):
    return list(map(float,re.findall('time ([0-9]+\.[0-9]+)s', s)))

def get_avgtime(s):
    ts = get_times(s)
    return sum(ts)/len(ts)

def get_medtime(s):
    ts = get_times(s)
    return sorted(ts)[len(ts) // 2]

class HybridTuner(MeasurementInterface):
    def __init__(self, args, *pargs, **kwargs):
        super(HybridTuner, self).__init__(args, *pargs, **kwargs)
        self.log = logging.getLogger(__name__)

    def add_arguments(argparser):
        pass

    def manipulator(self):
        """
        Define the search space by creating a
        ConfigurationManipulator
        """
        manipulator = ConfigurationManipulator()
        for p in self.params():
            manipulator.add_parameter(p)
        return manipulator

    def gen_run_cmd(self, cfg):
        # Program to run
        cmd = './main.mpl.bin'
        # Basic scheduling
        cmd += f' @mpl procs {self.args.procs} -- -impl hybrid --quiet'
        if self.args.devices:
            cmd += f' -devices "{self.args.devices}"'
        # Measurement config
        cmd += ' -warmup 2 -repeat 10'
        # Workload - this should be modified for each benchmark.
        cmd += ' ' + self.workload()
        for p in cfg:
            v = cfg[p]
            cmd += f' -{p} {v}'
        return cmd

    def run(self, desired_result, input, limit):
        """
        Compile and run a given configuration then
        return performance
        """
        data = desired_result.configuration.data
        # We can print data here if we want.

        cmd = self.gen_run_cmd(data)

        if self.args.average_by == "median":
          parser = get_medtime
        elif self.args.average_by == "mean":
          parser = get_avgtime
        else:
          raise Exception(f"unknown average_by: {self.args.average_by}")

        result = self.call_program(cmd)
        if result['returncode'] != 0:
            stdout=result['stdout'].decode('utf8')
            stderr=result['stderr'].decode('utf8')
            self.log.info(f'Command failed:\n{cmd}\nstdout:\n{stdout}\nstderr:{stderr}')
            return Result(state='ERROR', time=float('inf'))
        else:
            return Result(time=parser(result['stdout'].decode('utf8')))

    def save_final_config(self, cfg):
        """called at the end of tuning"""
        for p in sorted(cfg.data.keys()):
            v = cfg.data[p]
            print(f'-{p} {v}')

        print('To reproduce:')
        print(self.gen_run_cmd(cfg.data))

# TODO: fix hardcoded workloads.

class RayTracerTuner(HybridTuner):
    def workload(self):
        return '-h 4000 -w 4000'

    def params(self):
        return [FloatParameter('raytracer-outer-split', 0, 1),
                FloatParameter('raytracer-inner-split', 0, 1)]

class MandelbrotTuner(HybridTuner):
    def workload(self):
        return ''

    def params(self):
        return [FloatParameter('mandelbrot-outer-split', 0, 1),
                FloatParameter('mandelbrot-inner-split', 0, 1),
                IntegerParameter('mandelbrot-grain', 1, 100)]

class PrimesTuner(HybridTuner):
    def workload(self):
        n = 1000 * 1000 * 1000
        return f'-n {n}'

    def params(self):
        return [FloatParameter('primes-block-size-factor', 1, 64),
                FloatParameter('hybrid-gpu-split', 0, 1)]

class BfsTuner(HybridTuner):
    def workload(self):
        return 'rmat-10M-symm-bin'

    def params(self):
        return [FloatParameter('bfs-sparse-hybrid-threshold', 0.001, 2),
                FloatParameter('bfs-dense-hybrid-split', 0.001, 1),
                FloatParameter('bfs-sparse-hybrid-split', 0.001, 1)]

class KmeansTuner(HybridTuner):
    def workload(self):
        return f'-n {self.args.n} --gen-random-input -d {self.args.d} -k {self.args.k}'

    def params(self):
        return [IntegerParameter('hist-cpu-grain', 1, 1000),
                IntegerParameter('hist-gpu-grain', 1, 100000),
                FloatParameter('hist-gpu-split', 0.01, 1),
                FloatParameter('hist-outer-split', 0.01, 1)]

    def add_arguments(argparser):
        argparser.add_argument('-n', type=int, metavar='INT', default=100000)
        argparser.add_argument('-d', type=int, metavar='INT', default=512)
        argparser.add_argument('-k', type=int, metavar='INT', default=16)

class QuickhullTuner(HybridTuner):
    def params(self):
        return [IntegerParameter('quickhull-par-grain', 1, 10000),
                IntegerParameter('quickhull-hybrid-grain', 1, 10000),
                IntegerParameter('quickhull-reduce-grain', 1, 50000),
                FloatParameter('quickhull-reduce-inner-split', 0.001, 1),
                FloatParameter('quickhull-reduce-outer-split', 0.001, 1)]

    def add_arguments(argparser):
        argparser.add_argument('--points', type=str, metavar='FILE', required=True)

    def workload(self):
        return f'-points {self.args.points}'


class DmmTuner(HybridTuner):
    def params(self):
        return [IntegerParameter('dmm-leaf-size', 10, 1000),
                IntegerParameter('dmm-gpu-thresh', 100, 10000),
                FloatParameter('dmm-split', 0.1, 0.9)]

    def add_arguments(argparser):
        argparser.add_argument('--size', type=int, metavar='SIZE', required=True)

    def workload(self):
        return f'-n {self.args.size}'


class SparseMxvTuner(HybridTuner):
    def params(self):
        return [IntegerParameter('matcoo-nnz-grain', 1, 10000),
                IntegerParameter('matcoo-nnz-grain-hybrid', 1, 1000*1000),
                FloatParameter('matcoo-hybrid-split', 0.01, 1),
                IntegerParameter('matcoo-gpu-block-size', 1, 10*1000*1000),
                FloatParameter('matcoo-hybrid-gpu-work-rat', 0.001, 100)]

    def add_arguments(argparser):
        argparser.add_argument('--input', type=str, metavar='FILE', required=True)

    def workload(self):
        return f'-input {self.args.input}'

class MergesortTuner(HybridTuner):
    def params(self):
        return [IntegerParameter('qsort-grain', 1000, 10*1000*1000),
                PowerOfTwoParameter('gpu-merge-block', 1, 64),
                FloatParameter('sort-split', 0, 1),
                IntegerParameter('sort-grain', 1, 10*1000*1000),
                FloatParameter('merge-split', 0, 1),
                IntegerParameter('merge-grain', 1, 10*1000*1000)]

    def workload(self):
        return f'-n {self.args.n}'

    def add_arguments(argparser):
        argparser.add_argument('-n', type=int, metavar='INT', default=10000000)

problems = {
    'raytracer': RayTracerTuner,
    'mandelbrot': MandelbrotTuner,
    'primes': PrimesTuner,
    'bfs': BfsTuner,
    'kmeans': KmeansTuner,
    'quickhull': QuickhullTuner,
    'sparse-mxv': SparseMxvTuner,
    'mergesort': MergesortTuner,
    'dmm': DmmTuner
}

problem=sys.argv[1]
sys.argv = sys.argv[1:]

if problem not in problems:
    known = ', '.join(problems.keys())
    raise Exception(f'"{problem}" is not one of {known}.')

tuner = problems[problem]

if __name__ == '__main__':
    argparser = opentuner.default_argparser()
    tuner.add_arguments(argparser)
    argparser.add_argument('--procs', type=int, metavar='INT', default='64')
    argparser.add_argument('--devices', type=str, metavar='STR', default=None)
    argparser.add_argument('--average_by', type=str, metavar='[mean|median]', default='mean')
    tuner.main(argparser.parse_args())
