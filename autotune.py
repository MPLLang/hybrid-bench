#!/usr/bin/env python3
#
# Run this while the current directory contains an already compiled main.mpl.bin.
#
# Usage:
#
# $ ../../autotune.py <mandelbrot|raytracer> --procs N
#
# Probably takes a long time to terminate (if ever), but prints a
# running log of improvements, so feel free to just kill it when it
# looks like it's not making further progress.

from __future__ import print_function

import sys
import re
import opentuner
from opentuner import ConfigurationManipulator
from opentuner import FloatParameter, IntegerParameter
from opentuner import MeasurementInterface
from opentuner import Result

def get_times(s):
    return list(map(float,re.findall('time ([0-9]+\.[0-9]+)s', s)))

def get_avgtime(s):
    ts = get_times(s)
    return sum(ts)/len(ts)

class HybridTuner(MeasurementInterface):
    def __init__(self, args, *pargs, **kwargs):
        super(HybridTuner, self).__init__(args, *pargs, **kwargs)

    def manipulator(self):
        """
        Define the search space by creating a
        ConfigurationManipulator
        """
        manipulator = ConfigurationManipulator()
        for p in self.params():
            manipulator.add_parameter(p)
        return manipulator

    def run(self, desired_result, input, limit):
        """
        Compile and run a given configuration then
        return performance
        """
        cfg = desired_result.configuration.data
        # We can print cfg here if we want.

        # Program to run
        cmd = './main.mpl.bin'
        # Basic scheduling
        cmd += f' @mpl procs {self.args.procs} -- -impl hybrid'
        # Measurement config
        cmd += ' -warmup 2 -repeat 10'
        # Workload - this should be modified for each benchmark.
        cmd += self.workload()
        for p in cfg:
            v = cfg[p]
            cmd += f' -{p} {v}'

        result = self.call_program(cmd)
        if result['returncode'] != 0:
            stdout=result['stdout'].decode('utf8')
            stderr=result['stderr'].decode('utf8')
            raise Exception(f'Command failed:\n{cmd}\nstdout:\n{stdout}\nstderr:{stderr}')

        return Result(time=get_avgtime(result['stdout'].decode('utf8')))

    def save_final_config(self, cfg):
        """called at the end of tuning"""
        for p in cfg:
            v = cfg[p]
            print(f' -{p} {v}')

# TODO: fix hardcoded workloads.

class RayTracerTuner(HybridTuner):
    def workload(self):
        return ' -h 4000 -w 4000'

    def params(self):
        return [FloatParameter('render-hybrid-gpu-split', 0, 1)]

class MandelbrotTuner(HybridTuner):
    def workload(self):
        return ''

    def params(self):
        return [FloatParameter('mandelbrot-parfor-split', 0, 1),
                IntegerParameter('mandelbrot-parfor-grain', 1, 100)]

problems = {
    'raytracer': RayTracerTuner,
    'mandelbrot': MandelbrotTuner
}

problem=sys.argv[1]
sys.argv = sys.argv[1:]

if problem not in problems:
    raise Exception(f'{problem} is not one of {problems.keys()}')

tuner = problems[problem]

if __name__ == '__main__':
    argparser = opentuner.default_argparser()
    argparser.add_argument('--procs', type=int, metavar='INT', default='64')
    tuner.main(argparser.parse_args())
