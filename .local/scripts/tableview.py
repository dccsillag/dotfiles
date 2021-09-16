#!/usr/bin/python3
import sys
import os
import math
from argparse import ArgumentParser

import numpy as np
import pandas as pd


parser = ArgumentParser()
parser.add_argument('files',
                    nargs='+',
                    help="Files to view")
parser.add_argument('-nh', '--no-header',
                    action='store_true',
                    help="No header in CSV/TSV files")
parser.add_argument('-c', '--column',
                    type=str,
                    help="Name of the column to show")
parser.add_argument('-t', '--transform',
                    type=str,
                    nargs='*',
                    default=[],
                    help="Transformations to apply to the selected column")
parser.add_argument('-H', '--histogram',
                    action='store_true',
                    help="Show a histogram of the selected column")
parser.add_argument('-C', '--list-columns',
                    action='store_true',
                    help="List available columns")
parser.add_argument('-I', '--info',
                    action='store_true',
                    help="Show basic info about table")
args = parser.parse_args()


def throw_error(message):
    print(message, file=sys.stderr)
    sys.exit(1)


def print_histogram(series):
    minval, maxval = series.min(), series.max()
    n_bins = min(80, len(series.unique()))
    height = 20
    output_matrix = np.zeros((height, n_bins))
    bins = []
    for bin_start, bin_end in zip(np.linspace(minval, maxval, n_bins+1)[:-1],
                                  np.linspace(minval, maxval, n_bins+1)[1:]):
        bins.append(series[(bin_start <= series) & (series < bin_end)])
    max_bin_size = max(map(len, bins))
    for i, bin in enumerate(bins):
        if len(bin) == 0:
            output_matrix[-1, i] = -1
            continue
        output_matrix[-math.ceil(np.interp(len(bin),
                                           (0, max_bin_size),
                                           (0, height))):,
                      i] = 1

    char_matrix = np.where(output_matrix == 1,
                           '█',
                           np.where(output_matrix == -1,
                                    '‗',
                                    ' '))
    print('\n'.join([''.join(x) for x in char_matrix]))


for file in args.files:
    if not os.path.exists(file):
        throw_error(f"no such file: {file}")

    if len(args.files) > 1:
        print(f" -*- {file} -*- ")

    extension = os.path.basename(file).split('.')[-1]
    read_func = {
        'csv':    pd.read_csv,
        'tsv':    (lambda path: pd.read_csv(path, sep='\t')),
        'hdf':    pd.read_hdf,
        'pkl':    pd.read_pickle,
        'pickle': pd.read_pickle,
        'xlsx':   pd.read_excel,
    }.get(extension)
    if read_func is None:
        throw_error("Unknown file extension: '%s'" % extension)

    df = read_func(file)

    if args.list_columns:
        print('\n'.join(list(df.columns)))
    elif args.info:
        df.info()
    elif args.column is not None:
        if args.column not in df.columns:
            throw_error(f"no such column: {args.column}")

        series = df[args.column]
        for transform in args.transform:
            func = {
                'log': (lambda s: np.log(s[s > 0])),
                'exp': np.exp,
                'unique': np.unique,
            }.get(transform)
            if func is None:
                throw_error("No such transform: '%s'" % transform)
            series = func(series)

        if args.histogram:
            print_histogram(series)
        else:
            print(series)
    else:
        print(df)
