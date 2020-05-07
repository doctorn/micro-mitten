#!/usr/bin/python3

import argparse
import math
import os

import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import scipy.stats as st


def plot_iteration_time(data, ax, color='blue', legend=False, linestyle='-'):
    stats = (
        data.groupby(['GC Strategy', 'Problem Size'])[
            'Iteration (microseconds)']
        .agg(['mean', 'count', 'std'])
    )

    ci95 = []

    for i in stats.index:
        m, c, s = stats.loc[i]
        ci95.append(st.t.ppf(0.95, c - 1) * s / math.sqrt(c))

    stats['ci95'] = ci95

    stats = stats.unstack(level=0)
    stats.plot(y='mean', yerr='ci95', legend=legend,
               ax=ax, linestyle=linestyle, color=color)


def plot_absolute_performance(data):
    benchmarks = data['Benchmark'].nunique()

    plt.rcdefaults()
    fig, ax = plt.subplots(1, benchmarks)

    for (i, benchmark) in zip(range(benchmarks), data['Benchmark'].unique()):
        temp = data[data['Benchmark'] == benchmark].copy()

        temp['GC Strategy'] = temp['GC Strategy'] + \
            temp['Optimised'].map(lambda x: '' if x else ' (unoptimised)')
        temp.drop(columns='Optimised')

        plot_iteration_time(
            temp[temp['GC Strategy'] == 'BDW'], ax[i], legend=i == 0)
        plot_iteration_time(
            temp[temp['GC Strategy'] == 'None'], ax[i], color='red', legend=i == 0)
        plot_iteration_time(
            temp[temp['GC Strategy'] == 'Proust'], ax[i], color='green', legend=i == 0)
        plot_iteration_time(
            temp[temp['GC Strategy'] == 'BDW (unoptimised)'], ax[i], linestyle='--', legend=i == 0)
        plot_iteration_time(
            temp[temp['GC Strategy'] == 'None (unoptimised)'], ax[i], linestyle='--', color='red', legend=i == 0)
        plot_iteration_time(
            temp[temp['GC Strategy'] == 'Proust (unoptimised)'], ax[i], linestyle='--', color='green', legend=i == 0)

        ax[i].set_title(benchmark)
        ax[i].set_ylabel("Time/μs")

    plt.show()


def plot_compile_time(data):
    plt.rcdefaults()
    fig, ax = plt.subplots()

    stats = (
        data.groupby(['Benchmark', 'GC Strategy'])[
            'Compile Time (microseconds)']
        .agg(['mean', 'count', 'std'])
    )

    ci95 = []

    for i in stats.index:
        m, c, s = stats.loc[i]
        ci95.append(st.t.ppf(0.95, c) * s / math.sqrt(c))

    stats['ci95'] = ci95

    stats.unstack(level=1).plot.bar(y='mean', legend=True, yerr='ci95', ax=ax)

    plt.xticks(rotation=0)
    plt.ylabel("Time/μs")
    plt.legend(loc='upper left')

    plt.show()


def plot_binary_size(data):
    plt.rcdefaults()
    fig, ax = plt.subplots()

    stats = (
        data.groupby(['Benchmark', 'GC Strategy'])['Binary Size (bytes)']
        .agg(['mean'])
    )

    stats.unstack(level=1).plot.bar(y='mean', legend=True, ax=ax)

    plt.xticks(rotation=0)
    plt.ylabel("Size/B")
    plt.legend(loc='upper left')

    plt.show()


def plot_data_cache_miss_rate(data):
    benchmarks = data['Benchmark'].nunique()

    plt.rcdefaults()
    fig, ax = plt.subplots(1, benchmarks)

    for (i, benchmark) in zip(range(benchmarks), data['Benchmark'].unique()):
        temp = data[data['Benchmark'] == benchmark].copy()

        temp[temp['GC Strategy'] == 'BDW'].plot(
            ax=ax[i], x='Problem Size', y='Data Cache Miss Rate', legend=i == 0, label='BDW')
        temp[temp['GC Strategy'] == 'None'].plot(
            ax=ax[i], x='Problem Size', y='Data Cache Miss Rate', color='red', legend=i == 0, label='None')
        temp[temp['GC Strategy'] == 'Proust'].plot(
            ax=ax[i], x='Problem Size', y='Data Cache Miss Rate', color='green', legend=i == 0, label='Proust')

        ax[i].set_title(benchmark)
        ax[i].set_ylabel("Data-Cache Miss Rate/%")

    plt.show()


def plot_data_cache_misses(data):
    benchmarks = data['Benchmark'].nunique()

    plt.rcdefaults()
    fig, ax = plt.subplots(1, benchmarks)

    data['Data Cache Misses'] = data['D1 Missed Reads'] + \
        data['DL Missed Reads'] + data['D1 Missed Writes'] + \
        data['DL Missed Writes']

    for (i, benchmark) in zip(range(benchmarks), data['Benchmark'].unique()):
        temp = data[data['Benchmark'] == benchmark].copy()

        temp[temp['GC Strategy'] == 'BDW'].plot(
            ax=ax[i], x='Problem Size', y='Data Cache Misses', legend=i == 0, label='BDW')
        temp[temp['GC Strategy'] == 'None'].plot(
            ax=ax[i], x='Problem Size', y='Data Cache Misses', color='red', legend=i == 0, label='None')
        temp[temp['GC Strategy'] == 'Proust'].plot(
            ax=ax[i], x='Problem Size', y='Data Cache Misses', color='green', legend=i == 0, label='Proust')

        ax[i].set_title(benchmark)
        ax[i].set_ylabel("Data-Cache Misses")
        ax[i].ticklabel_format(axis='y', style='sci', scilimits=(0, 0))

    plt.show()


def plot_instruction_cache_miss_rate(data):
    benchmarks = data['Benchmark'].nunique()

    plt.rcdefaults()
    fig, ax = plt.subplots(1, benchmarks)

    for (i, benchmark) in zip(range(benchmarks), data['Benchmark'].unique()):
        temp = data[data['Benchmark'] == benchmark].copy()

        temp[temp['GC Strategy'] == 'BDW'].plot(
            ax=ax[i], x='Problem Size', y='Instruction Cache Miss Rate', legend=i == 0, label='BDW')
        temp[temp['GC Strategy'] == 'None'].plot(
            ax=ax[i], x='Problem Size', y='Instruction Cache Miss Rate', color='red', legend=i == 0, label='None')
        temp[temp['GC Strategy'] == 'Proust'].plot(
            ax=ax[i], x='Problem Size', y='Instruction Cache Miss Rate', color='green', legend=i == 0, label='Proust')

        ax[i].set_title(benchmark)
        ax[i].set_ylabel("Instruction-Cache Miss Rate/%")

    plt.show()


def plot_instruction_cache_misses(data):
    benchmarks = data['Benchmark'].nunique()

    plt.rcdefaults()
    fig, ax = plt.subplots(1, benchmarks)

    data['Instruction Cache Misses'] = data['I1 Missed Reads'] + \
        data['IL Missed Reads']

    for (i, benchmark) in zip(range(benchmarks), data['Benchmark'].unique()):
        temp = data[data['Benchmark'] == benchmark].copy()

        temp[temp['GC Strategy'] == 'BDW'].plot(
            ax=ax[i], x='Problem Size', y='Instruction Cache Misses', legend=i == 0, label='BDW')
        temp[temp['GC Strategy'] == 'None'].plot(
            ax=ax[i], x='Problem Size', y='Instruction Cache Misses', color='red', legend=i == 0, label='None')
        temp[temp['GC Strategy'] == 'Proust'].plot(
            ax=ax[i], x='Problem Size', y='Instruction Cache Misses', color='green', legend=i == 0, label='Proust')

        ax[i].set_title(benchmark)
        ax[i].set_ylabel("Instruction-Cache Misses")
        ax[i].ticklabel_format(axis='y', style='sci', scilimits=(0, 0))

    plt.show()


def plot_peak_heap_usage(data):
    benchmarks = data['Benchmark'].nunique()

    plt.rcdefaults()
    fig, ax = plt.subplots(1, benchmarks)

    for (i, benchmark) in zip(range(benchmarks), data['Benchmark'].unique()):
        temp = data[data['Benchmark'] == benchmark].copy()

        temp[temp['GC Strategy'] == 'BDW'].plot(
            ax=ax[i], x='Problem Size', y='Maximum Heap Size', legend=i == 0, label='BDW')
        temp[temp['GC Strategy'] == 'None'].plot(
            ax=ax[i], x='Problem Size', y='Maximum Heap Size', color='red', legend=i == 0, label='None')
        temp[temp['GC Strategy'] == 'Proust'].plot(
            ax=ax[i], x='Problem Size', y='Maximum Heap Size', color='green', legend=i == 0, label='Proust')

        ax[i].set_title(benchmark)
        ax[i].set_ylabel("Maximum Heap Size/B")
        ax[i].ticklabel_format(axis='y', style='sci', scilimits=(0, 0))

    plt.show()


parser = argparse.ArgumentParser(description='Data processing')
parser.add_argument('csv', nargs='+')
parser.add_argument('--hc', action='store_true')
args = parser.parse_args()

data = pd.concat([pd.read_csv(f) for f in args.csv])
data['Benchmark'] = data['Benchmark'].map(os.path.basename)

if args.hc:
    plot_data_cache_miss_rate(data.copy())
    plot_data_cache_misses(data.copy())
    plot_instruction_cache_miss_rate(data.copy())
    plot_instruction_cache_misses(data.copy())
    plot_peak_heap_usage(data.copy())
else:
    plot_compile_time(data.copy())
    plot_binary_size(data.copy())
    plot_absolute_performance(data.copy())
