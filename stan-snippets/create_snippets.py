#!/usr/bin/env python3
""" Create yasnippets for Stan functions and distributions from stan_lang.json

Yasnippets for other parts of the language are created manually.

"""
import json
import os
import re
import shutil
import sys
import subprocess as sp
from os import path

EXCLUDED_FUNCTIONS = ["increment_log_prob"]

FUNC_TEMPLATE = """# name: {funcname}
# key: {funcname}
# group: Functions
# --
{funcname}({args})$0"""

DIST_TEMPLATE = """# name: {funcname}
# key: {funcname}
# group: Distributions
# --
{funcname}({args})$0"""


def dir_create_or_clean(dst):
    if path.exists(dst):
        shutil.rmtree(dst)
    os.makedirs(dst)

def get_unique_function_args(data):
    excluded = EXCLUDED_FUNCTIONS + ['operator' + x for x in data['operators']]
    functions = [f for f in data['functions'] if f not in excluded]
    funcargs = set()
    for f in functions:
        for sig in data['functions'][f]:
            funcargs.add((f, tuple(data['functions'][f][sig]['argnames'])))
    return funcargs

def format_args_for_yasnippet(x):
    def clean_arg(x):
        return re.sub("\[.*?\]", "", x)
    if (len(x)):
        return ', '.join('${%d:%s}' % (i + 1, clean_arg(j)) for i, j in enumerate(x))
    else:
        return ''

def create_function_snippet(x):
    """ Write a single function to a filename """
    return FUNC_TEMPLATE.format(funcname = x[0], 
                                args = format_args_for_yasnippet(x[1]))

def write_all_function_snippets(functions, dst):
    for fxn in functions:
        filename = os.path.join(dst, '%s(%s).yasnippet' % (fxn[0], ','.join(fxn[1])))
        with open(filename, 'w') as f:
            f.write(create_function_snippet(fxn))

def get_distribution_name(funcname):
    return re.sub('_log$', '', funcname)

def create_distribution_snippet(x):
    """ Write a single function to a filename """
    return DIST_TEMPLATE.format(funcname = x[0], 
                                args = format_args_for_yasnippet(x[1]))

def write_all_distribution_snippets(functions, dst):
    for fxn in functions:
        filename = os.path.join(dst, '%s(%s).yasnippet' % (fxn[0], ','.join(fxn[1])))
        with open(filename, 'w') as f:
            f.write(create_distribution_snippet(fxn))

def main(src, dst):
    with open(src, 'r') as f:
        data = json.load(f)
    functions = get_unique_function_args(data)
    distributions = set()
    for fxn in functions:
        if get_distribution_name(fxn[0]) in data['distributions']:
            distributions.add((get_distribution_name(fxn[0]), fxn[1][1:]))
    dst_subdir = os.path.join(dst, 'stan-mode', 'functions')
    dir_create_or_clean(dst_subdir)
    write_all_function_snippets(functions, dst_subdir)
    write_all_distribution_snippets(distributions, dst_subdir)
        
if __name__ == '__main__':
    main(*sys.argv[1:3])
    pass
    # function_dir = path.join(dst, 'stan-mode', 'functions')
    # dist_dir = path.join(dst, 'stan-mode', 'distributions')

