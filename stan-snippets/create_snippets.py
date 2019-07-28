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


def format_args_for_yasnippet(x, prob_fun, dist, start_i = 1):
    def clean(x):
        return re.sub(r'\[.*\]', '', x)
    if len(x) >= 2 and dist:
        return format_args_for_yasnippet(x[1:], False, False, 1)
    elif len(x) == 1 and dist:
        return ''
    elif len(x) >= 2 and prob_fun:
        arg1 = '${%d:%s}' % (1, clean(x[0]))
        return arg1 + ' | ' + format_args_for_yasnippet(x[1:], False, False, 2)
    elif len(x) > 0:
        return ', '.join('${%d:%s}' % (i + start_i, clean(j)) for i, j in enumerate(x))
    else:
        return ''

def get_unique_function_args(data):
    funcargs = set()
    for k, v in data['functions'].items():
        if v['operator'] or v['deprecated']:
            continue
        for sig in v['signatures']:
            args = tuple([x['name'] for x in sig['args']])
            funcargs.add((k, args))
    return funcargs

def create_function_snippet(x, dist = False):
    """ Create a template for a single function

    The argument x is an object with two elements.
    x[0] is a string of the function name.
    x[1] is a list of strings corresponding to argument names.

    When dist is True and x[0] is *_lpdf or *_lpmf,
    the distribution equivalent is returned.
    """
    funcname = x[0]
    if (dist and
        (bool(re.match(r'.*_lpdf$', funcname)) or
         bool(re.match(r'.*_lpmf$', funcname)))):
        funcname = re.sub(r'_lpdf|_lpmf$', '', funcname)
        args = format_args_for_yasnippet(x[1], False, True)
        return DIST_TEMPLATE.format(funcname = funcname,
                                    args = args)
    elif (bool(re.match(r'.*_lpdf$', funcname)) or
          bool(re.match(r'.*_lpmf$', funcname)) or
          bool(re.match(r'.*_lcdf$', funcname)) or
          bool(re.match(r'.*_lccdf$', funcname))):
        args = format_args_for_yasnippet(x[1], True, False)
        return FUNC_TEMPLATE.format(funcname = funcname,
                                    args = args)
    else:
        args = format_args_for_yasnippet(x[1], False, False)
        return FUNC_TEMPLATE.format(funcname = funcname,
                                    args = args)

def write_all_function_snippets(functions, dst):
    """ Write all functions to files

    For a *_lpdf and *_lpmf function, the distribution
    the distribution equivalent is also created.
    """
    dir_create_or_clean(dst)
    for fxn in functions:
        funcname = fxn[0]
        filename = os.path.join(dst, '%s(%s).yasnippet' % (funcname, ','.join(fxn[1])))
        with open(filename, 'w') as f:
            f.write(create_function_snippet(fxn, False))
        # When we have the corresponding distributions
        if (bool(re.match(r'.*_lpdf$', funcname)) or
            bool(re.match(r'.*_lpmf$', funcname))):
            distname = re.sub(r'_lpdf|_lpmf$', '', funcname)
            if len(fxn[1]) >= 2:
                args = fxn[1][1:]
            else:
                args = tuple()
            distfilename = os.path.join(dst, '%s(%s).yasnippet' % (distname, ','.join(args)))
            with open(distfilename, 'w') as f:
                f.write(create_function_snippet(fxn, True))

def main(src, dst):
    with open(src, 'r') as f:
        data = json.load(f)
    functions = get_unique_function_args(data)
    write_all_function_snippets(functions, os.path.join(dst, 'stan-mode', 'functions'))

if __name__ == '__main__':
    main(*sys.argv[1:3])
