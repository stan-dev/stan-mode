import json
import os
import re
import shutil
import sys
import subprocess as sp
from os import path

DISTRIBUTION_PARTS = ('Discrete Distributions', 'Continuous Distributions')

TEMPLATE = """# name: {funcname}({sig})
# key: {funcname}
# group: {group}
# --
{funcname}({args})$0"""

def dir_create_or_clean(dst):
    if path.exists(dst):
        shutil.rmtree(dst)
    os.makedirs(dst)

def make_sig(x):
    if x['argtypes']:
        return ', '.join(x['argtypes'])
    else:
        return ''

def make_dist_sig(x):
    if x['argtypes']:
            return ', '.join(x['argtypes'][1:])
    else:
        return ''

def make_args(x):
    if x['argnames']:
        return ', '.join(['${%d:%s}' % (y[0] + 1, ' '.join(y[1])) 
                          for y in enumerate(zip(x['argtypes'], 
                                                 x['argnames']))])
    else:
        return ""

def make_dist_args(x):
    if x['argnames']:
        return ', '.join(['${%d:%s}' % (y[0] + 1, ' '.join(y[1])) 
                          for y in enumerate(zip(x['argtypes'][1:], 
                                                 x['argnames'][1:]))])
    else:
        return ""

def make_group_function(x):
    return 'Functions.%s' % '.'.join(y for y in x['location'] if y)

def make_group_dist(x):
    return 'Distributions.%s' % x['location'][0].split()[0]

def write_function_snippets(dst, functions):
    dir_create_or_clean(dst)
    for funcname, sigs in functions.items():
        if not re.match("operator", funcname):
            for sig, v in sigs.items():
                if v['argtypes']:
                    cleansig = re.sub(r"[\[\]]", "", '-'.join(v['argtypes']))
                    filename = path.join(dst,
                                         '%s-%s.yasnippet' 
                                         % (funcname, cleansig))
                else:
                    filename = path.join(dst, '%s.yasnippet' % funcname)
                snippet = TEMPLATE.format(funcname = funcname,
                                          sig = make_sig(v),
                                          args = make_args(v),
                                          group = make_group_function(v))
                with open(filename, 'w') as f:
                    f.write(snippet)
            
def write_distribution_snippets(dst, functions, distributions):
    dir_create_or_clean(dst)
    distribution_funcs = ['%s_log' % x for x in distributions]
    for funcname, sigs in functions.items():
        if funcname in distribution_funcs:
            distname = funcname[:-4]
            for sig, v in sigs.items():
                cleansig = re.sub(r"[\[\]]", "", '-'.join(v['argtypes']))
                filename = path.join(dst,
                                     '%s-%s.yasnippet' % (funcname, cleansig))
                snippet = TEMPLATE.format(funcname = distname,
                                          sig = make_dist_sig(v),
                                          args = make_dist_args(v),
                                          group = make_group_dist(v))
                with open(filename, 'w') as f:
                    f.write(snippet)
        
if __name__ == '__main__':
    src, dst = sys.argv[1:3]
    function_dir = path.join(dst, 'stan-mode', 'functions')
    dist_dir = path.join(dst, 'stan-mode', 'distributions')

    with open(src, 'r') as f:
        data = json.load(f)
    write_function_snippets(function_dir, data['functions'])
    write_distribution_snippets(dist_dir, data['functions'], 
                                data['distributions'])
