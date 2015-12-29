""" Extract function sign

- If new manual, may need to adjust any code that has hardcoded sections.

"""
import csv
import json
import re
import sys




arg_types = (
    "reals?",
    "ints?",
    "(?:(?:row|col)_)?vector",
    "matrix",
    "T"
)

##########

def parse_args(argtext):
    if argtext != "()":
        arg_type = r"(?:" + r"|".join(arg_types) + ")"
        arg_name = "[A-Za-z][A-Za-z0-9_]*(?:\[.*\])?"
        arg_regex = re.compile(r"(?P<type>%s(?:\[(?:\.{3}|,)?\])?)\s+(?P<name>%s)" % (arg_type, arg_name))
        matches = arg_regex.findall(argtext)
        if not len(matches):
            print("Could not find any matches: %s" % argtext)
    else:
        matches = ()
    return matches

def parse_functions(src):

    with open(src, "r") as f:
        reader = csv.reader(f, delimiter = ';')
        data = [row for row in reader][2:]

    distributions = set()
    functions = {}

    for row in data:
        funname, funargs, funret = row[:3]
        if funargs == "~":
            distributions.add(funname)
        else:
            if funname in FUNCTION_LIKE_KEYWORDS:
                continue
            else:
                args = parse_args(funargs)
            f = {
                'name': funname,
                'return': funret,
                'argtypes': [x[0] for x in args],
                'argnames': [x[1] for x in args],
            }
            signature = ','.join(f['argtypes'])
            if funname not in functions:
                functions[funname] = {}
            functions[funname][signature] = f
    return (functions, list(distributions))

def main(src, dst):
    functions, distributions = parse_functions(src)
    version = re.search(r"-([0-9]+\.[0.9]+\.[0-9]+)\.txt$", src).group(1)
    print("Stan version: %s" % version)
    data = {
        'version': version,
        'functions': functions,
        'operators': OPERATORS,
        'blocks': BLOCKS,
        'types': TYPES,
        'reserved': RESERVED,
        'bounds': BOUNDS,
        'cpp_reserved': CPP_RESERVED,
        'pseudo_keywords': PSEUDO_KEYWORDS,
        'function_like_keywords': FUNCTION_LIKE_KEYWORDS,
        'keywords': KEYWORDS,
        'basic_types': BASIC_TYPES,
        'function_return_types': FUNCTION_RETURN_TYPES,
        'distributions': distributions,
        'nondistribution_log_functions': NOT_DISTRIBUTIONS
    }
    with open(dst, 'w') as f:
        json.dump(data, f, sort_keys = True, indent = 2, separators = (',', ': '))

if __name__ == '__main__':
    src, dst = sys.argv[1:3]
    main(src, dst)
