import json
import re
import sys
import os
from os import path

def gen_dictwords(data):
    words = set()
    for k in data['keywords']:
        for x in data['keywords'][k]:
            words.add(x)
    for k in data['types']:
        for x in data['types'][k]:
            words.add(x)
    for x in data['blocks']:
        words.add(x)
    for k in data['functions']:
        if k not in data['operator_functions']:
            words.add(k)
    for x in data['distributions']:
        words.add(x)
    return '\n'.join(sorted(list(words)))

if __name__ == '__main__':
    src, dst = sys.argv[1:3]
    with open(src, 'r') as f:
        data = json.load(f)
    dirname = path.dirname(dst)
    if not path.exists(dirname):
        os.makedirs(dirname)
    with open(dst, 'w') as f:
        f.write(gen_dictwords(data))
    
