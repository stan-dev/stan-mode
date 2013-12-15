import json
import re
import sys
import os
from os import path

def gen_dictwords(data):
    dictwords = list()
    for k in ('keywords', 'pseudo_keywords', 'distributions', 'bounds', 'types',
              'blocks'):
        dictwords += data[k]
    dictwords += [x for x in data['functions'].keys() 
                  if not re.match('operator', x)]
    return '\n'.join(sorted(list(set(dictwords))))

if __name__ == '__main__':
    src, dst = sys.argv[1:3]
    with open(src, 'r') as f:
        data = json.load(f)
    dirname = path.dirname(dst)
    if not path.exists(dirname):
        os.makedirs(dirname)
    with open(dst, 'w') as f:
        f.write(gen_dictwords(data))
    
