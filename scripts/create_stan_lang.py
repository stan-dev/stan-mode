""" Extract function sign

- If new manual, may need to adjust any code that has hardcoded sections.

"""
import json
import re
import sys

""" Stan language version """
VERSION = "2.0.1"

PARTS = {"V": "Built-In Functions",
         "VI": "Discrete Distributions",
         "VII": "Continuous Distributions"}

## Figure 22.2
TYPES = ["int", "real", 
         "matrix", "cov_matrix", "corr_matrix", "cholesky_factor_cov",
         "vector", "simplex", "unit_vector", "ordered", "positive_ordered", "row_vector"]

BLOCKS = ["data",
          "transformed data",
          "parameters",
          "transformed parameters",
          "model",
          "generated quantities"]

## Section 24.1
KEYWORDS = ["for", "in", "while", "print", "if", "else"]
PSEUDO_KEYWORDS = ['lp__']
BOUNDS = ["lower", "upper"]

## Section 22.2
CPP_RESERVED = ["alignas", "alignof", "and", "and_eq", "asm", "auto", "bitand", "bitor", "bool",
                "break", "case", "catch", "char", "char16_t", "char32_t", "class", "compl",
                "const", "constexpr", "const_cast", "continue", "decltype", "default", "delete",
                "do", "double", "dynamic_cast", "else", "enum", "explicit", "export", "extern",
                "false", "float", "for", "friend", "goto", "if", "inline", "int", "long", "mutable",
                "namespace", "new", "noexcept", "not", "not_eq", "nullptr", "operator", "or", "or_eq",
                "private", "protected", "public", "register", "reinterpret_cast", "return",
                "short", "signed", "sizeof", "static", "static_assert", "static_cast", "struct",
                "switch", "template", "this", "thread_local", "throw", "true", "try", "typedef",
                "typeid", "typename", "union", "unsigned", "using", "virtual", "void", "volatile",
                "wchar_t", "while", "xor", "xor_eq"]

## Section 22.2
RESERVED = ["for", "in", "while", "repeat", "until", "if", "then", "else", "true", "false"]

## Section 22.5
OPERATORS = ["||", "&&", "==", "!=", "<", "<=", ">", ">=", "+", "-", "*", "/",
             "\\", ".*", "./", "!", "-", "+", "'"]

def escape_regex(x):
    return re.sub("([|*+.])", r"\\\1", x)

OPERATORS_ESC = [escape_regex(x) for x in OPERATORS]

##########

def parse_manual(src):

    with open(src, "r") as f:
        all_lines = f.readlines()

    # Keep only lines in Parts V-VII"
    re_start = re.compile(r"\s*Part V\b")
    re_end = re.compile(r"\s*Part VIII\b")
    keeplines = []
    touse = False
    for line in all_lines:
        if re_start.search(line):
            keeplines.append(line)
            touse = True
        elif re_end.match(line):
            break
        elif touse:
            keeplines.append(line)

    types = ("real",
             "int",
             "row_vector", 
             "vector", 
             "matrix", 
             "void",
             "T")
    type_decl = "(?P<ret>" + r"|".join([x + r"(?:\[.*?\])?" for x in types]) + ")"

    fun_name = "(?:operator(?:%s)|[A-Za-z][A-Za-z0-9_]*)" % "|".join(OPERATORS_ESC)

    # re_fitem = re.compile(type_decl + r"\s+" + "(?P<fun>%s)" % fun_name

    re_fitem = re.compile("?" + type_decl + r"\s+" + "(?P<fun>%s)" % fun_name + "\(")

    re_fitem_2 = (re.compile(type_decl + r"\s+" + "(?P<fun>%s)" % fun_name
                             + "\((?P<args>.*?)\) *(?P<description>.*)"))

    re_part = re.compile(r"\s*Part\s+(?P<title>%s)\b" % "|".join(PARTS.keys()))
    re_section = re.compile(r"?[0-9]{1,2}\.\s+(?P<title>.*)")
    re_subsection = re.compile(r"?[0-9]{1,2}\.[0-9]+\.?\s+(?P<title>.*)")

    distr_parts = ('Continuous Distributions', 'Discrete Distributions')

    current_part = None
    current_section = None
    current_subsection = None

    functionlist = {}
    distributionlist = set()
    hierarchy = {}

    def valid_distribution_name(x): 
        return (re.search("_log$", x) and not re.search("_c?cdf_log$", x))

    description = []
    in_fitem = False
    for line in keeplines:
        m_part = re_part.match(line)
        m_sec = re_section.match(line)
        m_subsec = re_subsection.match(line)
        m_fitem = re_fitem.match(line)
        if in_fitem:
            if not line or line.strip() == "":
                fitemtext = re.sub(r"\s+", " ", fitemtext)
                m = re_fitem_2.match(fitemtext)
                fname = m.group('fun')
                rettype = m.group('ret')
                arglist = m.group('args')
                description = m.group('description')
                if fname not in ["print"]:
                    ## If a distribution, add to distribution list
                    if (current_part in distr_parts
                        and valid_distribution_name(fname)):
                        distributionlist.add(fname[:-4])
                    args = [x.split() for x in arglist.split(",")]
                    if args == [[]]:
                        argtypes = None
                        argnames = None
                        key = ""
                    else:
                        argtypes = tuple([x[0] for x in args])
                        argnames = [x[1] for x in args]
                        key = ','.join(argtypes)
                    if fname not in functionlist:
                        functionlist[fname] = {}
                    functionlist[fname][key] = {
                        'return': rettype,
                        'argtypes': argtypes,
                        'argnames': argnames,
                        'location': (current_part, current_section, current_subsection),
                        'description': description
                    }
                in_fitem = False
                fitemtext == ""
            else:
                fitemtext += " " + line.strip()
        else:
            if m_part:
                current_part = PARTS[m_part.group('title')]
                current_section = None
                current_subsection = None
            if m_sec:
                current_section = m_sec.group('title')
                current_subsection = None
            elif m_subsec:
                current_subsection = m_subsec.group('title')
            elif m_fitem:
                fitemtext = line.strip()
                in_fitem = True
    return (functionlist, list(distributionlist))

def main(src):
    functions, distributions = parse_manual(src)
    data = {
        'version': VERSION,
        'functions': functions,
        'operators': OPERATORS,
        'blocks': BLOCKS,
        'types': TYPES,
        'reserved': RESERVED,
        'bounds': BOUNDS,
        'cpp_reserved': CPP_RESERVED,
        'pseudo_keywords': PSEUDO_KEYWORDS,
        'keywords': KEYWORDS,
        'distributions': distributions
    }
    return data

if __name__ == '__main__':
    data = main(sys.argv[1])
    print(json.dumps(data))
