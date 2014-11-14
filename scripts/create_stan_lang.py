""" Extract function sign

- If new manual, may need to adjust any code that has hardcoded sections.

"""
import json
import re
import sys

""" Stan language version """
VERSION = "2.4.0"

PARTS = ("Built-In Functions",
         "Discrete Distributions",
         "Continuous Distributions")

## Figure 22.2
TYPES = ["int", "real", 
         "matrix", "cov_matrix", "corr_matrix", "cholesky_factor_cov", "cholesky_factor_corr",
         "vector", "simplex", "unit_vector", "ordered", "positive_ordered", "row_vector"]

BASIC_TYPES = ["int", "real", "vector", "row_vector", "matrix"]
FUNCTION_RETURN_TYPES = BASIC_TYPES + ["void"]

BLOCKS = ["functions",
          "data",
          "transformed data",
          "parameters",
          "transformed parameters",
          "model",
          "generated quantities"]

## Section 24.1
KEYWORDS = ["for", "in", "while", "if", "else", "return"]
FUNCTION_LIKE_KEYWORDS = ["print", "increment_log_prob", "reject", "integrate_ode"]
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
RESERVED = ["for", "in", "while", "repeat", "until", "if", "then", "else", "true", "false", "fvar", "var"]

## Section 22.5
OPERATORS = ["||", "&&", "==", "!=", "<", "<=", ">", ">=", "+", "-", "*", "/",
             "\\", ".*", "./", "!", "-", "+", "'", "^"]

def escape_regex(x):
    return re.sub("([|*+.])", r"\\\1", x)

OPERATORS_ESC = [escape_regex(x) for x in OPERATORS]

##########

def parse_manual(src):

    with open(src, "r") as f:
        lines = f.readlines()

    types = ("real",
             "int",
             "row_vector", 
             "vector", 
             "matrix", 
             "void",
             "T")
    type_decl = "(?P<ret>" + "(?:" + r"|".join([x for x in types]) +")" + r"(?:\[.*?\])?" + ")"

    fun_name = "(?:operator(?:%s)|[A-Za-z][A-Za-z0-9_ ]*)" % "|".join(OPERATORS_ESC)

    arg_types = ("reals?",
                 "ints?",
                 "row_vector", 
                 "vector", 
                 "matrix", 
                 "void",
                 "T")
    named_arg = ("(?:" + "(?:" + r"|".join([x for x in arg_types]) +")" + r"(?:\[.*?\])?" + ")"
                 + " " + "[A-Za-z][A-Za-z0-9_]*(?:\[.*\])?")

    # re_fitem = re.compile(type_decl + r"\s+" + "(?P<fun>%s)" % fun_name

    re_fitem = re.compile("?" + type_decl + r"\s+" + "(?P<fun>%s)" % fun_name + "\(")

    re_fitem_2 = (re.compile(type_decl + r"\s+" + "(?P<fun>%s)" % fun_name
                             + "\((?P<args>.*?)\) *(?P<description>.*)"))

    re_part = re.compile(r"^# +(?P<title>.*)")
    re_section = re.compile(r"## +(?P<title>.*)")
    re_subsection = re.compile(r"### +(?P<title>.*)")

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
    for line in lines:
        m_part = re_part.match(line)
        m_sec = re_section.match(line)
        m_subsec = re_subsection.match(line)
        m_fitem = re_fitem.match(line)
        if in_fitem:
            if not line or line.strip() == "":
                fitemtext = re.sub(r"\s+", " ", fitemtext)
                m = re_fitem_2.match(fitemtext)
                fname = re.sub(' ', '_', m.group('fun'))
                rettype = m.group('ret')
                arglist = m.group('args')
                description = m.group('description')
                if fname not in ["print"]:
                    ## If a distribution, add to distribution list
                    if (current_part in distr_parts
                        and valid_distribution_name(fname)):
                        distributionlist.add(fname[:-4])
                    if arglist.strip() == "":
                        argtypes = None
                        argnames = None
                        key = ""
                    else:
                        args = [x.split() for x in re.findall(named_arg, arglist)]
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
                current_part = m_part.group('title')
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
        'function_like_keywords': FUNCTION_LIKE_KEYWORDS,
        'keywords': KEYWORDS,
        'basic_types': BASIC_TYPES,
        'function_return_types': FUNCTION_RETURN_TYPES,
        'distributions': distributions
    }
    return data

if __name__ == '__main__':
    data = main(sys.argv[1])
    dst = sys.argv[2]
    with open(dst, 'w') as f:
        json.dump(data, f, sort_keys = True, indent = 2, separators = (',', ': '))
