## validate whether the grammer is strict-LL1 grammer
## strict-LL1 grammer is defined as the grammer without
## 1. body that has optional consecutive variables such as V ::= [ Va Vb ], where V, Va, Vb are variables
## 2. variable whose any body does not share first elements with others (then, the grammer is called LL1 grammer)


import re
import functools as ft


## patterns for EBNF elements
TERMINAL_PATTERN = r'".*?"|@[A-Z][A-Za-z]*'
VARIABLE_PATTERN = r'[A-Z][A-Za-z]*' 
METACHAR_PATTERN = r'[\|\[\]\{\}]|::='


def validate_grammer():

    ## read the file written the grammer in
    with open('grammer.ebnf') as f:
        lines = f.readlines()
    lines = remove_comment_lines(lines)

    ## validate lexical and syntax features
    for idx, line in enumerate(lines):
        line_number = idx + 1
        if not validate_lexically(line):
            print( 'error: the grammer has lexical error')
            print(f'  at line {line_number}: {line}') 
            return 
        if not validate_syntactically(line):
            print( 'error: the grammer has syntax error')
            print(f'  at line {line_number}: {line}')
            return 

    ## construct grammer dict
    grammer = {}
    for line in lines:
        head, bodies = divide_into_head_and_bodies(line)
        if head in grammer.keys():
            grammer[head] += bodies
        grammer[head] = bodies

    ## validate the grammer has no body that has optional consecutive variables
    for head, bodies in grammer.items():
        for body in bodies:
            sanitised_body = drop_terminals(body)
            optional_parts = re.findall(r'\[.*?\]|\{.*?\}', sanitised_body)
            for optional_part in optional_parts:
                if has_consecutive_variables(optional_part):
                    print( 'error: the grammer has optional consecutive non-terminals')
                    print(f'  {optional_part} in {head} ::= {body}')
                    return
                
    ## validate the grammer is LL1 grammer
    for head, bodies in grammer.items():
        if len(bodies) < 2:
            continue
        first_sets = []
        for body in bodies:
            first_elem = extract_first_elem_from_body(body)
            first_set = get_first_set(grammer, first_elem)
            first_sets.append(first_set)
        universe = ft.reduce(set.union, first_sets, set())
        inter_set = ft.reduce(set.intersection, first_sets, universe)
        if inter_set != set():
            print( 'error: the grammer is not LL1 grammer')
            print(f'  {inter_set} in {head} ::= {" | ".join(bodies)}')
            return

    ## the grammer is strict-LL1 grammer
    print('OK')


def remove_comment_lines(lines: list[str]) -> list[str]:
    return [line for line in lines if re.match(r'[^#]\S+', line)]


def validate_lexically(line: str) -> bool:
    rest = \
        re.sub(METACHAR_PATTERN, '',
            re.sub(VARIABLE_PATTERN, '',
                re.sub(TERMINAL_PATTERN, '', line)
            )
        )
    return re.match(r'\S+', rest) is None


def validate_syntactically(line: str) -> bool:
    ## TODO
    return True


def divide_into_head_and_bodies(line: str) -> tuple[str, list[str]]:
    elems = line.split()
    head = elems[0]
    head = head.strip()
    bodies = (' '.join(elems[2:])).split('|')
    bodies = list(map(lambda s: s.strip(), bodies))
    return head, bodies


def drop_terminals(body: str) -> str:
    return re.sub(TERMINAL_PATTERN, ' ', body)


def has_consecutive_variables(optional_part: str) -> bool:
    optional_part = optional_part.strip(' []\{\}')
    return len(optional_part.split()) > 1


def get_first_set(grammer: dict, head: str) -> set[str]:
    if re.match(TERMINAL_PATTERN, head):
        return set([head])
    bodies = grammer[head]
    first_set = set()
    for body in bodies:
        first_elem = extract_first_elem_from_body(body)
        first_set = first_set.union(get_first_set(grammer, first_elem))
    return first_set
    
def extract_first_elem_from_body(body: str) -> str:
    body = re.sub(r'\[(.*?)\]', r'\1', body)
    body = re.sub(r'\{(.*?)\}', r'\1', body)
    return body.split()[0] 

if __name__ == '__main__':
    validate_grammer()
