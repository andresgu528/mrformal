import copy
from typing import Self

nandsym = 'D'

def tokenize(data, sep=None) -> list:
    if type(data) is str:
        return data.split(sep=sep)
    elif type(data) is list:
        return data
    else:
        raise TypeError("Type of 'data' not supported.")

def check_valid(data, node=nandsym, sep=None) -> None:
        tokens = tokenize(data, sep=sep)
        length = len(tokens)
        if length == 0:
            raise TypeError("Empty formula not allowed.")
        count = 0
        for i, token in enumerate(tokens):
            if token == node:
                count += 1
            else:
                count -= 1
            if (count < 0) and (i < length - 1):
                raise ValueError("'data' does not represent a valid formula.")
        if count != -1:
            raise ValueError("'data' does not represent a valid formula.")

def decompose(data, node=nandsym, sep=None, check=True):
    if check:
        check_valid(data, node=node, sep=sep)
    tokens = tokenize(data, sep=sep)
    length = len(tokens)
    if length == 1:
        return tokens[0], None, None
    count = 0
    tokens = tokens[1:]
    for i, token in enumerate(tokens):
        if token == node:
            count += 1
        else:
            count -= 1
        if count == -1:
            return None, tokens[:i+1], tokens[i+1:]
    raise ValueError('Decomposition failed.')


class SSF:

    def __init__(self, data=None, node:str=nandsym, sep:str=None, check:bool=True, v=None, l=None, r=None):
        if check:
            check_valid(data, node=node, sep=sep)
        if data is not None:
            tokens = tokenize(data, sep=sep)
            head = tokens[0]
            if head != node:
                self.v = head
                self.l = None
                self.r = None
            else:
                v, l, r = decompose(tokens, node=node, sep=sep, check=False)
                self.v = None
                self.l = SSF(data=l, node=node, sep=sep, check=False)
                self.r = SSF(data=r, node=node, sep=sep, check=False)
        elif v is not None:
            self.v = v
            self.l = None
            self.r = None
        elif (l is not None) and (r is not None):
            self.v = None
            self.l = l
            self.r = r
        else:
            raise ValueError("Parameters don't specify a valid formula.")
    
    @property
    def polish(self) -> str:
        if self.v is not None:
            return self.v
        else:
            return f'{nandsym} {self.l.polish} {self.r.polish}'
    
    def __repr__(self) -> str:
        return f"SSFormula('{self.polish}')"
    
    @property
    def leaves(self) -> list:
        if self.v is not None:
            return [self]
        else:
            return self.l.leaves + self.r.leaves

    @property
    def variables(self) -> list:
        bag = set()
        result = []
        for variable in [leave.v for leave in self.leaves]:
            if variable not in bag:
                bag.add(variable)
                result.append(variable)
        return result
    
    def substitute(self, subst:dict, node=nandsym, sep=None, inplace=False) -> Self:
        if inplace:
            tree = self
        else:
            tree = copy.deepcopy(self)
        if len(subst) == 0:
            return tree
        affected_leaves = [l for l in tree.leaves if l.v in subst.keys()]
        for leave in affected_leaves:
            subst_leave = SSF(subst[leave.v], node=node, sep=sep)
            if subst_leave.v is not None:
                leave.v = subst_leave.v
            else:
                leave.v = None
                leave.l = subst_leave.l
                leave.r = subst_leave.r
        return tree
    
    def standarize_variables(self, how='alph', inplace=False) -> Self:

        def how_num(n:int) -> str:
            return str(n + 1)
        
        def how_alph(n:int) -> str:
            result = ''
            while n >= 26:
                n
                result = f'{chr(97 + (n % 26))}{result}'
                n = (n // 26) - 1
            return f'{chr(97 + n)}{result}'
        
        if how == 'num':
            how_fn = how_num
        elif how == 'alph':
            how_fn = how_alph
        else:
            raise ValueError("'how' argument not accepted.")
        
        subst = {var:how_fn(i) for i, var in enumerate(self.variables)}
        return {'tree':self.substitute(subst, inplace=inplace), 'subst':subst}
    
    def prepend_variables(self, prefix:str='p', inplace=False):
        subst = {var:f"{prefix}{var}" for var in self.variables}
        return self.substitute(subst, inplace=inplace)
    
    def does_generalize(self, formula:Self) -> bool:

        def get_subst(f1:SSF, f2:SSF):
            if (f1.v is not None):
                if f2.v is not None:
                    if f1.v == f2.v:
                        return dict()
                    elif f1.v[0] == '2':
                        raise RecursionError(f"Generalization not possible.")
                    else:
                        return {f1.v:f2.v}
                else:
                    f2p = f2.polish
                    if f1.v not in f2.variables:
                        return {f1.v:f2p}
                    else:
                        raise RecursionError(f"Generalization not possible.")
            elif (f2.v is not None):
                raise RecursionError(f"Generalization not possible.")
            else:
                left_subst = get_subst(f1.l, f2.l)
                if len(left_subst) != 0:
                    return left_subst
                else:
                    right_subst = get_subst(f1.r, f2.r)
                    return right_subst
                
        tree1 = self.prepend_variables('1', inplace=False)
        tree2 = formula.prepend_variables('2', inplace=False)
        subst = dict()
        try:
            single_subst = get_subst(tree1, tree2)
        except RecursionError:
            return False
        while len(single_subst) != 0:
            subst = compose_subst(subst, single_subst)
            tree1.substitute(single_subst, inplace=True)
            # tree2.substitute(single_subst, inplace=True)
            try:
                single_subst = get_subst(tree1, tree2)
            except RecursionError:
                return False

        return True
    
    def check_generalization(self, against:dict):
        for code in against:
            if against[code]['tree'].does_generalize(self):
                print(f"Don't include: {code} ({against[code]['name']}) generalizes this formula.")
                return
            elif self.does_generalize(against[code]['tree']):
                print(f"Include use2 and remove use of previous: This formula is generalized by {code} ({against[code]['name']}).")
                return
        print("Include use2: The formula can be added to the list.")
        return

    def check_repeated_inference(self, against:dict):
        for code in {c:v for c,v in against.items() if v['use1']}:
            pagainst = SSF(check=False, v=None, l=against[code]['tree'].l, r=against[code]['tree'].r.r)
            pself = SSF(check=False, v=None, l=self.l, r=self.r.r)
            if pagainst.does_generalize(pself):
                print(f"Don't include use1: Hypothesis and conclusion of {code} ({against[code]['name']}) generalize the ones of this formula.")
                return
            elif pself.does_generalize(pagainst):
                print(f"Include use1 and remove use1 of previous: Hypothesis and conclusion of this formula generalize the ones of {code} ({against[code]['name']}).")
                return
        print('Include use1: This formula gives a new inference rule.')
        return


def compose_subst(subst1:dict, subst2:dict) -> dict:
    subst = dict()
    for k1 in subst1:
        subst[k1] = SSF(subst1[k1]).substitute(subst2, inplace=True).polish
    for k2 in subst2:
        if k2 not in subst:
            subst[k2] = subst2[k2]
    return subst
        

def unify(f1:SSF, f2:SSF, distinguish:bool=True):

    def get_subst(f1:SSF, f2:SSF) -> dict:
        if (f1.v is not None):
            if f2.v is not None:
                if f1.v == f2.v:
                    return dict()
                return {f1.v:f2.v}
            else:
                f2p = f2.polish
                if f1.v not in f2.variables:
                    return {f1.v:f2p}
                else:
                    raise RecursionError(f"Unification not possible. ({f1.v}) ({f2p})")
        elif (f2.v is not None):
            f1p = f1.polish
            if f2.v not in f1.variables:
                return {f2.v:f1p}
            raise RecursionError(f"Unification not possible. ({f1p}) ({f2.v})")
        else:
            left_subst = get_subst(f1.l, f2.l)
            if len(left_subst) != 0:
                return left_subst
            else:
                right_subst = get_subst(f1.r, f2.r)
                return right_subst

    if distinguish:
        tree1 = f1.prepend_variables('1', inplace=False)
        tree2 = f2.prepend_variables('2', inplace=False)
    else:
        tree1 = copy.deepcopy(f1)
        tree2 = copy.deepcopy(f2)
    
    unification = dict()
    subst = get_subst(tree1, tree2)
    
    while len(subst) != 0:
        unification = compose_subst(unification, subst)
        tree1.substitute(subst, inplace=True)
        tree2.substitute(subst, inplace=True)
        subst = get_subst(tree1, tree2)

    return {'subst':unification, 'tree':tree1}

def apply(f1:SSF, f2:SSF) -> dict:
    unif_subst = unify(f1.l, f2, distinguish=True)['subst']
    result = f1.r.r.prepend_variables('1', inplace=False)
    result.substitute(unif_subst, inplace=True)
    stand_subst = result.standarize_variables(how='alph', inplace=True)['subst']
    return {'tree':result, 'subst':compose_subst(unif_subst, stand_subst)}

theorems = {
    '0':{'name':'0',
        'tree':SSF('D D a D b c D D D d c D D a d D a d D a D a b'),
        'use1':True, 'use2':True},
    'p00':{'name':'1',
        'tree':SSF('D D a D b c D D a D b c D D d c D D a d D a d'),
        'use1':True, 'use2':True},
    'p0p00':{'name':'2.1',
        'tree':SSF('D D a D b c D D a D b c D a D b c'),
        'use1':True, 'use2':True},
    'pp000':{'name':'2.2',
        'tree':SSF('D D a D b D b c D D D b D c d a D D b D c d a'),
        'use1':True, 'use2':True},
    'pp00p00':{'name':'2.3',
        'tree':SSF('D D a D D b c D D d b D d b D D D d D e c a D D d D e c a'),
        'use1':True, 'use2':True},
    'p0pp000':{'name':'3.1',
        'tree':SSF('D D a D b D b c D D a D b D b c D D b D c d a'),
        'use1':False, 'use2':True},
    'p0pp00p00':{'name':'3.2',
        'tree':SSF('D D a D D b c D D d b D d b D D a D D b c D D d b D d b D D d D e c a'),
        'use1':False, 'use2':True},
    'pp00p0p00':{'name':'3.3',
        'tree':SSF('D D a D b D c d D D D b D c d a D D b D c d a'),
        'use1':True, 'use2':True},
    'pp00pp000':{'name':'3.4',
        'tree':SSF('D D a D D b D c d e D D D e D b D b c a D D e D b D b c a'),
        'use1':True, 'use2':True},
    'pp00pp00p00':{'name':'3.5',
        'tree':SSF('D D a D D b D c d e D D D e D D f d D D b f D b f a D D e D D f d D D b f D b f a'),
        'use1':True, 'use2':True},
}

def th_code(code:str) -> SSF:
    return theorems[code]['tree']
def th_name(name:str) -> SSF:
    filter = [theorems[code]['tree'] for code in theorems if theorems[code]['name'] == name]
    return filter[0]


def apply_and_check(f1:SSF, f2:SSF, against:dict=theorems):
    result = apply(f1, f2)
    result['tree'].check_generalization(against)
    result['tree'].check_repeated_inference(against)
    print(f"Substitutions needed:")
    print(result['subst'])
    print(f"Obtained formula: {result['tree'].polish}")
    print(f"Hypothesis: {result['tree'].l.polish}")
    print(f"Conclusion: {result['tree'].r.r.polish}")
    