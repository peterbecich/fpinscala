# https://en.wikipedia.org/wiki/LL_parser
# All constants are indexed from 0

Term = 0
Rule = 1

# Terminals
T_LPAR = 0
T_RPAR = 1
T_A = 2
T_PLUS = 3
T_END = 4
T_INVALID = 5

# Non-terminals
N_S = 0
N_F = 1

#parse table
table = [[ 1, -1, 0, -1, -1, -1],
         [-1, -1, 2, -1, -1, -1]]

rules = [[(Rule,N_F)],
         [(Term,T_LPAR), (Rule,N_S), (Term,T_PLUS), (Rule,N_F), (Term,T_RPAR)],
         [(Term,T_A)]]

stack = [(Term,T_END), (Rule,N_S)]

def lexicalAnalysis(inputstring):
    print('Lexical analysis') 
    tokens = []
    #cdict = {'+': T_PLUS, '(': T_LPAR, ')': T_RPAR, 'a': T_A}
    #for c in inputstring:
    #    tokens.append(cdict.get(c, T_INVALID))
    #
    # in the meantime it has been changed on wikipedia to simple mapping above, 
    # but the original if-elif-elif-else could be indented to make further distinction 
    # for multi-character terminals like between '-' and '->' .
    for c in inputstring:
        if c   == '+': tokens.append(T_PLUS)
        elif c == '(': tokens.append(T_LPAR)
        elif c == ')': tokens.append(T_RPAR)
        elif c == 'a': tokens.append(T_A)
        else: tokens.append(T_INVALID)
    tokens.append(T_END)
    print(tokens)
    return tokens

def syntacticAnalysis(tokens):
    print('Syntactic analysis')
    position = 0
    while len(stack) > 0:
        (stype, svalue) = stack.pop()
        token = tokens[position]
        if stype == Term:        
            if svalue == token:
                position += 1
                print('pop', svalue)
                if token == T_END:
                    print('input accepted')
            else:
                print('bad term on input:', token)
                break
        elif stype == Rule:
            print('svalue', svalue, 'token', token)
            rule = table[svalue][token]
            print('rule', rule)
            for r in reversed(rules[rule]):
                stack.append(r)
        print('stack', stack)

inputstring = '(a+a)'
syntacticAnalysis(lexicalAnalysis(inputstring))


