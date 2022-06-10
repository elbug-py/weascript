import ply.lex as lex
import ply.yacc as yacc


reserved={
   '+' : 'SU',
   '-' : 'RE',
   '=' : 'ASIGN',
   '*' : 'MULT',
   '/' : 'DIV',
   '(' : 'LPAREN',
   ')' : 'RPAREN'
}

tokens = [
    'N',
    'ID'
    ]+list(reserved.values())

t_SU = r'\+'
t_RE = r'\-'
t_MULT = r'\*'
t_DIV = r'\/'
t_ASIGN = r'\='
t_LPAREN  = r'\('
t_RPAREN  = r'\)'

t_ignore = ' \t'

def t_N(t):
    r'[0-9]+'
    t.value = int(t.value)
    return t
    
def t_ID(t):
    r'[a-zA-Z_][a-zA-Z_0-9]*'
    t.type = reserved.get(t.value,'ID')
    return t

def t_error(t):
    print(":(")
    t.lexer.skip(1)


precedence = (
    ('left', 'SU', 'RE'),
    ('left', 'MULT', 'DIV'),
    ('left', 'LPAREN', 'RPAREN'),)

variables={}

def p_resultado(t):
    'resultado : s'
    print(t[1])

def p_asignacion(t):
    'resultado : ID ASIGN s'
    variables[t[1]]=t[3]

def p_expr_num(t):
    's : N'
    t[0]=t[1]
    
def p_expr_id(t):
    's : ID'
    try:
        t[0] = variables[t[1]]
    except LookupError:
        print("Variable indefinida '%s'" % t[1])
        t[0] = 0
    
def p_oper(t):
    '''s : s SU s
        |  s RE s
        |  s MULT s
        |  s DIV s'''
    if t[2] == '+':
        t[0] = t[1] + t[3]
    elif t[2] == '-':
        t[0] = t[1] - t[3]
    elif t[2] == '*':
        t[0] = t[1] * t[3]
    elif t[2] == '/':
        try:
            t[0] = t[1] / t[3]
        except ZeroDivisionError:
            print('no podi dividir por 0 po oe')

def p_paren(t):
    '''s : LPAREN s SU s RPAREN
        | LPAREN s RE s RPAREN
        | LPAREN s DIV s RPAREN
        | LPAREN s MULT s RPAREN'''
    if t[3] == '+':
        t[0] = t[2] + t[4]
    elif t[3] == '-':
        t[0] = t[2] - t[4]
    elif t[3] == '*':
        t[0] = t[2] * t[4]
    elif t[3] == '/':
        try:
            t[0] = t[2] / t[4]
        except ZeroDivisionError:
            print('no podi dividir por 0 po oe')

def p_error(t):
    print(":'(")


lexer = lex.lex()
parser = yacc.yacc()
while True:
    try:
        data = input("core.wea> ")
    except EOFError:
        break
    parser.parse(data)