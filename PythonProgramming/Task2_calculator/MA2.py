
"""
Note:
The program is only working for a very tiny set of operations.
You have to add and/or modify code in ALL functions as well as add some new functions.
Use the syntax charts when you write the functions!
However, the class SyntaxError is complete as well as handling in main
of SyntaxError and TokenError.
"""

import math
from tokenize import TokenError  
from MA2tokenizer import TokenizeWrapper


class SyntaxError(Exception):
    def __init__(self, arg):
        self.arg = arg
        super().__init__(self.arg)

class TokenError(Exception):
    def __init__(self, arg):
        self.arg = arg
        super().__init__(self.arg)

class EvaluationError(Exception):
    def __init__(self, arg):
        self.arg = arg
        super().__init__(self.arg)



def log(n):
    if n <= 0:
        raise EvaluationError('Should be postive num')
    else:
        return math.log(n)

memory = {0:0,1:1}    
def fib(n):

    if n.is_integer() and n > 0:

        def fib_mem(n):
            if n not in memory:
                memory[n] = fib_mem(n-1) + fib_mem(n-2)
            return memory[n]
    
        return fib_mem(n)
    
    else:
        raise EvaluationError('Expected n to be postive integer')
  

def fac(n):
    if n == 1:
        return 1
    elif n <= 0 or not n.is_integer():
        raise EvaluationError('Expected n to be positive integer')
    else:
        return int(n) * fac(n-1)
    
def mean(*args):
    total = sum(*args)
    mean = total/len(*args)
    return mean if len(*args) > 0 else 0

function_1 = {
    'sin': math.sin,
    'cos': math.cos,
    'exp': math.exp,
    'log': log,
    'fib': fib,
    'fac': fac
}

function_n = {
    'sum': sum,
    'max': max,
    'min': min,
    'mean': mean
}

######################################################################### 

def arglist(wtok, variables):
    namelist = []
    if wtok.get_current() == '(':
        wtok.next()
        namelist.append(assignment(wtok,variables))
    else: 
        raise SyntaxError('Expected (')
    while wtok.get_current() == ',':
        wtok.next()
        namelist.append(assignment(wtok,variables))
    if wtok.get_current() == ')':
        wtok.next()
        return namelist
    else:
        raise SyntaxError('Expected )')
    

def statement(wtok, variables):
    if wtok.is_at_end():
        pass
    else:
        result = assignment(wtok, variables)
        if not wtok.is_at_end():
            raise SyntaxError('Expected end of line')
        else: 
            return result
        

def assignment(wtok, variables):
    result = expression(wtok, variables)
    while wtok.get_current() == '=':
        wtok.next()
        if wtok.is_name():
            variables[wtok.get_current()] = result
        else:
            raise SyntaxError ('The variable name is not defined')
        
        wtok.next()
    return result

def expression(wtok, variables):
    result = term(wtok, variables)
    while wtok.get_current() in ['+','-']:
        if wtok.get_current() == '+':
            wtok.next()
            result = result + term(wtok, variables)
        elif wtok.get_current() == '-':
            wtok.next()
            result = result - term(wtok, variables)
    return result


'''
Always set variable name to divisor and use that variable name
'''
def term(wtok, variables):
    result = factor(wtok, variables)
    while wtok.get_current() in ['*','/']: 
        if wtok.get_current() == '*': 
            wtok.next()
            result = result * factor(wtok, variables)
        elif wtok.get_current() == '/': 
            wtok.next()
            devisor = factor(wtok,variables)
            if devisor == 0:
                raise EvaluationError('devisor cannot be zero')
            result = result / devisor
    return result


def factor(wtok, variables):  
    if wtok.get_current() == '(':
        wtok.next()
        result = assignment(wtok, variables)
        if wtok.get_current() != ')':
            raise SyntaxError("Expected ')'")
        else:
            wtok.next()
    elif wtok.is_number():
        result = float(wtok.get_current())
        wtok.next()
    elif wtok.get_current() == '-':
        wtok.next()
        return (-1) * factor(wtok,variables)
    elif wtok.get_current() in function_1:
        wtok.next()
        if wtok.get_current() == '(':
            result = function_1[wtok.get_previous()](factor(wtok,variables))
        else:
            raise SyntaxError('Expected (')
    elif wtok.get_current() in function_n:
        wtok.next()
        result = function_n[wtok.get_previous()](arglist(wtok, variables))
    elif wtok.is_name():
        if wtok.get_current() not in variables:
            raise EvaluationError('This is an undefined function name')
        else:
            result = variables[wtok.get_current()]
        wtok.next()
    else:
        raise SyntaxError(
            "Expected number or '('")  
    return result


         
def main():
   
    print("Numerical calculator")
    variables = {"ans": 0.0, "E": math.e, "PI": math.pi}
   
    init_file = 'MA2init.txt'
    lines_from_file = ''
    try:
        with open(init_file, 'r') as file:
            lines_from_file = file.readlines()
    except FileNotFoundError:
        pass

    while True:
        if lines_from_file:
            line = lines_from_file.pop(0).strip()
            print('init  :', line)
        else:
            line = input('\nInput : ')
        if line == '' or line[0]=='#':
            continue
        wtok = TokenizeWrapper(line)

        if wtok.get_current() == 'quit':
            print('Bye')
            exit()

        elif wtok.get_current() == 'vars':
            print(f'The predefined variables are: {variables}')

    
        else:
            try:
                result = statement(wtok, variables)
                variables['ans'] = result
                print('Result:', result)

            except SyntaxError as se:
                print("*** Syntax error: ", se)
                print(
                f"Error occurred at '{wtok.get_current()}' just after '{wtok.get_previous()}'")

            except TokenError as te:
                print('*** Syntax error: Unbalanced parentheses')

            except EvaluationError as ee:
                print('***Evaluation error: ', ee)
 


if __name__ == "__main__":
    main()
