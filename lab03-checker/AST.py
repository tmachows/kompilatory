
class Node(object):
    def __str__(self):
        return self.printTree()

        
class Program(Node):
    def __init__(self, blocks):
        self.blocks = blocks
        
class Blocks(Node):
    def __init__(self, block, blocks):
		self.blocks = []
		if blocks:
			self.blocks.extend(blocks.blocks)
		if block:
			self.blocks.append(block)
			
class Block(Node):
	pass
        
class Declarations(Node):
    def __init__(self, declarations, declaration):
        self.declarations = []
        if declarations:
            self.declarations.extend(declarations.declarations)
        if declaration:
            self.declarations.append(declaration)
            
class Declaration(Node):
    def __init__(self, type, inits, error):
        self.type = type
        self.inits = inits
        self.error = error
        
        
class Inits(Node):
    def __init__(self, inits, init):
        self.inits = []
        if inits:
            self.inits.extend(inits.inits)
        if init:
            self.inits.append(init)
            
class Init(Node):
    def __init__(self, id, expression):
        self.id = id
        self.expression = expression
        
        
class Instructions(Node):
    def __init__(self, instructions, instruction):
        self.instructions = []
        if instructions:
            self.instructions.extend(instructions.instructions)
        if instruction:
            self.instructions.append(instruction)
            
class Instruction(Node):
    pass
    
class Print(Instruction):
    def __init__(self, expression, error):
        self.expression = expression
        self.error = error
        
class Labeled(Instruction):
    def __init__(self, id, instruction):
        self.id = id
        self.instruction = instruction
        


class Assignment(Instruction):
    def __init__(self, id, expression):
        self.id = id
        self.expression = expression

class Choice(Instruction):
    def __init__(self, _if, _else):
        self._if = _if
        self._else = _else
        
class If(Node):
    def __init__(self, cond, statement, error):
        self.cond = cond
        self.statement = statement
        self.error = error

class Else(Node):
    def __init__(self, statement):
        self.statement = statement

class While(Instruction):
    def __init__(self, cond, statement, error):
        self.cond = cond
        self.statement = statement
        self.error = error

class RepeatUntil(Instruction):
    def __init__(self, statement, cond):
        self.cond = cond
        self.statement = statement


class Return(Instruction):
    def __init__(self, expression):
        self.expression = expression


class Continue(Instruction):
    pass


class Break(Instruction):
    pass

class Compound(Instruction):
    def __init__(self, blocks):
        self.blocks = blocks


class Condition(Node):
    pass


class Expression(Condition):
    pass


class Const(Expression):
    def __init__(self, value):
        self.value = value


class Id(Expression):
    def __init__(self, id):
        self.id = id

class BinExpr(Expression):
    def __init__(self, left, op, right):
        self.expr1 = left
        self.operator = op
        self.expr2 = right
        self.children = ( left, right )

class ExpressionInPar(Expression):
    def __init__(self, expression, error):
        self.expression = expression
        self.error = error

class IdWithPar(Expression):
    def __init__(self, id, expression_list, error):
        self.id = id
        self.expression_list = expression_list
        self.error = error
        
class ExpressionList(Node):
    def __init__(self, expr_list, expression):
        self.expressions = []
        if expr_list:
            self.expressions.extend(expr_list.expressions)
        if expression:
            self.expressions.append(expression)


class FunctionDefinitions(Node):
    def __init__(self, fundef, fundefs):
        self.fundefs = []
        if fundef:
            self.fundefs.append(fundef)
        if fundefs:
            self.fundefs.extend(fundefs.fundefs)

class FunctionDefinition(Node):
    def __init__(self, type, id, arglist, compound_instr):
        self.type = type
        self.id = id
        self.arglist = arglist
        self.compound_instr = compound_instr        

class ArgumentList(Node):
    def __init__(self, arg_list, arg):
        self.arg_list = []
        if arg_list:
            self.arg_list.extend(arg_list.arg_list)
        if arg:
            self.arg_list.append(arg)

class Argument(Node):
    def __init__(self, type, id):
        self.type = type
        self.id = id




