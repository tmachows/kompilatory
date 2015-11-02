
class Node(object):
    def __str__(self):
        return self.printTree()

		
class Program(Node):
	def __init__(self, declarations, fundefs, instructions):
		self.declarations = declarations
		self.fundefs = fundefs
		self.instructions = instructions
		
		
class Declarations(Node):
	def __init__(self, declarations, declaration):
		self.declarations = []
		if declarations:
			self.declarations.extend(declarations.declarations)
		if(declaration):
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
		
		
# ##### do tego miejsca juz jest raczej ok, ponizej mozna modyfikowac

class BinExpr(Node):

    def __init__(self, op, left, right):
        self.op = op
        self.left = left
        self.right = right


class Const(Node):

    def __init__(self, const_type, value):
        self.type = const_type
        self.value = value

class Integer(Const):
    def __init__(self,const):
        self.type = const.type
        self.value = const.value


class Float(Const):
    def __init__(self,const):
        self.type = const.type
        self.value = const.value

class String(Const):
    def __init__(self,const):
        self.type = const.type
        self.value = const.value
      
class Expression(Node):
    def __init__(self, expr1, op, expr2):
        self.expr1 = expr1
        self.op = op
        self.expr2 = expr2
        self.type = types[op][expr1.type][expr2.type]
        
class OneArgExpression(Expression):
    def __init__(self, expr):
        self.expr1 = expr
        self.type = expr.type

class ExpressionInBrackets(Expression):
    def __init__(self, expr):
        self.expr1 = expr
        self.type = expr.type
    
class Assignment(Node):
    def __init__(self, var, expr):
        self.var = var
        self.expr = expr
        self.type = types["="][var.type][expr.type]
        
class Condition(Node):
    def __init__(self, expr1, op, expr2):
        self.expr1 = expr1
        self.op = op
        self.expr2 = expr2
        self.type = types[op][expr1.type][expr2.type]
		
      
class Choice(Node):
    def __init__(self, cond, stmt1, stmt2=None):
        self.cond = cond
        self.stmt1 = stmt1
        self.stmt2 = stmt2


class While(Node):
    def __init__(self, cond, stmt):
        self.cond = cond
        self.stmt = stmt

		
#????????????????????????????????????????????????????????????????????
class repeat-until(Node):
    def __init__(self, stmt,cond):
        self.cond = cond
        self.stmt = stmt
#????????????????????????????????????????????????????????????????????        

		
		
cond_reverse = {
    ">" : "<=",
    ">=" : "<",
    "<" : ">=",
    "<=" : ">",
    "==" : "!=",
    "!=" : "==",
}            
   
types = {
    "+" : {
        "int" : {
            "int" : "int",
            "float" : "float",
            "string" : None,
            None : None
        },
        "float" : {
            "int" : "float",
            "float" : "float",
            "string" : None,
            None : None
        },
        "string" : {
            "int" : None,
            "float" : None,
            "string" : "string",
            None : None
        },
        None : {
            "int" : None,
            "float" : None,
            "string" : None,
            None : None
        }
    },
    "-" : {
        "int" : {
            "int" : "int",
            "float" : "float",
            "string" : None,
            None : None
        },
        "float" : {
            "int" : "float",
            "float" : "float",
            "string" : None,
            None : None
        },
        "string" : {
            "int" : None,
            "float" : None,
            "string" : None,
            None : None
        },
        None : {
            "int" : None,
            "float" : None,
            "string" : None,
            None : None
        }
    },
    "*" : {
        "int" : {
            "int" : "int",
            "float" : "float",
            "string" : None,
            None : None
        },
        "float" : {
            "int" : "float",
            "float" : "float",
            "string" : None,
            None : None
        },
        "string" : {
            "int" : "string",
            "float" : None,
            "string" : None,
            None : None
        },
        None : {
            "int" : None,
            "float" : None,
            "string" : None,
            None : None
        }
    },
    "/" : {
        "int" : {
            "int" : "int",
            "float" : "float",
            "string" : None,
            None : None
        },
        "float" : {
            "int" : "float",
            "float" : "float",
            "string" : None,
            None : None
        },
        "string" : {
            "int" : None,
            "float" : None,
            "string" : None,
            None : None
        },
        None : {
            "int" : None,
            "float" : None,
            "string" : None,
            None : None
        }
    },
    ">" : {
        "int" : {
            "int" : "int",
            "float" : "int",
            "string" : None,
            None : None
        },
        "float" : {
            "int" : "int",
            "float" : "int",
            "string" : None,
            None : None
        },
        "string" : {
            "int" : None,
            "float" : None,
            "string" : "int",
            None : None
        },
        None : {
            "int" : None,
            "float" : None,
            "string" : None,
            None : None
        }
    },
    ">=" : {
        "int" : {
            "int" : "int",
            "float" : "int",
            "string" : None,
            None : None
        },
        "float" : {
            "int" : "int",
            "float" : "int",
            "string" : None,
            None : None
        },
        "string" : {
            "int" : None,
            "float" : None,
            "string" : "int",
            None : None
        },
        None : {
            "int" : None,
            "float" : None,
            "string" : None,
            None : None
        }
    },
    "<" : {
        "int" : {
            "int" : "int",
            "float" : "int",
            "string" : None,
            None : None
        },
        "float" : {
            "int" : "int",
            "float" : "int",
            "string" : None,
            None : None
        },
        "string" : {
            "int" : None,
            "float" : None,
            "string" : "int",
            None : None
        },
        None : {
            "int" : None,
            "float" : None,
            "string" : None,
            None : None
        }
    },
    "<=" : {
        "int" : {
            "int" : "int",
            "float" : "int",
            "string" : None,
            None : None
        },
        "float" : {
            "int" : "int",
            "float" : "int",
            "string" : None,
            None : None
        },
        "string" : {
            "int" : None,
            "float" : None,
            "string" : "int",
            None : None
        },
        None : {
            "int" : None,
            "float" : None,
            "string" : None,
            None : None
        }
    },
    "==" : {
        "int" : {
            "int" : "int",
            "float" : "int",
            "string" : None,
            None : None
        },
        "float" : {
            "int" : "int",
            "float" : "int",
            "string" : None,
            None : None
        },
        "string" : {
            "int" : None,
            "float" : None,
            "string" : "int",
            None : None
        },
        None : {
            "int" : None,
            "float" : None,
            "string" : None,
            None : None
        }
    },
    "!=" : {
        "int" : {
            "int" : "int",
            "float" : "int",
            "string" : None,
            None : None
        },
        "float" : {
            "int" : "int",
            "float" : "int",
            "string" : None,
            None : None
        },
        "string" : {
            "int" : None,
            "float" : None,
            "string" : "int",
            None : None
        },
        None : {
            "int" : None,
            "float" : None,
            "string" : None,
            None : None
        }
    },
    "=" : {
        "int" : {
            "int" : "int",
            "float" : "int",
            "string" : None,
            None : None
        },
        "float" : {
            "int" : "float",
            "float" : "float",
            "string" : None,
            None : None
        },
        "string" : {
            "int" : None,
            "float" : None,
            "string" : "string",
            None : None
        },
        None : {
            "int" : None,
            "float" : None,
            "string" : None,
            None : None
        }
    }
}

