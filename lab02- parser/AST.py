
class Node(object):

    def __str__(self):
        return self.printTree()


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

class Print(Node):
    def __init__(self, expr):
        self.expr = expr
		
#????????????????????????????????????????????????????????????????????
class repeat-until(Node):
    def __init__(self, stmt,cond):
        self.cond = cond
        self.stmt = stmt
#????????????????????????????????????????????????????????????????????        
class Instructions(Node):
    def __init__(self, instr):
        self.instrs = [instr]
		
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

