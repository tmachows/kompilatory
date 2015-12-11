import AST
import SymbolTable
from Memory import *
from Exceptions import  *
from visit import *
import sys

sys.setrecursionlimit(10000)

class Interpreter(object):

    def __init__(self):
        self.memoryStack = MemoryStack()
        self.declaredType = None

    @on('node')
    def visit(self, node):
        pass
        
        
    @when(AST.Program)
    def visit(self, node):
        node.blocks.accept(self)
        
    @when(AST.Blocks)
    def visit(self, node):
        for block in node.blocks:
            block.accept(self)
            
        
    @when(AST.Declaration)
    def visit(self, node):
        self.declaredType = node.type
        for init in node.inits:
            init.accept(self)
            
    @when(AST.Declarations)
    def visit(self, node):
        for declaration in node.declarations:
            declaration.accept(self)

            
    @when(AST.Init)
    def visit(self, node):
        expr_val = node.expression.accept(self)
        self.memoryStack.insert(node.id, expr_val)
        return expr_val
        
    @when(AST.Inits)
    def visit(self, node):
        for init in node.inits:
            init.accept(self)
        
    @when(AST.Instructions)
    def visit(self, node):
        for instruction in node.instructions:
            instruction.accept(self)
            
    
    @when(AST.BinExpr)
    def visit(self, node):
        r1 = node.left.accept(self)
        r2 = node.right.accept(self)
        return eval("a" + node.operator + "b", {"a": r1, "b": r2})
        # try sth smarter than:
        # if(node.op=='+') return r1+r2
        # elsif(node.op=='-') ...
        # but do not use python eval

    @when(AST.RelOp)
    def visit(self, node):
        r1 = node.left.accept(self)
        r2 = node.right.accept(self)
        # ...

    @when(AST.Assignment)
    def visit(self, node):
        expr_accept = node.expression.accept(self)
        self.memoryStack.set(node.id, expr_accept)
        return expr_accept
        
    @when(AST.Choice)
    def visit(self, node):
        # ???
        if node._if.accept(self) is not True:
            if node._else is not None:
                node._else.accept(self)
                
    @when(AST.If)
    def visit(self, node):
        if node.cond.accept(self):
            return node.statement.accept(self)
        else:
            pass

    @when(AST.Const)
    def visit(self, node):
        return node.value

        
    @when(AST.WhileInstr)
    def visit(self, node):
        r = None
        while node.cond.accept(self):
            r = node.body.accept(self)
        return r
        
    @when(AST.While)
    def visit(self, node):
        while node.cond.accept(self):
            try:
                node.statement.accept(self)
            except BreakException:
                break
            except ContinueException:
                pass
                
                
    @When(AST.RepeatUntil)
    def visit(self,node):
        while True:
            try:
                node.statement.accept(self)
                if node.cond.accept(self):
                    break
            except BreakException:
                break
            except ContinueException:
                pass
                
    @when(AST.Compound)
    def visit(self, node):
        node.blocks.accept(self)

    @when(AST.Break)
    def visit(self, node):
        raise BreakException()

    @when(AST.Continue)
    def visit(self, node):
        raise ContinueException()
    
    @when(AST.Return)
    def visit(self, node):
        value = node.expression.accept(self)
        raise ReturnValueException(value)
    
    @when(AST.ArgumentList)
    def visit(self, node):
        for arg in node.arg_list:
            arg.accept(self)
            
    @when(AST.Argument)
    def visit(self, node):
        return node.id
        
    @when(AST.ExpressionList)
    def visit(self, node):
        for expression in node.expressions:
            expression.accept(self)

    @when(AST.Labeled)
    def visit(self, node):
        node.instruction.accept(self)
    
    @when(AST.Print)
    def visit(self, node):
        print node.expression.accept(self)