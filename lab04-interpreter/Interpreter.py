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
            
    @when(AST.Init)
    def visit(self, node):
        expr_val = node.expression.accept(self)
        self.memoryStack.insert(node.id, expr_val)
        return expr_val
        
        
    

    @when(AST.BinOp)
    def visit(self, node):
        r1 = node.left.accept(self)
        r2 = node.right.accept(self)
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
    #
    #

    @when(AST.Const)
    def visit(self, node):
        return node.value

    # simplistic while loop interpretation
    @when(AST.WhileInstr)
    def visit(self, node):
        r = None
        while node.cond.accept(self):
            r = node.body.accept(self)
        return r
