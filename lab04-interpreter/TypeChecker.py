
from SymbolTable import SymbolTable, VariableSymbol, FunctionSymbol
import AST


ttype = {}
arithmetic_operators = ['+', '-', '*', '/', '%']
bitwise_operators = ['|', '&', '^', '<<', '>>']
logical_operators = ['&&', '||']
comparison_operators = ['==', '!=', '>', '<', '<=', '>=']
assignment_operators = ['=']

def all_operators():
    return arithmetic_operators + bitwise_operators + logical_operators + assignment_operators + comparison_operators


for operator in all_operators():
    ttype[operator] = {}
    for type_ in ['int', 'float', 'string']:
        ttype[operator][type_] = {}

for arithmetic_operator in arithmetic_operators:
    ttype[arithmetic_operator]['int']['int'] = 'int'
    ttype[arithmetic_operator]['int']['float'] = 'float'
    ttype[arithmetic_operator]['float']['int'] = 'float'
    ttype[arithmetic_operator]['float']['float'] = 'float'
ttype['+']['string']['string'] = 'string'
ttype['*']['string']['int'] = 'string'
ttype['=']['float']['int'] = 'float'
ttype['=']['float']['float'] = 'float'
ttype['=']['int']['int'] = 'int'
ttype['=']['string']['string'] = 'string'
ttype['=']['int']['float'] = ('int', 'warn')

for operator in bitwise_operators + logical_operators:
    ttype[operator]['int']['int'] = 'int'

for comp_op in comparison_operators:
    ttype[comp_op]['int']['int'] = 'int'
    ttype[comp_op]['int']['float'] = 'int'
    ttype[comp_op]['float']['int'] = 'int'
    ttype[comp_op]['float']['float'] = 'int'
    ttype[comp_op]['string']['string'] = 'int'

    
    
class NodeVisitor(object):

    def visit(self, node, *args):
        method = 'visit_' + node.__class__.__name__
        visitor = getattr(self, method, self.generic_visit)
        return visitor(node, *args)


    def generic_visit(self, node, *args):        # Called if no explicit visitor function exists for a node.
        if isinstance(node, list):
            for elem in node:
                self.visit(elem, *args)
        else:
            if node is None:
                pass
            else:
                for child in node.children:
                    if isinstance(child, list):
                        for item in child:
                            if isinstance(item, AST.Node):
                                self.visit(item, *args)
                    elif isinstance(child, AST.Node):
                        self.visit(child, *args)

    # simpler version of generic_visit, not so general
    #def generic_visit(self, node):
    #    for child in node.children:
    #        self.visit(child)



class TypeChecker(NodeVisitor):


    def findVariable(self, tab, variable):
        if variable in tab.symbols:
            return tab.get(variable)
        elif tab.symbol.name == variable:
            return tab.symbol
        elif tab.getParentScope() is not None:
            return self.findVariable(tab.getParentScope(), variable)

    def visit_Program(self, node):
        tab = SymbolTable(None, "program", None)
        self.scope = tab
        self.visit(node.blocks, tab)
        
    def visit_Blocks(self, node, tab):
        for block in node.blocks:
            self.visit(block, tab)
            
    def visit_Block(self, node, tab):
        pass
        
    #def visit_Declarations(self, node, tab):
    #    for declaration in node.declarations:
    #        self.visit(declaration, tab)

    def visit_Declaration(self, node, tab):
        self.visit(node.inits, tab, node.type)

    def visit_Inits(self, node, tab, type):
        for init in node.inits:
            self.visit(init, tab, type)

    def visit_Init(self, node, tab, type):
        if node.id in tab.symbols:
            print "Error: Duplicated usage of symbol {0} in line {1}".format(node.id, node.line)

        value_type = self.visit(node.expression, tab)
        
        if not value_type in ttype['='][type]:
            print "Error: Value of type {0} cannot be assigned to symbol {1} of type {2} (line {3})" \
                .format(value_type, node.id, type, node.line)
        else:
            if "warn" in ttype['='][type][value_type]:
                print "Warning: Value of type {0} assigned to symbol {1} of type {2} (line {3})" \
                .format(value_type, node.id, type, node.line)
        
            
            tab.put(node.id, VariableSymbol(node.id, type, node.expression))

    def visit_Instructions(self, node, tab):
        for instruction in node.instructions:
            self.visit(instruction, tab)

    def visit_Instruction(self, node, tab):
        pass
        
    def visit_Print(self, node, tab):
        self.visit(node.expression, tab)

    def visit_Labeled(self, node, tab):
        self.visit(node.instruction, tab)
        
    def visit_Assignment(self, node, tab):
        variable = self.findVariable(tab, node.id)
        if variable is None:
            print "Error: Symbol {0} in line {1} not defined before".format(node.id, node.line)
        else:
            value_type = self.visit(node.expression, tab)
            if not value_type in ttype["="][variable.type]:
                print "Error: Value of type {0} cannot be assigned to symbol {1} of type {2} (line {3})" \
                    .format(value_type, node.id, variable.type, node.line)
            else:
                if "warn" in ttype["="][variable.type][value_type]:
                    print "Warning: Value of type {0} assigned to symbol {1} of type {2} (line {3})" \
                            .format(value_type, node.id, variable.type, node.line)
                return ttype["="][variable.type][value_type]
                
    def visit_Choice(self, node, tab):
        self.visit(node._if, tab)
        self.visit(node._if, tab)

    def visit_If(self, node, tab):
        self.visit(node.cond, tab)
        self.visit(node.statement, tab)

    def visit_Else(self, node, tab):
        self.visit(node.statement, tab)
        
    def visit_While(self, node, tab):
        self.visit(node.cond, tab)
        self.visit(node.statement, tab)

    def visit_RepeatUntil(self, node, tab):
        self.visit(node.cond, tab)
        self.visit(node.statement, tab)

    def visit_Return(self, node, tab):
        
        if not type(self.actFunc)==AST.FunctionDefinition:
            print "Return placed outside of a function in line {0}".format( node.line)
        else:
            rettype = self.visit(node.expression,tab)
            if rettype != self.actFunc.type and (self.actFunc.type != "float" or rettype != "int"):
                print "Invalid return type of {0} in line {1}. Expected {2}".format(rettype, node.line, self.actFunc.type)
   
        
        
    def visit_Continue(self, node, tab):
        if not type(self.actFunc)==AST.Compound:
            print "Continue placed outside of a loop in line {0}".format( node.line)
        

    def visit_Break(self, node, tab):
        if not type(self.actFunc)==AST.Compound:
            print "Break placed outside of a loop in line {0}".format( node.line)
        
        
    def visit_Compound(self, node, tab, *args):
        if len(args) > 0 and args[0] is True:
            self.visit(node.blocks, tab)
        else:
            #new_tab = SymbolTable(tab, None, None)
            #self.visit(node.blocks, new_tab)
            tab = tab.pushScope(node)
            self.actFunc = node
            self.visit(node.blocks, tab)
            self.actFunc = None
            tab = tab.popScope()

    def visit_Condition(self, node, tab):
        pass

    def visit_Expression(self, node, tab):
        pass

    def visit_Const(self, node, tab):
        value = node.value
        if (value[0] in ('"', "'")) and (value[len(value) - 1] in ('"', "'")):
            return 'string'
        try:
            int(value)
            return 'int'
        except ValueError:
            try:
                float(value)
                return 'float'
            except ValueError:
                print "Error: Value's {0} type is not recognized".format(value)    

    def visit_Id(self, node, tab):
        variable = self.findVariable(tab, node.id)
        if variable is None:
            print "Error: Symbol {0} in line {1} not declared before".format(node.id, node.line)
        else:
            return variable.type
            
    def visit_BinExpr(self, node, tab):                                         
        type1 = self.visit(node.expr1, tab)    
        type2 = self.visit(node.expr2, tab)    
        op    = node.operator;
        if type1 is None or not type2 in ttype[op][type1]:
            #print op
            #print type1
            #print type2
            print "Error: Incompatible types in line", node.line
        else:
            return ttype[op][type1][type2]
 
    def visit_ExpressionInPar(self, node, tab):
        expression = node.expression
        return self.visit(expression, tab)

    def visit_IdWithPar(self, node, tab):
        function = self.findVariable(tab, node.id)
        if function is None:
            print "Error: Function {0} in line {1} not declared before".format(node.id, node.line)
        else:
            if len(function.arguments.arg_list) != len(node.expression_list.expressions):
                print "Error: Wrong number of arguments in line {0}".format(node.line - 1)
            else:
                for i in range(len(function.arguments.arg_list)):
                    arg_type = function.arguments.arg_list[i].type
                    given_type = self.visit(node.expression_list.expressions[i], tab)
                    if not given_type in ttype['='][arg_type]:
                        print "Error: Incompatible types of argument in {0} call in line {1}".format(node.id, node.line)
                        return None
                    
            self.visit(node.expression_list, tab)
            return function.type
            
    def visit_ExpressionList(self, node, tab):
        for expression in node.expressions:
            self.visit(expression, tab)

    def visit_FunctionDefinitions(self, node, tab):
        for fundef in node.fundefs:
            self.visit(fundef, tab)

    def visit_FunctionDefinition(self, node, tab):
        fun_name = self.findVariable(tab, node.id)
        if not fun_name is None:
            print "Error: Symbol {0} in line {1} declared before".format(node.id, node.line)
        else:
            tab.put(node.id, FunctionSymbol(node.id, node.type, node.arglist))
            tab = tab.pushScope(node.id)
            self.actFunc = node
            self.visit(node.arglist, tab)
            self.visit(node.compound_instr, tab, True)
            self.actFunc = None
            tab = tab.popScope()
            

    def visit_ArgumentList(self, node, tab):
        for arg in node.arg_list:
            self.visit(arg, tab)

    def visit_Argument(self, node, tab):
        if node.id in tab.symbols:
                print "Error: Duplicated usage of symbol {0} in line {1}".format(node.id, node.line)
        else:
            tab.put(node.id, VariableSymbol(node.id, node.type, None))
            return node.type