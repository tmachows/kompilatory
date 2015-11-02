import AST


def add_to_class(cls):
    def decorator(func):
        setattr(cls, func.__name__, func)
        return func

    return decorator


class TreePrinter:
    @classmethod
    def print_indented(cls, string, level):
        print "| " * level + string

    @add_to_class(AST.Node)
    def print_tree(self, indent):
        raise Exception("print_tree not defined in class " + self.__class__.__name__)

    @add_to_class(AST.Program)
    def print_tree(self, indent):
        self.blocks.print_tree(indent)
		
    @add_to_class(AST.Blocks)
    def print_tree(self, indent):
        for block in self.blocks:
            block.print_tree(indent)


    @add_to_class(AST.Declarations)
    def print_tree(self, indent):
        for declaration in self.declarations:
            declaration.print_tree(indent)

    @add_to_class(AST.Declaration)
    def print_tree(self, indent):
        if not self.error:
            TreePrinter.print_indented("DECL", indent)
            self.inits.print_tree(indent + 1)

    @add_to_class(AST.Inits)
    def print_tree(self, indent):
        for init in self.inits:
            init.print_tree(indent)

    @add_to_class(AST.Init)
    def print_tree(self, indent):
        TreePrinter.print_indented("=", indent)
        TreePrinter.print_indented(self.id, indent + 1)
        self.expression.print_tree(indent + 1)

    @add_to_class(AST.Id)
    def print_tree(self, indent):
        self.print_indented(self.id, indent)

    @add_to_class(AST.Instructions)
    def print_tree(self, indent):
        for instruction in self.instructions:
            instruction.print_tree(indent)

    @add_to_class(AST.Print)
    def print_tree(self, indent):
        TreePrinter.print_indented("PRINT", indent)
        self.expression.print_tree(indent + 1)

    @add_to_class(AST.Labeled)
    def print_tree(self, indent):
        TreePrinter.print_indented(self.id + ":", indent)
        self.instruction.print_tree(indent)

    @add_to_class(AST.Assignment)
    def print_tree(self, indent):
        TreePrinter.print_indented("=", indent)
        TreePrinter.print_indented(self.id, indent + 1)
        self.expression.print_tree(indent + 1)

    @add_to_class(AST.Choice)
    def print_tree(self, indent):
        self._if.print_tree(indent)
        if self._else:
            self._else.print_tree(indent)

    @add_to_class(AST.If)
    def print_tree(self, indent):
        TreePrinter.print_indented("IF", indent)
        self.cond.print_tree(indent + 1)
        self.statement.print_tree(indent + 1)

    @add_to_class(AST.Else)
    def print_tree(self, indent):
        TreePrinter.print_indented("ELSE", indent)
        self.statement.print_tree(indent + 1)

    @add_to_class(AST.While)
    def print_tree(self, indent):
        TreePrinter.print_indented("WHILE", indent)
        self.cond.print_tree(indent + 1)
        self.statement.print_tree(indent + 1)

    @add_to_class(AST.RepeatUntil)
    def print_tree(self, indent):
        TreePrinter.print_indented("REPEAT", indent)
        self.statement.print_tree(indent + 1)
        TreePrinter.print_indented("UNTIL", indent)
        self.cond.print_tree(indent + 1)

    @add_to_class(AST.Return)
    def print_tree(self, indent):
        TreePrinter.print_indented("RETURN", indent)
        self.expression.print_tree(indent + 1)

    @add_to_class(AST.Continue)
    def print_tree(self, indent):
        TreePrinter.print_indented("CONTINUE", indent)

    @add_to_class(AST.Break)
    def print_tree(self, indent):
        TreePrinter.print_indented("BREAK", indent)

    @add_to_class(AST.Compound)
    def print_tree(self, indent):
        self.blocks.print_tree(indent)

    @add_to_class(AST.Const)
    def print_tree(self, indent):
        TreePrinter.print_indented(self.value, indent)

    @add_to_class(AST.Id)
    def print_tree(self, indent):
        TreePrinter.print_indented(self.id, indent)

    @add_to_class(AST.BinExpr)
    def print_tree(self, indent):
        TreePrinter.print_indented(self.operator, indent)
        self.expr1.print_tree(indent + 1)
        self.expr2.print_tree(indent + 1)

    @add_to_class(AST.ExpressionInPar)
    def print_tree(self, indent):
        self.expression.print_tree(indent)

    @add_to_class(AST.IdWithPar)
    def print_tree(self, indent):
        TreePrinter.print_indented("FUNCALL", indent)
        TreePrinter.print_indented(self.id, indent + 1)
        self.expression_list.print_tree(indent + 1)

    @add_to_class(AST.ExpressionList)
    def print_tree(self, indent):
        for expression in self.expressions:
            expression.print_tree(indent)

    @add_to_class(AST.FunctionDefinitions)
    def print_tree(self, indent):
        for function in self.fundefs:
            function.print_tree(indent)

    @add_to_class(AST.FunctionDefinition)
    def print_tree(self, indent):
        TreePrinter.print_indented("FUNDEF", indent)
        TreePrinter.print_indented(self.id, indent + 1)
        TreePrinter.print_indented("RET " + self.type, indent + 1)
        self.arglist.print_tree(indent + 1)
        self.compound_instr.print_tree(indent + 1)

    @add_to_class(AST.ArgumentList)
    def print_tree(self, indent):
        for argument in self.arg_list:
            argument.print_tree(indent)

    @add_to_class(AST.Argument)
    def print_tree(self, indent):
        TreePrinter.print_indented("ARG " + self.id, indent)


