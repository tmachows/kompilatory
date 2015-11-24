
class Symbol(object):

    def __init__(self, name, type):
        self.name, self.type = name, type
		
class VariableSymbol(Symbol):

    def __init__(self, name, type, value):
        super(VariableSymbol, self).__init__(name, type)
        self.value = value
        
class FunctionSymbol(Symbol):

    def __init__(self, name, type, arguments):
        super(FunctionSymbol, self).__init__(name, type)
        self.arguments = arguments


class SymbolTable(object):

    def __init__(self, parent, name, type): # parent scope and symbol table name
        self.parent, self.symbol, self.symbols, self.scopes = parent, Symbol(name, type), {}, []

    def put(self, name, symbol): # put variable symbol or fundef under <name> entry
        self.symbols[name] = symbol
    #

    def get(self, name): # get variable symbol or fundef from <name> entry
        return self.symbols[name]
    #

    def getParentScope(self):
        return self.parent
    #

    def pushScope(self, name):
        current = SymbolTable(self, name, self.symbol.type)
        self.scopes.append(current)
        return current
    #

    def popScope(self):
        if len(self.scopes) > 0:
            self.scopes.pop()
        return self.parent
    #

