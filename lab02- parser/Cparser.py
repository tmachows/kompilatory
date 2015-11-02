#!/usr/bin/python

from scanner import Scanner
import AST

	
class Cparser(object):

    def __init__(self):
        self.scanner = Scanner()
        self.scanner.build()
		self.no_error = True

    tokens = Scanner.tokens


    precedence = (
       ("nonassoc", 'IFX'),
       ("nonassoc", 'ELSE'),
       ("right", '='),
       ("left", 'OR'),
       ("left", 'AND'),
       ("left", '|'),
       ("left", '^'),
       ("left", '&'),
       ("nonassoc", '<', '>', 'EQ', 'NEQ', 'LE', 'GE'),
       ("left", 'SHL', 'SHR'),
       ("left", '+', '-'),
       ("left", '*', '/', '%'),
    )


    def p_error(self, p):
		self.no_error = False
        if p:
            print("Syntax error at line {0}, column {1}: LexToken({2}, '{3}')".format(p.lineno, self.scanner.find_tok_column(p), p.type, p.value))
        else:
            print("Unexpected end of input")

    
    def p_program(self, p):
        """program : declarations fundefs instructions"""
        p[0] = AST.Program(p[1], p[2], p[3])
		p[0].line = self.scanner.lexer.lineno
		if self.no_error:
			p[0].print_tree(0)
			pass

			
    def p_declarations(self, p):
        """declarations : declarations declaration
                        | """
        if self.no_error:
            if len(p) > 1:
				p[0] = AST.Declarations(p[1], p[2])
			else:
				p[0] = AST.Declarations(None, None)
			p[0].line = self.scanner.lexer.lineno
    				   
	def p_declaration(self, p):
        """declaration : TYPE inits ';' 
                       | error ';' """
		if len(p) > 3:
			p[0] = AST.Declaration(p[1], p[2], None)
		else:
			p[0] = AST.Declaration(None, None, p[1])
		p[0].line = self.scanner.lexer.lineno
					   
					   
    def p_inits(self, p):
        """inits : inits ',' init
                 | init """
		if len(p) > 2:
			p[0] = AST.Inits(p[1], p[3])
		else:
			p[0] = AST.Inits(None, p[1])
		p[0].line = self.scanner.lexer.lineno

    def p_init(self, p):
        """init : ID '=' expression """
		p[0] = AST.Init(p[1], p[3])
		p[0].line = self.scanner.lexer.lineno
		
    
    def p_instructions(self, p):
        """instructions : instructions instruction
                        | instruction """
        if len(p) > 2:
			p[0] = AST.Instructions(p[1], p[2])
		else:
			p[0] = AST.Instructions(None, p[1])
		p[0].line = self.scanner.lexer.lineno 
    
    def p_instruction(self, p):
        """instruction : print_instr
                       | labeled_instr
                       | assignment
                       | choice_instr
                       | while_instr 
                       | repeat_instr 
                       | return_instr
                       | break_instr
                       | continue_instr
                       | compound_instr
                       | expression ';' """
        if no_error:
            p[0] = p[1]
			p[0].line = self.scanner.lexer.lineno
    
	
    def p_print_instr(self, p):
        """print_instr : PRINT expression ';'
                       | PRINT error ';' """
		if isinstance(p[2], AST.Expression):
			p[0] = AST.Print(p[2], None)
		else:
			p[0] = AST.Print(None, p[2])
		p[0].line = self.scanner.lexer.lineno
		
	def p_labeled_instr(self, p):
        """labeled_instr : ID ':' instruction """
		p[0] = AST.Labeled(p[1], p[3])
		p[0].line = self.scanner.lexer.lineno
  
# ##### do tego miejsca juz jest raczej ok, ponizej mozna modyfikowac
  
    
    
    def p_assignment(self, p):
        """assignment : ID '=' expression ';' """
    
    
    def p_choice_instr(self, p):
        """choice_instr : IF '(' condition ')' instruction  %prec IFX
                        | IF '(' condition ')' instruction ELSE instruction
                        | IF '(' error ')' instruction  %prec IFX
                        | IF '(' error ')' instruction ELSE instruction """
        if no_error:
            if len(p) == 6:
                p[0] = AST.Choice(p[3], p[5])
            else:
                p[0] = AST.Choice(p[3], p[5], p[7])

    
    def p_while_instr(self, p):
        """while_instr : WHILE '(' condition ')' instruction
                       | WHILE '(' error ')' instruction """
        if no_error:
             p[0] = AST.While(p[3], p[5])

    def p_repeat_instr(self, p):
        """repeat_instr : REPEAT instructions UNTIL condition ';' """
    
    
    def p_return_instr(self, p):
        """return_instr : RETURN expression ';' """

    
    def p_continue_instr(self, p):
        """continue_instr : CONTINUE ';' """

    
    def p_break_instr(self, p):
        """break_instr : BREAK ';' """
    
    
    def p_compound_instr(self, p):
        """compound_instr : '{' declarations instructions_opt '}' """

    
    def p_condition(self, p):
        """condition : expression"""
		if no_error:
			p[0] = AST.Condition(p[1], p[2], p[3])

    def p_const(self, p):
        """const : INTEGER
                 | FLOAT
                 | STRING"""
        if no_error:             
            arg_type = ""
            try:
                int(p[1])
                arg_type = "int"
            except ValueError:
                try:
                    float(p[1])
                    arg_type = "float"
                except ValueError:
                    arg_type = "string"
            p[0] = AST.Const(arg_type, p[1])
    
    def p_expression(self, p):
        """expression : const
                      | ID
                      | expression '+' expression
                      | expression '-' expression
                      | expression '*' expression
                      | expression '/' expression
                      | expression '%' expression
                      | expression '|' expression
                      | expression '&' expression
                      | expression '^' expression
                      | expression AND expression
                      | expression OR expression
                      | expression SHL expression
                      | expression SHR expression
                      | expression EQ expression
                      | expression NEQ expression
                      | expression '>' expression
                      | expression '<' expression
                      | expression LE expression
                      | expression GE expression
                      | '(' expression ')'
                      | '(' error ')'
                      | ID '(' expr_list_or_empty ')'
                      | ID '(' error ')' """
        if no_error:
            if len(p) == 4:
                if p[1] == "(":
                    p[0] = AST.ExpressionInBrackets(p[2])
                else:
                    p[0] = AST.Expression(p[1], p[2], p[3])
            else:
                if isinstance(p[1], AST.Const):
                    p[0] = AST.OneArgExpression(p[1])
                else:
                    if p[1] not in IDs:
                        print_error("ERROR: Zmienna niezdefiniowana: " + p[1])
                    p[0] = AST.OneArgExpression(AST.Variable(IDs[p[1]], p[1]))
    
    def p_expr_list_or_empty(self, p):
        """expr_list_or_empty : expr_list
                              | """

    
    def p_expr_list(self, p):
        """expr_list : expr_list ',' expression
                     | expression """
    
    
    def p_fundefs_opt(self, p):
        """fundefs_opt : fundefs
                       | """

    def p_fundefs(self, p):
        """fundefs : fundefs fundef
                   | fundef """

          
    def p_fundef(self, p):
        """fundef : TYPE ID '(' args_list_or_empty ')' compound_instr """
    
    
    def p_args_list_or_empty(self, p):
        """args_list_or_empty : args_list
                              | """
    
    def p_args_list(self, p):
        """args_list : args_list ',' arg 
                     | arg """
    
    def p_arg(self, p):
        """arg : TYPE ID """


    