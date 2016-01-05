package simplifier

import AST._


// to implement
// avoid one huge match of cases
// take into account non-greedy strategies to resolve cases with power laws
object Simplifier {
  var flag = false


  def simplify(node: Node): Node = {
    node match {
      case n: NodeList => SimplifyNodeList(n)

      case n: Tuple => SimplifyTuple(n)
      case n: BinExpr => SimplifyBinExpr(n)
      case n: Unary => SimplifyUnary(n)
      case n: IfElseExpr => SimplifyIfElseExpr(n)
      case n: Assignment => SimplifyAssignment(n)
      case n: KeyDatum => SimplifyKeyDatum(n)
      case n: KeyDatumList => SimplifyKeyDatumList(n)
      case FunCall(name, arg_list) => SimplifyFunCall(name, arg_list)
      case n: ReturnInstr => ReturnInstr(simplify(n.expr))
      case PrintInstr(expr) => PrintInstr(simplify(expr))
      case FunDef(name, formal_args, body) => SimplifyFunDef(name, formal_args, body)
      case n: WhileInstr => SimplifyWhile(n)
      case n: IfInstr => SimplifyIfInstr(n)
      case n: IfElseInstr => SimplifyIfElseInstr(n)
      case n: IfElifInstr => SimplifyIfElifInstr(n)
      case n: IfElifElseInstr => SimplifyIfElifElseInstr(n)

      case _ => node
    }
  }

  def SimplifyIfElifElseInstr(n: IfElifElseInstr): IfElifElseInstr = {
    IfElifElseInstr(simplify(n.cond), simplify(n.left), n.elifs.map({ case IfInstr(c, l) => IfInstr(simplify(c), simplify(l)) }), simplify(n.right))
  }

  def SimplifyIfElifInstr(n: IfElifInstr): IfElifInstr = {
    IfElifInstr(simplify(n.cond), simplify(n.left), n.elifs map (_ => IfInstr(simplify(n.cond), simplify(n.left))))
  }

  def SimplifyFunDef(name: String, formal_args: Node, body: Node): FunDef = {
    FunDef(name, simplify(formal_args), simplify(body))
  }

  def SimplifyFunCall(name: Node, arg_list: Node): FunCall = {
    FunCall(name, simplify(arg_list))
  }

  def SimplifyKeyDatumList(n: KeyDatumList): Node = {
    n.list map simplify
    processDict(n)
  }

  def SimplifyKeyDatum(n: KeyDatum): KeyDatum = {
    new KeyDatum(simplify(n.key), simplify(n.value))
  }

  def SimplifyTuple(n: Tuple): Tuple = {
    Tuple(n.list map simplify)
  }

  def SimplifyNodeList(n: NodeList): NodeList = {

    NodeList(n.list map simplify filter (_ != null)) match {
      case NodeList(Nil) => null
      case NodeList(List(NodeList(x))) => NodeList(x)
      case NodeList(nodes) => NodeList(remove_dead_assignments(nodes))
    }

  }

  def process_cummutative(expr: BinExpr): BinExpr = {
    val commutative: List[String] = List("+", "*", "and", "or", "!=", "==")

    def collect_nodes(expr: BinExpr, op: String): List[Node] = {
      if(!(expr.op equals op)) return List(null)

      expr match {
        case BinExpr(_, left: BinExpr, right: BinExpr) =>
          collect_nodes(left, op) ::: collect_nodes(right, op)

        case BinExpr(_, left: Node, right: BinExpr) =>
          left :: collect_nodes(right, op)

        case BinExpr(_, left: BinExpr, right: Node) =>
          right :: collect_nodes(left, op)

        case BinExpr(_, left: Node, right: Node) =>
          right :: left :: Nil
      }
    }

    if(!(commutative contains expr.op)) return expr

    var nodes = collect_nodes(expr, expr.op)
    if(nodes contains null) return expr
    nodes = nodes.sortBy(_.toStr)

    nodes match {
      case x :: y :: tail =>
        tail.foldLeft(BinExpr(expr.op, x, y)) ((l, r) => BinExpr(expr.op, l, r))
    }
  }

  def equals_commutative(expr1: BinExpr, expr2: BinExpr): Boolean = {
    val commutative: List[String] = List("+", "*", "and", "or", "!=", "==")
    if(!(commutative contains expr1.op) || !(expr1.op equals expr2.op)) return false

    (expr1, expr2) match {
      case (BinExpr(_, l1, r1), BinExpr(_, l2, r2))
      => (l1 equals l2) && (r1 equals r2) ||
        (l1 equals r2) && (l2 equals r1)

    }
  }

  def SimplifyBinExpr(expr: BinExpr): Node = {


    (expr.op, expr.left, expr.right) match {


      case ("/", l: BinExpr, r: BinExpr) if equals_commutative(l, r) => IntNum(1)
      case ("/", l: Node, r: Node) if simplify(l) equals simplify(r) => IntNum(1)

      // x*1/y = 1
      case ("*", x, BinExpr("/", IntNum(1), y)) => simplify(BinExpr("/", x, y))

      // Wyrazenie numeryczne
      case (op, _: Num, _: Num) =>
        (op, expr.left, expr.right) match {
          case ("+", l: FloatNum, r: FloatNum) => FloatNum(l.value + r.value)
          case ("+", l: FloatNum, r: IntNum) => FloatNum(l.value + r.value)
          case ("+", l: IntNum, r: FloatNum) => FloatNum(l.value + r.value)
          case ("+", l: IntNum, r: IntNum) => IntNum(l.value + r.value)

          case ("**", l: FloatNum, r: FloatNum) => FloatNum(math.pow(l.value, r.value))
          case ("**", l: FloatNum, r: IntNum) => FloatNum(math.pow(l.value, r.value * 1.0))
          case ("**", l: IntNum, r: FloatNum) => FloatNum(math.pow(l.value * 1.0, r.value))
          case ("**", l: IntNum, r: IntNum) => IntNum(math.pow(l.value * 1.0, r.value * 1.0).toInt)

          case ("-", l: FloatNum, r: FloatNum) => FloatNum(l.value - r.value)
          case ("-", l: FloatNum, r: IntNum) => FloatNum(l.value - r.value)
          case ("-", l: IntNum, r: FloatNum) => FloatNum(l.value - r.value)
          case ("-", l: IntNum, r: IntNum) => IntNum(l.value - r.value)

          case ("*", l: FloatNum, r: FloatNum) => FloatNum(l.value * r.value)
          case ("*", l: FloatNum, r: IntNum) => FloatNum(l.value * r.value)
          case ("*", l: IntNum, r: FloatNum) => FloatNum(l.value * r.value)
          case ("*", l: IntNum, r: IntNum) => IntNum(l.value * r.value)

          case ("/", l: FloatNum, r: FloatNum) => FloatNum(l.value / r.value)
          case ("/", l: FloatNum, r: IntNum) => FloatNum(l.value / r.value)
          case ("/", l: IntNum, r: FloatNum) => FloatNum(l.value / r.value)
          case ("/", l: IntNum, r: IntNum) => IntNum(l.value / r.value)

          case ("%", l: IntNum, r: IntNum) => IntNum(l.value % r.value)

          case _ => expr
        }

      // Przy dodawaniu i mnozeniu zawsze daje (liczba */+ zmienna)
      case (op, l: Node, r: Num) =>
        (op, l, r) match {
          case ("+", node, FloatNum(0.0)) => simplify(node)
          case ("+", node, IntNum(0)) => simplify(node)
          case ("+", node, num) => BinExpr(op, num, simplify(node))

          case ("-", node, FloatNum(0.0)) => simplify(node)
          case ("-", node, IntNum(0)) => simplify(node)
          case ("-", node, num) => BinExpr(op, simplify(node), num)

          case ("/", node, IntNum(1)) => simplify(node)
          case ("/", node, FloatNum(1.0)) => simplify(node)
          case ("/", node, num) => BinExpr(op, simplify(node), num)

          case ("*", node, IntNum(1)) => simplify(node)
          case ("*", node, IntNum(0)) => IntNum(0)
          case ("*", node, FloatNum(1.0)) => simplify(node)
          case ("*", node, FloatNum(0.0)) => FloatNum(0.0)
          case ("*", node, num) => BinExpr(op, num, simplify(node))

          case ("**", node, FloatNum(1.0)) => simplify(node)
          case ("**", node, FloatNum(0.0)) => FloatNum(1.0)
          case ("**", node, IntNum(1)) => simplify(node)
          case ("**", node, IntNum(0)) => IntNum(1)
          case ("**", BinExpr("+", x, y),IntNum(2)) => simplify(BinExpr("+",BinExpr("+",BinExpr("**", x, IntNum(2)),BinExpr("*",BinExpr("*",IntNum(2),x),y)),BinExpr("**", y, IntNum(2))))
          case ("**", _, _) => BinExpr(op, simplify(l), r)

          case ("==", node, num) => BinExpr(op, node, num)
          case ("!=", node, num) => BinExpr(op, node, num)
          case (">=", node, num) => BinExpr(op, node, num)
          case ("<=", node, num) => BinExpr(op, node, num)
          case (">", node, num) => BinExpr(op, node, num)
          case ("<", node, num) => BinExpr(op, node, num)

          case _ => simplify(BinExpr(op, simplify(l), r))
        }

      // Przy dodawaniu i mnozeniu zawsze daje (liczba */+ zmienna)
      case (op, l: Num, r: Node) =>
        (op, l, r) match {
          case ("+", FloatNum(0.0), node) => simplify(node)
          case ("+", IntNum(0), node) => simplify(node)
          case ("+", num, node) => simplify(BinExpr(op, num, simplify(node)))

          case ("-", FloatNum(0.0), node) => Unary("-", simplify(node))
          case ("-", IntNum(0), node) => Unary("-", simplify(node))
          case ("-", num, node) => simplify(BinExpr(op, num, simplify(node)))

          case ("/", IntNum(1), BinExpr("/", IntNum(1), x)) => simplify(x)
          case ("/", IntNum(1), node) => simplify(node)
          case ("/", FloatNum(1.0), node) => simplify(node)
          case ("/", num, node) => BinExpr(op, num, simplify(node))


          case ("*", IntNum(1), node) => node
          case ("*", IntNum(0), node) => IntNum(0)
          case ("*", FloatNum(1.0), node) => node
          case ("*", FloatNum(0.0), node) => FloatNum(0.0)
          case ("*", num, node) => simplify(BinExpr(op, num, simplify(node)))

          case ("**", FloatNum(1.0), node) => FloatNum(1.0)
          case ("**", FloatNum(0.0), node) => FloatNum(0.0)
          case ("**", IntNum(1), node) => IntNum(1)
          case ("**", IntNum(0), node) => IntNum(0)
          case ("**", num, node) => simplify(BinExpr(op, num, simplify(node)))

          case ("==", num, node) => BinExpr(op, num, node)
          case ("!=", num, node) => BinExpr(op, num, node)
          case (">=", num, node) => BinExpr(op, num, node)
          case ("<=", num, node) => BinExpr(op, num, node)
          case (">", num, node) => BinExpr(op, num, node)
          case ("<", num, node) => BinExpr(op, num, node)

          case _ => simplify(BinExpr(op, l, simplify(r)))
        }

      // A-A = 0
      case ("-", x, y) if x equals y => IntNum(0)

      // operacje binarne
      case ("and", x: Node, _: TrueConst) => x
      case ("and", _: TrueConst, x: Node) => x
      case ("and", x: Node, _: FalseConst) => new FalseConst
      case ("and", _: FalseConst, x: Node) => new FalseConst

      case ("or", _: Node, _: TrueConst) => new TrueConst
      case ("or", _: TrueConst, _: Node) => new TrueConst

      case ("or", _: FalseConst, x: Node) => x
      case ("or", x: Node, _: FalseConst) => x

      case ("and", x: Node, y: Node) if x equals y => x
      case ("or", x: Node, y: Node) if x equals y => x

      case (">", l: Variable, r: Variable) if l.name equals r.name => new FalseConst
      case ("<", l: Variable, r: Variable) if l.name equals r.name => new FalseConst
      case ("<=", l: Variable, r: Variable) if l.name equals r.name => new TrueConst
      case (">=", l: Variable, r: Variable) if l.name equals r.name => new TrueConst
      case ("==", l: Variable, r: Variable) if l.name equals r.name => new TrueConst
      case ("!=", l: Variable, r: Variable) if l.name equals r.name => new FalseConst

      // -x+y = y-x, y+-x=y-x
      case ("+", Unary("-", x), y) => simplify(BinExpr("-", y, x))
      case ("+", y, Unary("-", x)) => simplify(BinExpr("-", y, x))

      // upraszczanie map, list
      case ("+", ElemList(l), ElemList(r)) => ElemList((l map simplify) ::: (r map simplify))
      case ("+", Tuple(l), Tuple(r)) => Tuple((l map simplify) ::: (r map simplify))

      // x*y+z*y = y*(x+z)
      case ("+",BinExpr("*", x1, y1),BinExpr("*", x2, y2)) if y1 equals y2 => simplify(BinExpr("*",y1, BinExpr("+", x2, x1)))
      case ("-",BinExpr("*", x1, y1),BinExpr("*", x2, y2)) if y1 equals y2 => simplify(BinExpr("*",y1, BinExpr("+", x2, x1)))

      // x*y+x*z = x*(y+z)
      case ("+",BinExpr("*", x1, y1),BinExpr("*", x2, y2)) if x1 equals x2 => simplify(BinExpr("*",x1, BinExpr("+", y2, y1)))
      case ("-",BinExpr("*", x1, y1),BinExpr("*", x2, y2)) if x1 equals x2 => simplify(BinExpr("*",x1, BinExpr("+", y2, y1)))

      // 2*x + 3x = 5x
      case ("+", BinExpr("*", a1, b), BinExpr("*", a2, c)) if a1 equals a2 => simplify(BinExpr("*", a1, simplify(BinExpr("+", b, c))))
      case ("-", BinExpr("*", a1, b), BinExpr("*", a2, c)) if a1 equals a2 => simplify(BinExpr("-", a1, simplify(BinExpr("+", b, c))))
      case ("+", BinExpr("*", b, a1), BinExpr("*", a2, c)) if a1 equals a2 => simplify(BinExpr("*", a1, simplify(BinExpr("+", b, c))))
      case ("-", BinExpr("*", b, a1), BinExpr("*", a2, c)) if a1 equals a2 => simplify(BinExpr("-", a1, simplify(BinExpr("+", b, c))))
      // 2*x + x = 3*x
      case ("+", BinExpr("*", a1, b), a2) if a1 equals a2 => simplify(BinExpr("*", a1, simplify(BinExpr("+", b, IntNum(1)))))
      case ("-", BinExpr("*", a1, b), a2) if a1 equals a2 => simplify(BinExpr("*", a1, simplify(BinExpr("-", b, IntNum(1)))))
      case ("-", BinExpr("*", b, a1), a2) if a1 equals a2 => simplify(BinExpr("*", a1, simplify(BinExpr("-", b, IntNum(1)))))
      case ("+", a2, BinExpr("*", a1, b)) if a1 equals a2 => simplify(BinExpr("*", a1, simplify(BinExpr("+", b, IntNum(1)))))
      case ("-", a2, BinExpr("*", a1, b)) if a1 equals a2 => simplify(BinExpr("*", a1, simplify(BinExpr("-", b, IntNum(1)))))
      case ("-", BinExpr("*", a1, b), a2) if a1 equals a2 => simplify(BinExpr("*", a1, BinExpr("-", b, IntNum(1))))
      case ("-", a2, BinExpr("*", a1, b)) if a1 equals a2 => simplify(BinExpr("*", a1, BinExpr("-", b, IntNum(1))))

      // x + 2*x = 3*x
      case ("+", a1, BinExpr("*", a2, b)) if a1 equals a2 => simplify(BinExpr("*", a1, BinExpr("+", IntNum(1), b)))
      case ("-", a1, BinExpr("*", a2, b)) if a1 equals a2 => simplify(BinExpr("*", a1, BinExpr("-", IntNum(1), b)))
      case ("+", a1, BinExpr("*", b, a2)) if a1 equals a2 => simplify(BinExpr("*", a1, BinExpr("+", IntNum(1), b)))
      case ("-", a1, BinExpr("*", b, a2)) if a1 equals a2 => simplify(BinExpr("*", a1, BinExpr("-", IntNum(1), b)))

      // x*y+x*z+v*y+v*z = (x+v)*(y+z)
      case ("+", BinExpr("+", BinExpr("+", BinExpr("*", x1 ,y1), BinExpr("*", x2, z1)), BinExpr("*", v1, y2)), BinExpr("*", v2, z2))
        if (x1 equals x2) && (y1 equals y2) && (v1 equals v2) =>
        simplify(BinExpr("*", BinExpr("+", x1, v1), BinExpr("+", y1, z1)))

      case ("*", BinExpr("**", a1, b), BinExpr("**", a2, c)) if a1 equals a2 => simplify(BinExpr("**", a1, BinExpr("+", b, c)))
      case ("**", BinExpr("**", a, b), c) => simplify(BinExpr("**", a, BinExpr("*", b, c)))

      case ("+", BinExpr("+", BinExpr("**", x1, IntNum(2)), BinExpr("*", BinExpr("*", IntNum(2), x2), y2)), BinExpr("**", y1, IntNum(2)))
        if (x1 equals x2) && (y1 equals y2) =>
        simplify(BinExpr("**", BinExpr("+", x1, y1), IntNum(2)))


      case (op, l: BinExpr, r: BinExpr) => {
        (simplify(l), simplify(r)) match {
          case (l_simple, r_simple) if (l_simple equals l) && (r_simple equals r) => BinExpr(op, l_simple, r_simple)
          case (l_simple, r_simple) => simplify(BinExpr(op, simplify(l), simplify(r)))
        }
      }


      // default
      case _ => expr //TODO inne rodzaje BinaryExpression do zaimplementowania
    }
  }

  def processDict(dict: KeyDatumList): Node = {
    val elems = scala.collection.mutable.Map[Node, Node]()
    for (key_val <- dict.list) {
      elems.put(key_val.key, key_val.value)
    }
    KeyDatumList((for ((key, value) <- elems) yield KeyDatum(key, value)).toList)
  }

  def SimplifyAssignment(assignment: Assignment): Node = {
    assignment match {
      case Assignment(left, right) if left.toStr == right.toStr => null
      case Assignment(left, right) => new Assignment(assignment.left, simplify(assignment.right))
    }
  }

  def SimplifyUnary(unary: Unary): Node = {
    (unary.op, unary.expr) match {
      case ("not", BinExpr("==", Variable(x), Variable(y))) => BinExpr("!=", Variable(x), Variable(y))
      case ("not", BinExpr("!=", Variable(x), Variable(y))) => BinExpr("==", Variable(x), Variable(y))
      case ("not", BinExpr(">", Variable(x), Variable(y))) => BinExpr("<=", Variable(x), Variable(y))
      case ("not", BinExpr("<", Variable(x), Variable(y))) => BinExpr(">=", Variable(x), Variable(y))
      case ("not", BinExpr("<=", Variable(x), Variable(y))) => BinExpr(">", Variable(x), Variable(y))
      case ("not", BinExpr(">=", Variable(x), Variable(y))) => BinExpr("<", Variable(x), Variable(y))

      case ("not", FalseConst()) => TrueConst()
      case ("not", TrueConst()) => FalseConst()
      case ("not", Unary("not", x)) => simplify(x)

      case ("-", Unary("-", x)) => simplify(x)
      case ("-", FloatNum(x)) => FloatNum(-x)
      case ("-", IntNum(x)) => IntNum(-x)

      case _ => new Unary(unary.op, unary.expr)
    }
  }

  def SimplifyWhile(instr: WhileInstr): Node = {
    (simplify(instr.cond), simplify(instr.body)) match {
      case (FalseConst(), body) => null
      case e => WhileInstr(e._1, e._2)
    }
  }

  def SimplifyIfElseExpr(expr: IfElseExpr): Node = {
    (simplify(expr.cond), simplify(expr.left), simplify(expr.right)) match {
      case (TrueConst(), left, _) => left
      case (FalseConst(), _, right) => right
      case (cond, left, right) => IfElseExpr(cond, left, right)
    }
  }

  def SimplifyIfElseInstr(instr: IfElseInstr): Node = {
    (simplify(instr.cond), simplify(instr.left), simplify(instr.right)) match {
      case (TrueConst(), left, _) => left
      case (FalseConst(), _, right) => right
      case (cond, left, right) => IfElseInstr(cond, left, right)
    }
  }


  def SimplifyIfInstr(instr: IfInstr): Node = {
    (simplify(instr.cond), simplify(instr.left)) match {
      case (TrueConst(), expr) => expr
      case (FalseConst(), _) => null
      case (cond, expr) => IfInstr(cond, expr)
    }
  }

  def remove_dead_assignments(nodeList: List[Node]): List[Node] = {
    nodeList match {
      case Assignment(x, s) :: Assignment(y, t) :: Nil if x equals y =>
        Assignment(y, t) :: Nil

      case Assignment(x, _) :: Assignment(y, t) :: tail if x equals y =>
        remove_dead_assignments(Assignment(y, t) :: tail)

      case Assignment(x, s) :: Assignment(y, t) :: tail =>
        Assignment(x, s) :: remove_dead_assignments(Assignment(y, t) :: tail)

      case other => other
    }
  }
}