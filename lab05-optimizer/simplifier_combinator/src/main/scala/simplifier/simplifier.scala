package simplifier

import AST._

import scala.math.pow

// to implement
// avoid one huge match of cases
// take into account non-greedy strategies to resolve cases with power laws
object Simplifier {

  def simplify(node: Node): Node = node match {
    // dopasowania - od bardziej szczegolowych, do bardziej ogolnych

    // konkatenacja krotek i list
    case BinExpr("+", Tuple(l1), Tuple(l2)) => Tuple((l1 ++ l2) map simplify)
    case BinExpr("+", ElemList(l1), ElemList(l2)) => ElemList((l1 ++ l2) map simplify)
    // upraszczanie slownikow
    case KeyDatumList(list) => SimplifyKeyDatumList(list)
    // upraszczanie konstrukcji
    case WhileInstr(cond, body) => SimlifyWhileInstr(cond, body)
    case IfElseInstr(cond, left, right) => SimplifyIfElseInstr(cond, left, right)
    case IfInstr(cond, left) => SimplifyIfInstr(cond, left)
    case IfElseExpr(cond, left, right) => SimplifyIfElseExpr(cond, left, right)
    case Assignment(Variable(x), expr) => SimplifyAssignment(x, expr)
    // ewaluacja wyrazen (jesli to mozliwe)
    case BinExpr(op, IntNum(x), IntNum(y)) => SimplifyBinExprIntInt(op, x, y)
    case BinExpr(op, FloatNum(x), FloatNum(y)) => SimplifyBinExprFloatFloat(op, x, y)
    case BinExpr(op, IntNum(x), FloatNum(y)) => SimplifyBinExprIntFloat(op, x, y)
    case BinExpr(op, FloatNum(x), IntNum(y)) => SimplifyBinExprFloatInt(op, x, y)
    case BinExpr("==", x, y)  if x == y => TrueConst()
    case BinExpr(">=", x, y)  if x == y => TrueConst()
    case BinExpr("<=", x, y)  if x == y => TrueConst()
    case BinExpr("!=", x, y)  if x == y => FalseConst()
    case BinExpr("<", x, y)   if x == y => FalseConst()
    case BinExpr(">", x, y)   if x == y => FalseConst()
    case BinExpr("or", x, y)  if x == y => x
    case BinExpr("and", x, y) if x == y => x
    // upraszczanie wyrazen unarnych
    case Unary("not", expr) => SimplifyUnaryNot(expr)
    case Unary("-", expr) => SimplifyUnary(expr)
    // balansowanie drzew:
    case (BinExpr("+", BinExpr("+", BinExpr("+", BinExpr("*", x1, y1), BinExpr("*", x2, y2)), BinExpr("*", x3, y3)), BinExpr("*", x4, y4)))
      => simplify(BinExpr("+", BinExpr("+", BinExpr("*", x1, y1), BinExpr("*", x2, y2)), BinExpr("+", BinExpr("*", x3, y3), BinExpr("*", x4, y4))))
    // upraszczanie wyrazen typu x + 0
    case BinExpr("-", left, right)   => SimplifyBinExprMinus(left, right)
    case BinExpr("+", left, right)   => SimplifyBinExprPlus(left, right)
    case BinExpr("*", left, right)   => SimplifyBinExprMul(left, right)
    case BinExpr("/", left, right)   => SimplifyBinExprDiv(left, right)
    case BinExpr("**", left, right)  => SimplifyBinExprPow(left, right)
    case BinExpr("and", left, right) => SimplifyBinExprAnd(left, right)
    case BinExpr("or", left, right)  => SimplifyBinExprOr(left, right)
    // upraszczanie listy nodow
    case NodeList(list) => SimplifyNodeList(list)
    // w pozostalych przypadkach nie da sie juz nic uproscic:
    case n => n
  }


  // jesli mamy liste node'ow, to upraszczamy kazdy element z osobna:
  def SimplifyNodeList(list: List[Node]): Node = {
    list match {
      case Nil => EmptyInstr()
      case (nl :: Nil) => nl match {
        case EmptyInstr() => EmptyInstr()
        case NodeList(l) => simplify(NodeList(l map simplify)) // zostawiamy tylko jeden layer node listow
        case n =>
          val sN = simplify(n)
          if (sN != n) simplify(NodeList(List(simplify(n)))) else NodeList(List(sN))
      }
      case _ =>
        val sList = (list map simplify).foldRight(List.empty[Node])(
          (n: Node, list2: List[Node]) => list2 match {
            case Nil => List(n)
            case x :: xs => (n, x) match {
              case (Assignment(lvalN, rvalN), Assignment(lvalX, rvalX)) =>
                if (lvalN == lvalX) x :: xs else n :: x :: xs
              case _ => x :: xs
            }
          }
        ).reverse

        val change = (list.length != sList.length) && ((list zip sList) exists (p => p._1 != p._2))
        if (change) simplify(NodeList(sList)) else NodeList(sList)
    }
  }

  def SimplifyBinExprOr(left: Node, right: Node): Node = {
    val sLeft = simplify(left)
    val sRight = simplify(right)
    (sLeft, sRight) match {
      case (_, TrueConst()) => TrueConst()
      case (TrueConst(), _) => TrueConst()
      case (expr, FalseConst()) => expr
      case (FalseConst(), expr) => expr
      case (exprL, exprR) if exprL == exprR => exprL
      case (exprL, exprR) =>
        if (exprL != left || exprR != right) simplify(BinExpr("or", exprL, exprR))
        else BinExpr("or", exprL, exprR)
    }
  }

  def SimplifyBinExprAnd(left: Node, right: Node): Node = {
    val sLeft = simplify(left)
    val sRight = simplify(right)
    (sLeft, sRight) match {
      case (_, FalseConst()) => FalseConst()
      case (FalseConst(), _) => FalseConst()
      case (expr, TrueConst()) => expr
      case (TrueConst(), expr) => expr
      case (exprL, exprR) if exprL == exprR => exprL
      case (exprL, exprR) =>
        if (exprL != left || exprR != right) simplify(BinExpr("and", exprL, exprR))
        else BinExpr("and", exprL, exprR)
    }
  }

  def SimplifyBinExprPow(left: Node, right: Node): Node = {
    (simplify(left), simplify(right)) match {
      case (expr, IntNum(n)) => if (n == 1) simplify(expr)
      else if (n == 0) IntNum(1)
      else {
        val s = simplify(expr)
        if (s != expr) simplify(BinExpr("**", s, IntNum(n))) else BinExpr("**", s, IntNum(n))
      }
      case (IntNum(n), expr) => if (n == 1) simplify(expr)
      else if (n == 0) IntNum(1)
      else {
        val s = simplify(expr)
        if (s != expr) simplify(BinExpr("**", s, IntNum(n))) else BinExpr("**", s, IntNum(n))
      }
      case (expr, FloatNum(n)) => if (n == 1) simplify(expr)
      else if (n == 0) FloatNum(1)
      else {
        val s = simplify(expr)
        if (s != expr) simplify(BinExpr("**", s, FloatNum(n))) else BinExpr("**", s, FloatNum(n))
      }
      case (FloatNum(n), expr) => if (n == 1) simplify(expr)
      else if (n == 0) FloatNum(1)
      else {
        val s = simplify(expr)
        if (s != expr) simplify(BinExpr("**", s, FloatNum(n))) else BinExpr("**", s, FloatNum(n))
      }

      // power laws:
      //case (BinExpr("**", IntNum(x), IntNum(y)), expr) => simplify(BinExpr("**", IntNum(x), BinExpr("**", IntNum(y), expr))) // dla testow tylko
      case (BinExpr("**", l, r), expr) => simplify(BinExpr("**", l, BinExpr("*", r, expr))) // to powinno byc, tego powyzej -- nie

      case (exprL, exprR) =>
        val sL = simplify(exprL)
        val sR = simplify(exprR)
        if (sL != exprL || sR != exprR) simplify(BinExpr("**", sL, sR)) else BinExpr("**", sL, sR)
    }
  }

  def SimplifyBinExprDiv(left: Node, right: Node): Node = {
    (left, right) match {
      case (exprL, exprR) if exprL == exprR => IntNum(1)
      // nazwy ponizej moga nie byc najbardziej czytelne, ale to raczej przez specyfike problemu :)
      case (exprNum, BinExpr("/", denomNum, denomDenom)) =>
        val sNum = simplify(exprNum)
        val sDenomNum = simplify(denomNum)
        val sDenomDenom = simplify(denomDenom)
        sNum match {
          case BinExpr("/", sNumNum, sNumDenom) =>
            simplify(BinExpr("/", BinExpr("*", sNumNum, sDenomDenom), BinExpr("*", sNumDenom, sDenomNum)))
          case expr => simplify(BinExpr("/", BinExpr("*", expr, sDenomDenom), sDenomNum))
        }

      // power laws:
      case (BinExpr("**", leftL, rightL), BinExpr("**", leftR, rightR)) if leftL == leftR =>
        simplify(BinExpr("**", leftL, BinExpr("-", rightL, rightR))) // TODO: czy potrzebny wewnetrzny simplify?

      case (expr, IntNum(n)) if n == 1 => simplify(expr)
      case (expr, FloatNum(n)) if n == 1 => simplify(expr)
      case (exprL, exprR) =>
        val sL = simplify(exprL)
        val sR = simplify(exprR)
        if (sL != exprL || sR != exprR) simplify(BinExpr("/", sL, sR)) else BinExpr("/", sL, sR)
    }
  }

  def SimplifyBinExprMul(left: Node, right: Node): Node = {
    (left, right) match {
      case (expr, IntNum(n)) => if (n == 1) simplify(expr)
      else if (n == 0) IntNum(0)
      else {
        val s = simplify(expr)
        if (s != expr) simplify(BinExpr("*", s, IntNum(n))) else BinExpr("*", s, IntNum(n))
      }
      case (IntNum(n), expr) => if (n == 1) simplify(expr)
      else if (n == 0) IntNum(0)
      else {
        val s = simplify(expr)
        if (s != expr) simplify(BinExpr("*", s, IntNum(n))) else BinExpr("*", s, IntNum(n))
      }
      case (expr, FloatNum(n)) => if (n == 1) simplify(expr)
      else if (n == 0) FloatNum(0)
      else {
        val s = simplify(expr)
        if (s != expr) simplify(BinExpr("*", s, FloatNum(n))) else BinExpr("*", s, FloatNum(n))
      }
      case (FloatNum(n), expr) => if (n == 1) simplify(expr)
      else if (n == 0) FloatNum(0)
      else {
        val s = simplify(expr)
        if (s != expr) simplify(BinExpr("*", s, FloatNum(n))) else BinExpr("*", s, FloatNum(n))
      }

      // power laws:
      case (BinExpr("**", leftL, rightL), BinExpr("**", leftR, rightR)) if leftL == leftR =>
        simplify(BinExpr("**", leftL, BinExpr("+", rightL, rightR))) // TODO: czy potrzebny wewnetrzny simplify?

      case (expr, BinExpr("/", exprNum, exprDenom)) => simplify(BinExpr("/", BinExpr("*", expr, exprNum), exprDenom))
      case (BinExpr("/", exprNum, exprDenom), expr) => simplify(BinExpr("/", BinExpr("*", expr, exprNum), exprDenom))
      case (exprL, exprR) =>
        val sL = simplify(exprL)
        val sR = simplify(exprR)
        if (sL != exprL || sR != exprR) simplify(BinExpr("*", sL, sR)) else BinExpr("*", sL, sR)
    }
  }

  def SimplifyBinExprPlus(left: Node, right: Node): Node = {
    (simplify(left), simplify(right)) match {
      case (expr, IntNum(n)) if n == 0 => expr
      case (IntNum(n), expr) if n == 0 => expr
      case (expr, FloatNum(n)) if n == 0 => expr
      case (FloatNum(n), expr) if n == 0 => expr
      case (Unary("-", exprU), expr) => simplify(BinExpr("-", expr, exprU))
      case (expr, Unary("-", exprU)) => simplify(BinExpr("-", expr, exprU))

      // balansowanie drzewa, w razie czego:
      //      case (BinExpr("+",
      //              BinExpr("+",
      //                 BinExpr("*", x1, y1),
      //                 BinExpr("*", x2, y2)),
      //              BinExpr("*", x3, y3)),
      //            BinExpr("*", x4, y4)) =>
      //
      //          simplify(BinExpr("+",
      //            simplify(BinExpr("+",
      //              simplify(BinExpr("*", x1, y1)),
      //              simplify(BinExpr("*", x2, y2)))),
      //            simplify(BinExpr("+",
      //              simplify(BinExpr("*", x2, y3)),
      //              simplify(BinExpr("*", x4, y4)))))
      //          )
      // distributive properties of "*":
      case (BinExpr("*", l, r), expr) if expr == l => simplify(BinExpr("*", BinExpr("+", r, IntNum(1)), l))
      case (BinExpr("*", l, r), expr) if expr == r => simplify(BinExpr("*", BinExpr("+", l, IntNum(1)), r))

      // wzory sk. mnozenia:
      case (BinExpr("+", BinExpr("**", x1, IntNum(a)), BinExpr("*", BinExpr("*", x2, IntNum(b)), y2)), BinExpr("**", y3, IntNum(c)))
        if a == 2 && b == 2 && c == 2 && (x1 == x2 && y2 == y3) =>
        simplify(BinExpr("**", BinExpr("+", x1, y2), IntNum(2)))
      case (BinExpr("+", BinExpr("**", x1, IntNum(a)), BinExpr("*", BinExpr("*", x2, IntNum(b)), y2)), BinExpr("**", y3, IntNum(c)))
        if a == 2 && b == 2 && c == 2 && (x1 == y2 && x2 == y3) =>
        simplify(BinExpr("**", BinExpr("+", x1, y3), IntNum(2)))


      case (e1@BinExpr("*", l1, r1), e2@BinExpr("*", l2, r2)) =>
        if (l1 == l2) BinExpr("*", BinExpr("+", r1, r2), l1)
        else if (r1 == r2) BinExpr("*", BinExpr("+", l1, l2), r1)
        else if (l1 == r2) BinExpr("*", BinExpr("+", r1, l2), l1)
        else if (r1 == l2) BinExpr("*", BinExpr("+", l1, r2), r1)
        else {
          val s1 = simplify(e1)
          val s2 = simplify(e2)
          if (s1 != e1 || s2 != e2) simplify(BinExpr("+", s1, s2)) else BinExpr("+", s1, s2)
        }

      // distributive properties of "/":
      case (e1@BinExpr("/", l1, r1), e2@BinExpr("/", l2, r2)) =>
        if (r1 == r2) BinExpr("/", BinExpr("+", l1, l2), r1)
        else {
          val s1 = simplify(e1)
          val s2 = simplify(e2)
          if (s1 != e1 || s2 != e2) simplify(BinExpr("+", s1, s2)) else BinExpr("+", s1, s2)
        }

      // commutative properties:
      case (e@BinExpr("-", exprL, exprR), expr) =>
        if (exprR == expr) simplify(exprL)
        else BinExpr("+", simplify(e), simplify(expr))
      case (expr, e@BinExpr("-", exprL, exprR)) =>
        if (exprR == expr) simplify(exprL)
        else BinExpr("+", simplify(expr), simplify(e))

      case (exprL, exprR) =>
        val sL = simplify(exprL)
        val sR = simplify(exprR)
        if (sL != exprL || sR != exprR) simplify(BinExpr("+", sL, sR)) else BinExpr("+", sL, sR)
    }
  }

  def SimplifyBinExprMinus(left: Node, right: Node): Node = {
    (simplify(left), simplify(right)) match {
      case (exprL, exprR) if exprL == exprR => IntNum(0)
      case (expr, IntNum(n)) if n == 0 => expr
      case (IntNum(n), expr) if n == 0 => simplify(Unary("-", expr))
      case (expr, FloatNum(n)) if n == 0 => expr
      case (FloatNum(n), expr) if n == 0 => simplify(Unary("-", expr))

      // distributive properties of "*":
      case (BinExpr("*", l, r), expr) if expr == l => simplify(BinExpr("*", BinExpr("-", r, IntNum(1)), l))
      case (BinExpr("*", l, r), expr) if expr == r => simplify(BinExpr("*", BinExpr("-", l, IntNum(1)), r))

      case (e1@BinExpr("*", l1, r1), e2@BinExpr("*", l2, r2)) =>
        if (l1 == l2) BinExpr("*", BinExpr("+", r1, r2), l1)
        else if (r1 == r2) BinExpr("*", BinExpr("-", l1, l2), r1)
        else if (l1 == r2) BinExpr("*", BinExpr("-", r1, l2), l1)
        else if (r1 == l2) BinExpr("*", BinExpr("-", l1, r2), r1)
        else {
          val s1 = simplify(e1)
          val s2 = simplify(e2)
          if (s1 != e1 || s2 != e2) simplify(BinExpr("-", s1, s2)) else BinExpr("-", s1, s2)
        }

      // distributive properties of "/":
      case (e1@BinExpr("/", l1, r1), e2@BinExpr("/", l2, r2)) =>
        if (r1 == r2) BinExpr("/", BinExpr("-", l1, l2), r1)
        else {
          val s1 = simplify(e1)
          val s2 = simplify(e2)
          if (s1 != e1 || s2 != e2) simplify(BinExpr("-", s1, s2)) else BinExpr("-", s1, s2)
        }

      // wzory skr. mnozenia:
      case (e1@BinExpr("**", BinExpr(op1, x1, y1), IntNum(a)), e2@BinExpr("**", BinExpr(op2, x2, y2), IntNum(b)))
        if x1 == x2 && y1 == y2 && a == 2 && b == 2 =>
        if (op1 == "+" && op2 == "-") BinExpr("*", BinExpr("*", x1, IntNum(4)), y1)
        else if (op1 == "-" && op2 == "+") Unary("-", BinExpr("*", BinExpr("*", x1, IntNum(4)), y1))
        else BinExpr("-", simplify(e1), simplify(e2))

      case (BinExpr("-", BinExpr("**", BinExpr("+", x1, y1), IntNum(a)), BinExpr("**", x2, IntNum(b))), BinExpr("*", BinExpr("*", x3, IntNum(c)), y3))
        if a == 2 && b == 2 && c == 2 && x1 == x2 && ((x1 == x3 && y1 == y3) || (x1 == y3 && y1 == x3)) =>
        simplify(BinExpr("**", y1, IntNum(2)))
      case (BinExpr("-", BinExpr("**", BinExpr("+", x1, y1), IntNum(a)), BinExpr("**", x2, IntNum(b))), BinExpr("*", BinExpr("*", x3, IntNum(c)), y3))
        if a == 2 && b == 2 && c == 2 && y1 == x2 && ((x1 == x3 && y1 == y3) || (x1 == y3 && y1 == x3)) =>
        BinExpr("**", x1, IntNum(2))

      // commutative properties:
      case (e@BinExpr("+", exprL, exprR), expr) =>
        if (exprL == expr) simplify(exprR)
        else if (exprR == expr) simplify(exprL)
        else BinExpr("-", simplify(e), simplify(expr))
      case (expr, e@BinExpr("+", exprL, exprR)) =>
        if (exprL == expr) simplify(Unary("-", exprR))
        else if (exprR == expr) simplify(Unary("-", exprL))
        else BinExpr("-", simplify(expr), simplify(e))

      case (exprL, exprR) =>
        val sL = simplify(exprL)
        val sR = simplify(exprR)
        if (sL != exprL || sR != exprR) simplify(BinExpr("-", sL, sR)) else BinExpr("-", sL, sR)
    }
  }

  def SimplifyUnary(expr: Node): Node = {
    expr match {
      case Unary("-", expr2) => simplify(expr2)
      // tutaj jeszcze tak naprawde czesc ewaluacji, ale juz zeby nie bylo az takiej redundancji tych kejsow...
      case IntNum(x) => IntNum(-x)
      case FloatNum(x) => FloatNum(-x)
      case expr2 => Unary("-", simplify(expr2))
    }
  }

  def SimplifyUnaryNot(expr: Node): Node = {
    expr match {
      case BinExpr("==", left, right) => simplify(BinExpr("!=", left, right))
      case BinExpr("!=", left, right) => simplify(BinExpr("==", left, right))
      case BinExpr("<=", left, right) => simplify(BinExpr(">", left, right))
      case BinExpr(">=", left, right) => simplify(BinExpr("<", left, right))
      case BinExpr("<", left, right) => simplify(BinExpr(">=", left, right))
      case BinExpr(">", left, right) => simplify(BinExpr("<=", left, right))

      case TrueConst() => FalseConst()
      case FalseConst() => TrueConst()

      case Unary("not", expr2) => simplify(expr2) // double negation

      case expr2 => Unary("not", simplify(expr2))
    }
  }

  def SimplifyBinExprFloatInt(op: String, x: Double, y: Integer): Node with Product with Serializable = {
    op match {
      case "+" => FloatNum(x + y)
      case "-" => FloatNum(x - y)
      case "*" => FloatNum(x * y)
      case "/" => FloatNum(x / y)
      case "%" => FloatNum(x % y)
      case "**" => FloatNum(pow(x, y.toDouble))

      case "==" => if (x == y) TrueConst() else FalseConst()
      case "!=" => if (x != y) TrueConst() else FalseConst()
      case ">=" => if (x >= y) TrueConst() else FalseConst()
      case "<=" => if (x <= y) TrueConst() else FalseConst()
      case ">" => if (x > y) TrueConst() else FalseConst()
      case "<" => if (x < y) TrueConst() else FalseConst()
    }
  }

  def SimplifyBinExprIntFloat(op: String, x: Integer, y: Double): Node with Product with Serializable = {
    op match {
      case "+" => FloatNum(x + y)
      case "-" => FloatNum(x - y)
      case "*" => FloatNum(x * y)
      case "/" => FloatNum(x / y)
      case "%" => FloatNum(x % y)
      case "**" => FloatNum(pow(x.toDouble, y))

      case "==" => if (x == y) TrueConst() else FalseConst()
      case "!=" => if (x != y) TrueConst() else FalseConst()
      case ">=" => if (x >= y) TrueConst() else FalseConst()
      case "<=" => if (x <= y) TrueConst() else FalseConst()
      case ">" => if (x > y) TrueConst() else FalseConst()
      case "<" => if (x < y) TrueConst() else FalseConst()
    }
  }

  def SimplifyBinExprFloatFloat(op: String, x: Double, y: Double): Node with Product with Serializable = {
    op match {
      case "+" => FloatNum(x + y)
      case "-" => FloatNum(x - y)
      case "*" => FloatNum(x * y)
      case "/" => FloatNum(x / y)
      case "%" => FloatNum(x % y)
      case "**" => FloatNum(pow(x, y))

      case "==" => if (x == y) TrueConst() else FalseConst()
      case "!=" => if (x != y) TrueConst() else FalseConst()
      case ">=" => if (x >= y) TrueConst() else FalseConst()
      case "<=" => if (x <= y) TrueConst() else FalseConst()
      case ">" => if (x > y) TrueConst() else FalseConst()
      case "<" => if (x < y) TrueConst() else FalseConst()
    }
  }

  def SimplifyBinExprIntInt(op: String, x: Integer, y: Integer): Node with Product with Serializable = {
    op match {
      case "+" => IntNum(x + y)
      case "-" => IntNum(x - y)
      case "*" => IntNum(x * y)
      case "/" => IntNum(x / y)
      case "%" => IntNum(x % y)
      case "**" => IntNum(pow(x.toDouble, y.toDouble).toInt)

      case "==" => if (x == y) TrueConst() else FalseConst()
      case "!=" => if (x != y) TrueConst() else FalseConst()
      case ">=" => if (x >= y) TrueConst() else FalseConst()
      case "<=" => if (x <= y) TrueConst() else FalseConst()
      case ">" => if (x > y) TrueConst() else FalseConst()
      case "<" => if (x < y) TrueConst() else FalseConst()
    }
  }

  def SimplifyAssignment(x: String, expr: Node): Node with Product with Serializable = {
    expr match {
      case Variable(y) if x == y => EmptyInstr()
      case _ => Assignment(Variable(x), simplify(expr))
    }
  }

  def SimplifyIfElseExpr(cond: Node, left: Node, right: Node): Node = {
    val sCond = simplify(cond)
    sCond match {
      case TrueConst() => simplify(left)
      case FalseConst() => simplify(right)
      case _ => IfElseExpr(sCond, simplify(left), simplify(right))
    }
  }

  def SimplifyIfInstr(cond: Node, left: Node): Node = {
    val sCond = simplify(cond)
    sCond match {
      case TrueConst() => simplify(left)
      case FalseConst() => EmptyInstr()
      case _ => IfInstr(sCond, simplify(left))
    }
  }

  def SimplifyIfElseInstr(cond: Node, left: Node, right: Node): Node = {
    val sCond = simplify(cond)
    sCond match {
      case TrueConst() => simplify(left)
      case FalseConst() => simplify(right)
      case _ => IfElseInstr(sCond, simplify(left), simplify(right))
    }
  }

  def SimlifyWhileInstr(cond: Node, body: Node): Node with Product with Serializable = {
    val sCond = simplify(cond)
    sCond match {
      case FalseConst() => EmptyInstr()
      case _ => WhileInstr(sCond, simplify(body))
    }
  }

  def SimplifyKeyDatumList(list: List[KeyDatum]): KeyDatumList = {
    KeyDatumList(list.foldLeft(Map.empty[Node, KeyDatum])(
      (_map, kd) => _map + (kd.key -> kd)
    ).toList.map(p => p._2))
  }
}
