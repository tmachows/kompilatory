package simplifier
import AST._

object BinarySimplifier {
  def apply(node: BinExpr) = {
    val nodeExpressionSimplified = ExpressionSimplifier(node)
    val nodeDivisionSimplified = nodeExpressionSimplified match {
      case expr: BinExpr => DivisionSimplifier(expr)
      case default => default
    }
    val constantEvaluationSimplified = nodeDivisionSimplified match {
      case expr: BinExpr => ConstantEvaluationSimplifier(expr)
      case default => default
    }

    val listContatenationSimplified: Node = constantEvaluationSimplified match {
      case expr: BinExpr => ListConcatenationSimplifier(expr)
      case default => default
    }

    val comutativitySimplified: Node = listContatenationSimplified match {
      case expr: BinExpr => CommutativitySimplifier(expr)
      case default => default
    }

    comutativitySimplified match {
      case expr:BinExpr => DistributiveMultiplicationSimplifier(expr)
      case default => default
    }
  }
}

object ExpressionSimplifier {
  def apply(node: BinExpr) = node match {
    case BinExpr("+", x, IntNum(0)) => x
    case BinExpr("+", IntNum(0), x) => x
    case BinExpr("-", x, y) if x == y => IntNum(0)
    case BinExpr("+", Unary("-", x), y) if x == y => IntNum(0)
    case BinExpr("*", x, IntNum(1)) => x
    case BinExpr("*", IntNum(1), x) => x
    case BinExpr("*", IntNum(0), _) => IntNum(0)
    case BinExpr("**", x, IntNum(1)) => x
    case BinExpr("**", x, IntNum(0)) => IntNum(1)
    case BinExpr("or", x, y) if x == y => x
    case BinExpr("and", x, y) if x == y => x
    case BinExpr("or", x, TrueConst()) => TrueConst()
    case BinExpr("or", x, FalseConst()) => x
    case BinExpr("and", x, FalseConst()) => FalseConst()
    case BinExpr("and", x, TrueConst()) => x
    case BinExpr("==", x, y) if x==y => TrueConst()
    case BinExpr(">=", x, y) if x==y => TrueConst()
    case BinExpr("<=", x, y) if x==y => TrueConst()
    case BinExpr("!=", x, y) if x==y => FalseConst()
    case BinExpr(">", x, y) if x==y => FalseConst()
    case BinExpr("<", x, y) if x==y => FalseConst()
    case default => BinExpr(node.op, Simplifier(node.left), Simplifier(node.right))
  }
}

object DivisionSimplifier {
  def apply(node: BinExpr) = node match {
    case BinExpr("/", x, y) if x==y => IntNum(1)
    case BinExpr("/", x, BinExpr("/", y, z)) if x==y => z
    case BinExpr("*", x, BinExpr("/", IntNum(1), y)) => BinExpr("/", x, y)
    case default => default
  }
}

object ConstantEvaluationSimplifier {
  def apply(node: BinExpr) = {
    val left = Simplifier(node.left)
    val right = Simplifier(node.right)
    BinExpr(node.op, left, right) match {
      case BinExpr("+", IntNum(x), IntNum(y)) => IntNum(x+y)
      case BinExpr("*", IntNum(x), IntNum(y)) => IntNum(x*y)
      case BinExpr("**", IntNum(x), IntNum(y)) => IntNum(x^y)
      case default => default
    }
  }
}

object ListConcatenationSimplifier {
  def apply(node: BinExpr) = {
    val left = Simplifier(node.left)
    val right = Simplifier(node.right)

    BinExpr(node.op, left, right) match {
      case BinExpr("+", ElemList(x), ElemList(y)) => ElemList(x:::y)
      case default => default
    }
  }
}

object CommutativitySimplifier {
  def apply(node: BinExpr) = node match {
    case BinExpr("-", BinExpr("+", x, y), z) if x==z => y
    case default => default
  }
}

object DistributiveMultiplicationSimplifier {
  def apply(node: BinExpr) = {
    val left = Simplifier(node.left)
    val right = Simplifier(node.right)


    BinExpr(node.op, left, right) match {
      case BinExpr("-", BinExpr("*", IntNum(v), x), y) if x==y => if (v == 2) x else BinExpr("*", IntNum(v-1), x)
      case BinExpr("-", BinExpr("*", IntNum(v), x), BinExpr("*", IntNum(w), y)) if x==y => if (v-w == 1) x else BinExpr("*", IntNum(v-w), x)
      case BinExpr("+", BinExpr("*", a, b), BinExpr("*", c, d)) if b==d => BinExpr("*", BinExpr("+", a, c), b)
      case BinExpr("+", BinExpr("*", a, b), BinExpr("*", c, d)) if a==c => BinExpr("*", a, BinExpr("+", b, d))
      case default => default
    }
  }
}