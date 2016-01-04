package simplifier

import AST._

// to implement
// avoid one huge match of cases
// take into account non-greedy strategies to resolve cases with power laws
object Simplifier {
  def apply(node: Node) = simplify(node)

  def simplify(node: Node): Node = node match {
    case node: Unary => UnarySimplifier(node)
    case node: BinExpr => BinarySimplifier(node)
    case node: IfElseExpr => IfElseExprSimplifier(node)
    case node: Assignment => AssignmentSimplifier(node)
    case node: Subscription => SubscriptionSimplifier(node)
    case node: KeyDatum => KeyDatumSimplifier(node)
    case node: GetAttr => GetAttrSimplifier(node)
    case node: IfInstr => IfInstrSimplifier(node)
    case node: IfElseInstr => IfElseInstrSimplifier(node)
    case node: WhileInstr => WhileInstrSimplifier(node)
    case node: ReturnInstr => ReturnInstrSimplifier(node)
    case node: PrintInstr => PrintInstrSimplifier(node)
    case node: FunCall => FunCallSimplifier(node)
    case node: FunDef => FunDefSimplifier(node)
    case node: LambdaDef => LambdaDefSimplifier(node)
    case node: ClassDef => ClassDefSimplifier(node)
    case node: NodeList => NodeListSimplifier(node)
    case node: KeyDatumList => KeyDatumListSimplifier(node)
    case node: ElemList => ElemListSimplifier(node)
    case node: Tuple => TupleSimplifier(node)
    case default => node
  }
}
object UnarySimplifier {
  def apply(node: Unary) = {
    val expr = Simplifier(node.expr)

    Unary(node.op, expr) match {
      //get rid of not before comparisons
      case Unary("not", BinExpr("==", x, y)) => BinExpr("!=", x, y)
      case Unary("not", BinExpr("!=", x, y)) => BinExpr("==", x, y)
      case Unary("not", BinExpr(">", x, y)) => BinExpr("<=", x, y)
      case Unary("not", BinExpr("<", x, y)) => BinExpr(">=", x, y)
      case Unary("not", BinExpr(">=", x, y)) => BinExpr("<", x, y)
      case Unary("not", BinExpr("<=", x, y)) => BinExpr(">", x, y)

      //cancel double unary ops
      case Unary("not", Unary("not", Unary("not", x))) => Unary("not", x)
      case Unary("-", Unary("-", x)) => x

      //evaluate constants
      case Unary("not", TrueConst()) => FalseConst()
      case Unary("not", FalseConst()) => TrueConst()

      case default => default
    }
  }
}

object IfElseExprSimplifier {
  def apply(node: IfElseExpr) = node match {
    case IfElseExpr(TrueConst(), left, right) => left
    case IfElseExpr(FalseConst(), left, right) => right
    case default => IfElseExpr(Simplifier(node.cond), Simplifier(node.left), Simplifier(node.right))
  }
}

object AssignmentSimplifier {
  def apply(node: Assignment) = {
    val left = Simplifier(node.left)
    val right = Simplifier(node.right)
    if (left==right)
      Empty()
    else Assignment(left, right)
  }
}

object SubscriptionSimplifier {
  def apply(node: Subscription) = Subscription(Simplifier(node.expr), Simplifier(node.sub))
}

object KeyDatumSimplifier {
  def apply(node: KeyDatum) = KeyDatum(Simplifier(node.key), Simplifier(node.value))
}

object GetAttrSimplifier {
  def apply(node: GetAttr) = GetAttr(Simplifier(node.expr), node.attr)
}

object IfInstrSimplifier {
  def apply(node: IfInstr) = IfInstr(Simplifier(node.cond), Simplifier(node.left))
}

object IfElseInstrSimplifier {
  def apply(node: IfElseInstr) = {
    val cond = Simplifier(node.cond)
    val left = Simplifier(node.left)
    val right = Simplifier(node.right)

    cond match {
      case TrueConst() => left
      case FalseConst() => right
      case default => node
    }
  }
}

object WhileInstrSimplifier {
  def apply(node: WhileInstr) = {
    val cond = Simplifier(node.cond)
    val body = Simplifier(node.body)

    cond match {
      case FalseConst() => Empty()
      case default => node
    }
  }
}
object ReturnInstrSimplifier {
  def apply(node: ReturnInstr) = ReturnInstr(Simplifier(node.expr))
}

object PrintInstrSimplifier {
  def apply(node: PrintInstr) = PrintInstr(Simplifier(node.expr))
}

object FunCallSimplifier {
  def apply(node: FunCall) = FunCall(Simplifier(node.name), Simplifier(node.args_list))
}

object FunDefSimplifier {
  def apply(node: FunDef) = FunDef(node.name, Simplifier(node.formal_args), Simplifier(node.body))
}

object LambdaDefSimplifier {
  def apply(node: LambdaDef) = LambdaDef(Simplifier(node.formal_args), Simplifier(node.body))
}

object ClassDefSimplifier {
  def apply(node: ClassDef) = ClassDef(node.name, Simplifier(node.inherit_list), Simplifier(node.suite))
}

object KeyDatumListSimplifier {
  def apply(node: KeyDatumList) =
    KeyDatumList(node.list.map(KeyDatumSimplifier.apply).groupBy(kd => kd.key).mapValues(v => v.last).values.toList)
}

object ElemListSimplifier {
  def apply(node: ElemList) = ElemList(node.list map Simplifier.simplify)
}

object TupleSimplifier {
  def apply(node: Tuple) = Tuple(node.list map Simplifier.simplify)
}