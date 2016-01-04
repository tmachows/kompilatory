package simplifier
import AST._


object NodeListSimplifier {
  def apply(node: NodeList) = {
    val deadAssignmentSimplified = DeadAssignmentSimplifier(node)
    EmptyNodeExtractionSimplifier(deadAssignmentSimplified)
  }
}

object EmptyNodeExtractionSimplifier {
  def apply(node: NodeList) = {
    val nodes = node.list map Simplifier.simplify filterNot (node => node.isInstanceOf[Empty])
    if (nodes.length == 1)
      nodes(0)
    else NodeList(nodes)
  }
}

object DeadAssignmentSimplifier {
  def apply(node: NodeList) = {
    if (node.list forall (node => node.isInstanceOf[Assignment])) {
      NodeList(node.list.map(node => node.asInstanceOf[Assignment]).groupBy(node => node.left).mapValues(list => list.last).values.toList)
    }
    else node
  }
}