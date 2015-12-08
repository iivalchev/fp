package fp

import scala.annotation.tailrec
import scala.collection.immutable.{Stream => ScalaStream}

/**
 * Created by Ivan Valchev (ivan.valchev@estafet.com) on 12/4/15.
 */
object DFS extends App {

  case class Node[A](value: A, adjecent: () => List[Node[A]])

  def directedCycle[A](node: Node[A]): List[Node[A]] = {
    @tailrec
    def dfs(visited: Set[Node[A]], stack: List[Node[A]], path: List[Node[A]]): List[Node[A]] = {
      stack match {
        case h :: _ if visited(h) => h :: path
        case h :: t => dfs(visited + h, h.adjecent() ::: t, h :: path)
        case _ => Nil
      }
    }
    dfs(Set(), List(node), List())
  }


  assert(directedCycle(Node(1, () => List())).isEmpty)

  val node: Node[Int] = Node(1, () => List(node))
  private val cycle: List[Node[Int]] = directedCycle(node)
  println(cycle)
  assert(cycle.nonEmpty)

}
