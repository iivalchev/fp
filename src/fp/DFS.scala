package fp

import scala.annotation.tailrec
import scala.collection.immutable.{Stream => ScalaStream}

/**
 * Created by Ivan Valchev (ivan.valchev@estafet.com) on 12/4/15.
 */
object DFS extends App {

  case class Node[A](value: A, adjecent: () => List[Node[A]])

  def directedCycle[A](node: Node[A]): List[List[Node[A]]] = {
    @tailrec
    def backTrack(n: Node[A], fullPath: List[Node[A]], path: List[Node[A]]): List[Node[A]] = {
      fullPath match {
        case h :: _ if h == n => (h :: path).reverse
        case h :: t => backTrack(n, t, h :: path)
      }
    }
    @tailrec
    def dfs(visited: Set[Node[A]], stack: List[Node[A]], path: List[Node[A]], cycles: List[List[Node[A]]]): List[List[Node[A]]] = {
      stack match {
        case h :: t if visited(h) => dfs(visited, t, path, (h :: backTrack(h, path, List())) :: cycles)
        case h :: t => dfs(visited + h, h.adjecent() ::: t, h :: path, cycles)
        case _ => cycles
      }
    }
    dfs(Set(), List(node), List(), List())
  }

  val node2: Node[Int] = Node(2, () => List(Node(3, () => List(Node(4, () => List(node2))))))
  val node: Node[Int] = Node(1, () => List(node, node2))
  private val cycle: List[List[Node[Int]]] = directedCycle(node)
  println(cycle)

}
