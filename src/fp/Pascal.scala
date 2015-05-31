package fp

import fp.Chapter6.State

/**
 * Created by ivan on 5/31/15.
 */
object Pascal extends App {

  def pTriangleOf(size: Int) = {
    def s(n: Int): State[List[Int], List[List[Int]]] = {
      State.sequence(List.fill(n)(State(l => {
        val nl = (1 :: (l zip l.tail map { case (i, j) => i + j })) :+ 1
        (nl, nl)
      })))
    }
    s(size).run(List(1))._1.map(_.map(_.toString).reduce(_ + " " + _))
  }

  pTriangleOf(60*4).foreach(println)
}
