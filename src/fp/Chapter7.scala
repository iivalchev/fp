package fp

import java.util.concurrent._

/**
 * Created by Ivan Valchev (ivan.valchev@estafet.com) on 3/10/15.
 */
object Chapter7 extends App {

  type Par[A] = ExecutorService => Future[A]

  object Par {

    def unit[A](a: A): Par[A] = _ => UnitFuture(a)

    def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

    def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = es => {
      val af = a(es)
      val bf = b(es)
      UnitFuture(f(af.get(), bf.get()))
    }

    def map2balanced[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = es => {
      val af = fork(a)(es)
      val bf = fork(b)(es)
      UnitFuture(f(af.get(), bf.get()))
    }

    def map[A, B](a: Par[A])(f: A => B): Par[B] = map2(a, unit(()))((a, _) => f(a))

    def fork[A](a: => Par[A]): Par[A] = es => es.submit(new Callable[A] {
      def call() = {
        a(es).get
      }
    })

    def deadLockFork[A](numOfThreads: Int, a: => Par[A]): Par[A] = {
      if (numOfThreads < 0) {
        fork(a)
      } else {
        deadLockFork(numOfThreads - 1, fork(a))
      }
    }

    def run[A](p: Par[A]): A = throw new Exception()

    def asyncF[A, B](f: A => B): A => Par[B] = a => lazyUnit(f(a))

    def sortPar(parList: Par[List[Int]]): Par[List[Int]] = map(parList)(_.sorted)

    def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] = fork(sequence(ps.map(asyncF(f))))

    def sequence[A](ps: List[Par[A]]): Par[List[A]] = ps.foldRight[Par[List[A]]](unit(List()))((h, t) => map2balanced(h, t)(_ :: _))

    def sequenceBalanced[A](ps: IndexedSeq[Par[A]]): Par[IndexedSeq[A]] = {
      if (ps.length == 0) unit(IndexedSeq())
      else if (ps.length == 1) map(ps.head)(a => IndexedSeq(a))
      else {
        val (l, r) = ps.splitAt(ps.length / 2)
        map2(fork(sequenceBalanced(l)), fork(sequenceBalanced(r)))(_ ++ _)
      }
    }

    def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = mapReduce(as)(a => if (f(a)) List(a) else List())(_.flatten)

    def mapReduce[A, B](as: List[A])(m: A => B)(r: List[B] => B): Par[B] = map(sequence(as.map(asyncF(m))))(r)

    def parDivide[A](as: IndexedSeq[A])(f: (A, A) => A): Par[Option[A]] = {
      if (as.size <= 1) {
        unit(as.headOption)
      } else {
        val (l, r) = as.splitAt(as.size / 2)
        map2(fork(parDivide(l)(f)), fork(parDivide(r)(f))) {
          (oa1, oa2) =>
            for {
              a1 <- oa1
              a2 <- oa2
            } yield f(a1, a2)
        }
      }
    }

    def map3[A, B, C, D](pa: Par[A], pb: Par[B], pc: Par[C])(f: (A, B, C) => D): Par[D] = map2(map2(pa, pb)((_, _)), pc)((t, c) => f(t._1, t._2, c))

    def map4[A, B, C, D, E](pa: Par[A], pb: Par[B], pc: Par[C], pd: Par[D])(f: (A, B, C, D) => E): Par[E] = map2(map3(pa, pb, pc)((_, _, _)), pd)((t, d) => f(t._1, t._2, t._3, d))

    def map5[A, B, C, D, E, F](pa: Par[A], pb: Par[B], pc: Par[C], pd: Par[D], pe: Par[E])(f: (A, B, C, D, E) => F): Par[F] = map2(map4(pa, pb, pc, pd)((_, _, _, _)), pe)((t, e) => f(t._1, t._2, t._3, t._4, e))

    def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] = es => choices(n(es).get)(es)

    def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] = choiceN(map(cond)(if (_) 0 else 1))(List(t, f))

    def flatMap[A, B](cond: Par[A])(f: A => Par[B]): Par[B] = join(map(cond)(f(_)))

    def join[A](a: Par[Par[A]]): Par[A] = es => a(es).get()(es)

  }

  case class UnitFuture[A](get: A) extends Future[A] {
    override def cancel(mayInterruptIfRunning: Boolean): Boolean = false

    override def isCancelled: Boolean = false

    override def get(timeout: Long, unit: TimeUnit): A = get

    override def isDone: Boolean = true
  }

  import Par._

  def sum(ints: IndexedSeq[Int]): Par[Option[Int]] = Par.parDivide(ints)(_ + _)

  def max(ints: IndexedSeq[Int]): Par[Option[Int]] = Par.parDivide(ints)(Math.max)

  def countWords(p: List[String]): Par[Int] = mapReduce(p)(s => s.split("\\s").size)(_.sum)


  println(sum(IndexedSeq(1, 2, 3, 4, 5, 6))(Executors.newCachedThreadPool()))

  println(max(IndexedSeq(1, 2, 3, 4, 5, 6))(Executors.newCachedThreadPool()))

  println(countWords(List("one two three", "four five"))(Executors.newCachedThreadPool()))

  println(parMap(List.range(1, 1000003))(math.sqrt(_))(Executors.newFixedThreadPool(2)).get)

  object NonBlocking {

  }

}
