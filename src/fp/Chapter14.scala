package fp

import fp.Chapter11.Monad

/**
  * Created by Ivan Valchev (ivan.valchev@estafet.com) on 10/22/15.
  */
object Chapter14 extends App {

  sealed trait ST[S, A] {
    self =>
    protected def run(s: S): (A, S)
    def map[B](f: A => B): ST[S, B] = new ST[S, B] {
      def run(s: S): (B, S) = {
        val (a, s1) = self.run(s)
        (f(a), s1)
      }
    }
    def flatMap[B](f: A => ST[S, B]): ST[S, B] = new ST[S, B] {
      def run(s: S): (B, S) = {
        val (a, s1) = self.run(s)
        f(a).run(s1)
      }
    }
  }

  object ST {
    def apply[S, A](a: => A): ST[S, A] = {
      lazy val memo = a
      new ST[S, A] {
        def run(s: S): (A, S) = (memo, s)
      }
    }

    def monad[S] = new Monad[({type f[x] = ST[S, x]})#f] {
      override def unit[A](a: => A): ST[S, A] = ST(a)
      override def flatMap[A, B](fa: ST[S, A])(f: (A) => ST[S, B]): ST[S, B] = fa.flatMap(f)
    }

    def runST[A](r: RunnableST[A]): A = {
      r.apply[Unit].run(())._1
    }
  }

  sealed trait STRef[S, A] {
    protected var cell: A
    def read = new ST[S, A] {
      def run(s: S) = (cell, s)
    }
    def write(a: A) = new ST[S, Unit] {
      def run(s: S) = {
        cell = a
        ((), s)
      }
    }
  }

  object STRef {
    def apply[S, A](a: A): ST[S, STRef[S, A]] = ST(new STRef[S, A] {
      var cell = a
    })
  }

  trait RunnableST[A] {
    def apply[S]: ST[S, A]
  }

  val r: Int = ST.runST(new RunnableST[Int] {
    def apply[S]: ST[S, Int] = for {
      c <- STRef(1)
      i <- c.read
      _ <- c.write(i + 1)
      j <- c.read
    } yield j
  })

  sealed abstract class STArray[S, A](implicit manifest: Manifest[A]) {
    protected def value: Array[A]
    def size(): ST[S, Int] = ST(value.length)
    def write(i: Int, a: A) = new ST[S, Unit] {
      def run(s: S): (Unit, S) = {
        value(i) = a
        ((), s)
      }
    }
    def read(i: Int) = new ST[S, A] {
      def run(s: S): (A, S) = (value(i), s)
    }
    def freeze: ST[S, List[A]] = ST(value.toList)
    def fill(xs: Map[Int, A]): ST[S, Unit] = xs.foldRight(ST[S, Unit](()))({
      case ((k, v), st) => st.flatMap(_ => write(k, v))
    })
    def swap(i: Int, j: Int) = new ST[S, Unit] {
      def run(s: S): (Unit, S) = {
        val t = value(i)
        value(i) = value(j)
        value(j) = t
        ((), s)
      }
    }
  }

  object STArray {
    def apply[S, A: Manifest](sz: Int, v: A): ST[S, STArray[S, A]] =
      ST(new STArray[S, A] {
        lazy val value = Array.fill(sz)(v)
      })

    def partition[S](a: STArray[S, Int])(n: Int, r: Int, pivot: Int): ST[S, Int] = {
      val noop: ST[S, Unit] = new ST[S, Unit] {
        def run(s: S): (Unit, S) = ((), s)
      }
      for {
        pv <- a.read(pivot)
        _ <- a.swap(pivot, r)
        j <- STRef(n)
        _ <- (n until r).foldLeft(noop)((_, i) =>
          for {
            vi <- a.read(i)
            _ <- if (vi < pv) for {
              vj <- j.read
              _ <- a.swap(i, vj)
              _ <- j.write(vj + 1)
            } yield ()
            else noop
          } yield ()
        )
        x <- j.read
        _ <- a.swap(x, r)
      } yield x
    }
  }

  println(r)
}
