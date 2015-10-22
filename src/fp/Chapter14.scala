package fp

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

  println(r)
}
