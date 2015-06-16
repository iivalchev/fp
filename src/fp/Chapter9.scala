package fp


import fp.Chapter8._

import scala.util.matching.Regex


/**
 * Created by ivan on 5/27/15.
 */

object Chapter9 {

  object ReferenceTypes {
    type Parser[+A] = Location => Result[A]

    trait Result[+A]

    case class Success[+A](get: A, charsConsumed: Int) extends Result[A]

    case class Failure[+A](get: ParseError) extends Result[A]


    object Reference extends Parsers[Parser] {
      override def run[A](p: Parser[A])(input: String): Either[ParseError, A] = p(Location(input, 0)) match {
        case Success(a, _) => Right(a)
        case Failure(e) => Left(e)
      }

      override def flatMap[A, B](p: Parser[A])(f: (A) => Parser[B]): Parser[B] = ???

      override def or[A](p1: Parser[A], p2: => Parser[A]): Parser[A] = ???

      override implicit def string(s: String): Parser[String] = {
        in => {
          if (in.input.startsWith(s, in.offset)) {
            Success(s, s.length)
          } else {
            Failure(Location(in.input.substring(in.offset)).toError("Expected: " + s))
          }
        }
      }

      override def scope[A](msg: String)(p: Parser[A]): Parser[A] = ???

      override implicit def regex(r: Regex): Parser[String] = {
        loc =>
          r.findPrefixOf(loc.input.substring(loc.offset)) match {
            case Some(m) => Success(m, m.length)
            case _ => Failure(ParseError(List((loc, "regex " + r))))
          }
      }

      override def slice[A](p: Parser[A]): Parser[String] = {
        loc => {
          p(loc) match {
            case Success(_, charsConsumed) => Success(loc.input.substring(loc.offset, loc.offset + charsConsumed), charsConsumed)
            case Failure(e) => Failure(e)
          }
        }
      }

      override def fail: Parser[Nothing] = ???

      override def label[A](msg: String)(p: Parser[A]): Parser[A] = ???

      override def attempt[A](p: Parser[A]): Parser[A] = ???
    }

  }

  trait Parsers[Parser[+ _]] {
    self =>

    def run[A](p: Parser[A])(input: String): Either[ParseError, A]


    def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B]

    def or[A](p1: Parser[A], p2: => Parser[A]): Parser[A]

    implicit def string(s: String): Parser[String]

    implicit def regex(r: Regex): Parser[String]

    def slice[A](p: Parser[A]): Parser[String]

    def attempt[A](p: Parser[A]): Parser[A]

    def label[A](msg: String)(p: Parser[A]): Parser[A]

    def scope[A](msg: String)(p: Parser[A]): Parser[A]


    def fail: Parser[Nothing]

    def succeeded[A](a: A): Parser[A] = string("").map(_ => a)

    def char(c: Char): Parser[Char] = string(c.toString).map(_.charAt(0))

    def map[A, B](p: Parser[A])(f: A => B): Parser[B] = flatMap(p)(a => succeeded(f(a)))

    def map2[A, B, C](p1: Parser[A], p2: => Parser[B])(f: (A, B) => C): Parser[C] = flatMap(p1)(a => map(p2)(b => f(a, b)))

    def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] = if (n == 0) succeeded(List()) else map2(p, listOfN(n - 1, p))(_ :: _)

    implicit def parserOps[A](p: Parser[A]): ParserOps[A] = ParserOps(p)

    implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))

    def many[A](p: Parser[A]): Parser[List[A]] = map2(p, many(p))(_ :: _) | succeeded(List())

    def many1[A](p: Parser[A])(notFound: ParseError) = map2(p, p.many)

    def product[A, B](p1: Parser[A], p2: => Parser[B]): Parser[(A, B)] = map2(p1, p2)((_, _))

    def op1[A, B](p: Parser[(A, B)]): Parser[A] = p.map(_._1)

    def op2[A, B](p: Parser[(A, B)]): Parser[B] = p.map(_._2)


    def errorLocation(e: ParseError): Location = e.stack.head._1

    def errorMessage(e: ParseError): String = e.stack.head._2


    case class ParserOps[A](p: Parser[A]) {
      def |[B >: A](p2: Parser[B]) = self.or(p, p2)

      def or[B >: A](p2: Parser[B]) = self.or(p, p2)

      def map[B](f: A => B) = self.map(p)(f)

      def flatMap[B](f: A => Parser[B]) = self.flatMap(p)(f)

      def many: Parser[List[A]] = self.many(p)

      def slice: Parser[String] = self.slice(p)

      def **[B](p2: Parser[B]) = self.product(p, p2)

      def product[B](p2: Parser[B]) = self.product(p, p2)
    }

    object Laws {
      def equal[A](p1: Parser[A], p2: Parser[A])(s: String) = run(p1)(s) == run(p2)(s)

      def succeededLaw[A](a: A, g: Gen[String]) = Prop.forAll(g)(in => run(succeeded(a))(in) == a)

      def charLaw(g: Gen[Char]) = Prop.forAll(g)(c => run(char(c))(c.toString) == Right(c))

      def stringLaw(g: Gen[String]) = Prop.forAll(g)(s => run(s)(s) == Right(s))

      def orLaw = Prop.check(run("abra" | "cadabra")("abra") == Right("abra")) &&
        Prop.check(run("abra" | "cadabra")("cadabra") == Right("cadabra"))

      def listOfNLaw = Prop.check(run(self.listOfN(3, "ab" | "cad"))("ababcad") == Right("ababcad")) &&
        Prop.check(run(self.listOfN(3, "ab" | "cad"))("abcadcad") == Right("abcadcad")) &&
        Prop.check(run(self.listOfN(3, "ab" | "cad"))("ababab") == Right("ababab"))

      def mapLaw[A](p: Parser[A])(g: Gen[String]) = Prop.forAll(g)(in => equal(p, p.map(a => a))(in))

      def productLaw = Prop.check(run(string("a") product string("b"))("ab") == Right(("a", "b"))) &&
        Prop.check {
          run("a" product succeeded(""))("") match {
            case _: Left => true
            case _ => false
          }
        } &&
        Prop.check {
          run(succeeded("") product "a")("") match {
            case _: Left => true
            case _ => false
          }
        }

      def attemptLaw = Prop.check(run(attempt(string("a").flatMap(_ => fail)) | string("b"))("b") == Right("a")) &&
        Prop.check(run(string("a").flatMap(_ => fail) | string("b"))("b") match {
          case _: Left => true
          case _ => false
        })
    }

  }

  case class ParseError(stack: List[(Location, String)])

  case class Location(input: String, offset: Int = 0) {
    lazy val line = input.slice(0, offset + 1).count(_ == "/n") + 1
    lazy val col = input.lastIndexOf("/n") match {
      case -1 => offset + 1
      case lineStart => offset - lineStart
    }

    def toError(msg: String) = ParseError(List((this, msg)))
  }

  trait JSON

  case object JNull extends JSON

  case class JNumber(get: Double) extends JSON

  case class JString(get: String) extends JSON

  case class JBool(get: Boolean) extends JSON

  case class JArray(get: IndexedSeq[JSON]) extends JSON

  case class JObject(get: Map[String, JSON]) extends JSON

  def jsonParser[ParseError, Parser[+ _]](P: Parsers[Parser]): Parser[JSON] = {
    import P._

    def NAME: Parser[String] = regex( """"(.*)"""".r)
    def NULL: Parser[JNull.type] = regex( """(?i)(null)""".r).map(_ => JNull)
    def NUMBER: Parser[JNumber] = regex( """(\d*(\.\d+))""".r).map(s => JNumber(s.toDouble))
    def BOOLEAN: Parser[JBool] = regex( """(?i)(true|false)""".r).map(s => JBool(s.toBoolean))
    def STRING: Parser[JString] = NAME.map(s => JString(s))

    def VALUE: Parser[JSON] = NULL | NUMBER | BOOLEAN | STRING | ARRAY | OBJECT
    def PROP: Parser[(String, JSON)] = op1(NAME ** ":") ** VALUE

    def OBJECT: Parser[JObject] = (op2("{" ** PROP) ** op1(op2("," ** PROP).many ** "}")).map(t => t._1 :: t._2).map(l => JObject(l.foldLeft[Map[String, JSON]](Map())(_ + _)))
    def ARRAY: Parser[JArray] = (op2("[" ** VALUE.many) ** op1(op2("," ** VALUE).many ** "]")).map(t => JArray((t._1 ++ t._2).toArray))

    ARRAY | OBJECT
  }

}
