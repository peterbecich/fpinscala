package fpinscala.parsing

import java.util.regex._
import scala.util.matching.Regex
import fpinscala.testing._
import fpinscala.testing.Prop._
import scala.language.higherKinds
import scala.language.implicitConversions
import fpinscala.monads.Monad



/*
 All Parsers in this example take a String as input.  The parametric type A is the type "measured": a count, a string, a char.
 Parser[Int] that counts the number of chars "x" will require a Parser[Char] to function.
 */
trait Parsers[ParseError, Parser[+_]] { self => // so inner classes may call methods of trait
  implicit def string(s: String): Parser[String]
  implicit def operators[A](p: Parser[A]) = ParserOps[A](p)
  implicit def asStringParser[A](a: A)(
    implicit f: A => Parser[String]): ParserOps[String] =
    ParserOps(f(a))

  type Parser[+A] = Location => Result[A]

  trait Result[+A]
  case class Success[+A](get: A, charsConsumed: Int) extends
      Result[A]
  case class Failure(get: ParseError) extends
      Result[Nothing]

  val parserMonad = Monad.parserMonad[Parser](self)

  def run[A](p: Parser[A])(input: String): Either[ParseError,A]

  def map[A,B](p: Parser[A])(f: A=>B): Parser[B]

  def flatMap[A,B](p: Parser[A])(f: A=>Parser[B]): Parser[B]

  def map2[A,B,C](p: Parser[A], p2: Parser[B])(f: (A,B)=>C): Parser[C] = {
    val parserAB: Parser[(A,B)] = p.product(p2)
    val parserC: Parser[C] = parserAB.map(
      (tup: (A,B)) => f(tup._1, tup._2)
    )
    parserC
  }
  // akin to unit?
  def succeed[A](a: A): Parser[A] =
    string("").map((s: String) => a)

  def product[A,B](p: Parser[A], p2: Parser[B]): Parser[(A,B)]


  // parser returned recognized char c
  def char(c: Char): Parser[Char] =
    string(c.toString).map((s: String)=>s.charAt(0))

  // parser returned recognizes string s
  //def string(s: String): Parser[String]

  // what does this do?
  def many[A](p: Parser[A]): Parser[List[A]]

  // "see what portion of the input string" is examined
  def slice[A](p: Parser[A]): Parser[String]

  // parser returned recognizes either p1 or p2
  /*
  run(or(string("abra"),string("cadabra")))("abra") == Right("abra")
run(or(string("abra"),string("cadabra")))("cadabra") == Right("cadabra")
   */
  def or[A](p1: Parser[A], p2: Parser[A]): Parser[A]

  // use map2 and succeed
  /*
run(listOfN(3, "ab" | "cad"))("ababcad") == Right(List("ab","ab","cad"))
run(listOfN(3, "ab" | "cad"))("cadabab") == Right(List("cad","ab","ab"))
run(listOfN(3, "ab" | "cad"))("ababab") == Right(List("ab","ab","ab"))
   */

  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] = {
    //import p._
    //import P._
    if(n<=1){
      p.map((a: A)=>List(a))
    } else {
      val pla: Parser[List[A]] = listOfN(n-1, p)
      p.map2(pla)((a: A, la: List[A]) => a::la)
    }
  }


  // [error]  Note: implicit method operators is not applicable here because it comes after the application point and it lacks an explicit result type
  // implicit def string(s: String): Parser[String]
  // implicit def operators[A](p: Parser[A]) = ParserOps[A](p)
  // implicit def asStringParser[A](a: A)(
  //   implicit f: A => Parser[String]): ParserOps[String] =
  //   ParserOps(f(a))
  case class ParserOps[A](p: Parser[A]) {
    def map[B](f: A=>B): Parser[B] = self.map(p)(f)
    def map2[B,C](p2: Parser[B])(f: (A,B)=>C): Parser[C] =
      self.map2(p, p2)(f)
    def product[B](p2: Parser[B]): Parser[(A,B)] =
      self.product(p, p2)
    def **[B](p2: Parser[B]): Parser[(A,B)] = this.product(p2)
    def many: Parser[List[A]] = self.many(p)
    def slice: Parser[String] = self.slice(p)
    def or[B>:A](p2: Parser[B]): Parser[B] = self.or(p, p2)
    def |[B>:A](p2: Parser[B]): Parser[B] = this.or(p2)

    // We expect that, for instance,
    // run(numA)("aaa") gives Right(3) ,
    // and run(numA)("b") gives Right(0) .
    // val numberOfA: Parser[Int] = char('a').many.map(
    //   (s: String) => s.size)
    val numberOfA: Parser[Int] = char('a').many.slice.map(
      (s: String) => s.size)

  }

  object Laws {
    //run(char(c))(c.toString) == Right(c)
    // map(p)(a => a) == p

    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
      forAll(in)((s: String) => run(p1)(s) == run(p2)(s))

    def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop =
      equal(p, p.map(a => a))(in)

    // run(succeed(a))(s) == Right(s)
    //def succeedLaw[
    // check the behavior of product ---> Monad Laws
    // def productLaw[A,B](pa: Parser[A], pb: Parser[B])(
    //   in: Gen[String]): Prop = {
    //   val pab: Parser[(A,B)] = pa.product(pb)
    //   /*
    //    What needs to be shown for Parser[(A,B)] 
    //    and Parser[A], Parser[B]?

       
    //    */
    // }

  }
}




case class Location(input: String, offset: Int = 0) {

  lazy val line = input.slice(0,offset+1).count(_ == '\n') + 1
  lazy val col = input.slice(0,offset+1).reverse.indexOf('\n')

  def toError(msg: String): ParseError =
    ParseError(List((this, msg)))

  def advanceBy(n: Int) = copy(offset = offset+n)

  /* Returns the line corresponding to this location */
  def currentLine: String = 
    if (input.length > 1) input.lines.drop(line-1).next
    else ""
}

case class ParseError(stack: List[(Location,String)] = List(),
                      otherFailures: List[ParseError] = List()) {
}
