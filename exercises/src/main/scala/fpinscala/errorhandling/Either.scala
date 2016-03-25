package fpinscala.errorhandling


import scala.{Option => _, Either => _, Left => _, Right => _, _} // hide std library `Option` and `Either`, since we are writing our own in this chapter

sealed trait Either[+E,+A] {
  def map[B](f: A => B): Either[E, B] = this match {
    case Right(a) => Right(f(a))
    case left @ Left(e) => left
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Right(a) => f(a)
    case left @ Left(e) => left // allowed because EE >: E
  }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case right @ Right(a) => right
    case left @ Left(e) => b
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    this.flatMap { (a: A) =>
      b.map { (b: B) => f(a,b) }
    }

}
case class Left[+E](get: E) extends Either[E,Nothing]
case class Right[+A](get: A) extends Either[Nothing,A]

object Either {

  def unit[E, A](a: A): Either[E, A] = Right(a)

  def prepend[E,A](eitherA: Either[E,A], eitherListA: Either[E, List[A]]):
      Either[E, List[A]] =
    eitherA.flatMap { (a: A) =>
      eitherListA.map { (listA: List[A]) =>
        a::listA
      }
    }

  def traverse[E,A,B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    es.foldRight(unit(List[B]()): Either[E, List[B]]) { (a: A, eitherListB: Either[E, List[B]]) =>
      val eitherB: Either[E, B] = f(a)
      prepend(eitherB, eitherListB)
    }

  def sequenceThroughTraverse[E,A](es: List[Either[E,A]]):
      Either[E,List[A]] =
    traverse(es)((ea: Either[E,A]) => ea)

  def sequence[E,A](es: List[Either[E,A]]): Either[E,List[A]] = {
    es.foldRight[Either[E, List[A]]](unit(List[A]()): Either[E, List[A]]) {
      (eitherA: Either[E,A], eitherListA: Either[E, List[A]]) =>
      prepend(eitherA, eitherListA)
    }
  }

  def foo(arg1: Int)(
    arg2: Int): Int =
    arg1+arg2


  def mean(xs: IndexedSeq[Double]): Either[String, Double] = 
    if (xs.isEmpty) 
      Left("mean of empty list!")
    else  
      Right(xs.sum / xs.length)

  import java.lang.ArithmeticException

  def safeDiv(x: Int, y: Int): Either[Exception, Double] = 
    try {
      val d: Double = x.toDouble / y
      if(d.isNaN || d.isPosInfinity || d.isNegInfinity)
        Left(new ArithmeticException("$x/$y incalculable"))
      else
        Right(d)
    } catch { case e: Exception => Left(e) }

  def Try[A](a: => A): Either[Exception, A] =
    try { Right(a) }
    catch { case e: Exception => Left(e) }

}


object EitherTest extends App {

  import Either._

  val a = (10 to 30).toList
  val b = (-10 to 10).toList
  val fracs: List[Tuple2[Int, Int]] = a.zip(b)

  println("fractions to be calculated: "+fracs)

  def divTuple(tup: (Int, Int)): Either[Exception, Double] =
    safeDiv(tup._1, tup._2)

  val failingDiv = traverse(fracs)(divTuple)

  println("Failing division of tuples: "+failingDiv)

  val c = (1 to 21).toList
  val fracsSuccessful = a.zip(c)

  println("fraction to be divided: "+fracsSuccessful)

  val successDiv = traverse(fracsSuccessful)(divTuple)

  println("Successful divison (need Double type though): " + successDiv)

}
