package fpinscala.errorhandling


import scala.{Option => _, Some => _, Either => _, _} // hide std library `Option`, `Some` and `Either`, since we are writing our own in this chapter

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case Some(get) => Some(f(get))
    case None => None
  }

  def getOrElse[B>:A](default: => B): B = this match {
    case Some(get) => get
    case None => default
  }

  // def flatMap[B](f: A => Option[B]): Option[B] = this match {
  //   case Some(get) => f(get)
  //   case None => None
  // }

  // getOrElse necessary in case of change in container type;
  // not really necessary in this case, because our only Functor is Option
  // Only option is Option, ha ha
  // def flatMap[B](f: A => Option[B]): Option[B] = 
  //   this.map((a: A) => f(a)).getOrElse(None)

  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case Some(a) => f(a)
    case None => None
  }

  def orElse[B>:A](ob: => Option[B]): Option[B] = this match {
    case Some(get) => Some(get)
    case None => ob
  }

  // def filter(f: A => Boolean): Option[A] = this match {
  //   case Some(get) if f(get)==true => Some(get)
  //   case _ => None
  // }

  def filter(f: A => Boolean): Option[A] = 
    this.flatMap(
      (a: A) => if(f(a)==true) Some(a) else None
    )

}

// built-in deconstructor of case class used in pattern matching
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {

  def unit[A](a: A): Option[A] = Some(a)


  def failingFn(i: Int): Int = {
    val y: Int = throw new Exception("fail!") // `val y: Int = ...` declares `y` as having type `Int`, and sets it equal to the right hand side of the `=`.
    try {
      val x = 42 + 5
      x + y
    }
    catch { case e: Exception => 43 } // A `catch` block is just a pattern matching block like the ones we've seen. `case e: Exception` is a pattern that matches any `Exception`, and it binds this value to the identifier `e`. The match returns the value 43.
  }

  def failingFn2(i: Int): Int = {
    try {
      val x = 42 + 5
      x + ((throw new Exception("fail!")): Int) // A thrown Exception can be given any type; here we're annotating it with the type `Int`
    }
    catch { case e: Exception => 43 }
  }

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)
  def variance(xs: Seq[Double]): Option[Double] = {
    if (xs.isEmpty) None
    else {
      val opMean: Option[Double] = mean(xs)
      val opVariance = opMean.map { (m: Double) => 
        val subMSquared: Seq[Double] = xs.map ( s => (s-m)*(s-m) )
        val sum: Double = subMSquared.foldLeft(0.0)(_+_)
        sum / xs.length
      
      }

      opVariance
    }
  }
  // def map2[A,B,C](aOp: Option[A], bOp: Option[B])(f: (A, B) => C): Option[C] = 
  //   (aOp, bOp) match {
  //     case (Some(a), Some(b)) => Some(f(a,b))
  //     case _ => None
  //   }

  def map2[A,B,C](aOp: Option[A], bOp: Option[B])(f: (A, B) => C): Option[C] = 
    aOp.flatMap(
      (a: A) => bOp.map((b: B) => f(a,b))
    )
/*
 Note that this would not work;
 it would produce Option[Option[C]].
    aOp.map(
      (a: A) => bOp.map(
        (b: B) => f(a,b)
      )
    )
 */

  /*

   Write a function sequence that combines a list of Options into one Option containing a list of all the Some values in the original list. If the original list contains None even once, the result of the function should be None; otherwise the result should be Some with a list of all the values. Here is its signature

This is a clear instance where it’s not appropriate to define the function in the OO style. This shouldn’t be a method on List (which shouldn’t need to know anything about Option), and it can’t be a method on Option, so it goes in the Option companion object.

   */

  /*
   Flatten the recursion with flatMap
   We want Option[List[A]]

   Cons(Some(a), Cons(Some(b), Cons(Some(c), Nil)))
   make up f
   sequence(Cons(Some(a), Cons(Some(b), Cons(Some(c), Nil))))
   Some(Cons(a, sequence(Cons(Some(b), Cons(Some(c), Nil)))))
   Some(Cons(a, Cons(b, sequence(Cons(Some(c), Nil)))))
   Some(Cons(a, Cons(b, Cons(c, Nil))))
   

   flatMap{
   (a: A]): Option[List[A]] =>
   a.match {

   }
   */

  def sequence[A](a: List[Option[A]]): Option[List[A]] = {

    // val flatten: (A => List[A]) = 
    //   a1 => Some(List(a1))

    // a match {
    //   case sh::t => sh.flatMap(flatten)  //sh = some head
    //   case None::t => None
    //   case Nil => Some(Nil)

    // }

    a.foldLeft(Some(List[A]()): Option[List[A]]) {
      (opListA: Option[List[A]], opA: Option[A]) =>
      opListA.flatMap { (listA: List[A]) =>
        opA.map { (a: A) => a::listA }
      }
    }: Option[List[A]]

  }


  /*
   Implement through traverse.
   traverse's types on left hand side
   B = A
   A = Option[A]
   */
  def sequenceThroughTraverse[A](a: List[Option[A]]): Option[List[A]] = 
    traverse(a){
      // output Option[A]
      (opA: Option[A]) => opA
    }

  def prepend[A](opA: Option[A], opListA: Option[List[A]]): Option[List[A]] =
    opA.flatMap { (a: A) =>
      opListA.map { (listA: List[A]) => a::listA }
    }

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = 
    a.foldRight(unit(List[B]())) { (a: A, opListB: Option[List[B]]) =>
      val opB: Option[B] = f(a)
      prepend(opB, opListB)
    }
}


object OptionTest extends App {
  import Option._

  val xs: Seq[Double] = (500 to 1300).toSeq.map(_ / 1000.0)

  val opVariance: Option[Double] = variance(xs)

  println("variance: " + opVariance)

  val empty = Seq[Double]()

  val opEmptyVariance = variance(empty)

  println("variance of an empty sequence: "+opEmptyVariance)

  println("sequence example")

  val listOpFailing = List(Some(1), None, Some(3))

  val opListFailing = sequence(listOpFailing)

  println("list of options: "+listOpFailing)

  println("option of list: "+opListFailing)

  val listOpSuccessful = List(Some(1), Some(2), Some(3))

  val opListSuccessful = sequence(listOpSuccessful)

  println("list of options: "+listOpSuccessful)

  println("Note the effect of fold left")
  println("option of lists: "+opListSuccessful)

  println("traverse example")

  val capAsciiCodes = List(65, 66, 67)

  def capitalAscii(i: Int): Option[Char] =
    if(i>=65 && i<=90) Some(i.toChar)
    else None

  println("cap ascii codes: "+capAsciiCodes)

  val capAscii = traverse(capAsciiCodes)(capitalAscii)

  println("cap ascii: "+capAscii)
    
  val mixedAsciiCodes = List(64, 65, 66)

  println("mixed ascii codes: "+mixedAsciiCodes)

  val mixedAscii = traverse(mixedAsciiCodes)(capitalAscii)

  println("mixed ascii: "+mixedAscii)

}
