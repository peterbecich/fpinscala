package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
case class Cons[+A](head: A, tail: List[A]) extends List[A] // Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`, which may be `Nil` or another `Cons`.

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  } 
  
  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }
  
  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42 
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101 
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }
  // def _append[A](a1: List[A], a2: List[A]): List[A] = 
  //   foldLeft(a1, Nil: List[A]){
  //     // f: (List[A], A) => List[A]
  //     (la2: List[A], a: A) => Cons(a, la2)
  //   }

  def passThru[A](l: List[A]): List[A] =
    foldLeft(l, Nil: List[A]){
      (la: List[A], a: A) => Cons[A](a, la)
    }

  def reverse[A](l: List[A]): List[A] =
    foldLeft(l, Nil: List[A]){
      (la: List[A], a: A) => Cons[A](a, la)
    }


  def passThruFoldRight[A](l: List[A]): List[A] =
    foldRight(l, Nil: List[A]){
      (a: A, la: List[A]) => Cons[A](a, la)
    }


  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }
  
  def sum2(ns: List[Int]) = 
    foldRight(ns, 0)((x,y) => x + y)
  
  def product2(ns: List[Double]) = 
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] = l match {
    // unreachable code
    // case Cons(h, Nil) => Cons(h, Nil)

    // case Cons(h, t) => tail(t)
    // case Cons(h, Nil) => Cons(h, Nil)
    // case Nil => Nil

    case Cons(h, Nil) => Cons(h, Nil)
    case Cons(h, t) => tail(t)
    case Nil => Nil

  }


  def setHead[A](l: List[A], h: A): List[A] = Cons(h, l)

  def drop[A](l: List[A], n: Int): List[A] = l match {
    case Cons(h, t) if n>0 => drop(t, n-1)
    case Cons(h, t) if n==0 => Cons(h, t)
    case Nil => Nil
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(h, t) if f(h)==true => dropWhile(t, f)
    case Cons(h, t) if f(h)==false => Cons(h, t)
    case Nil => Nil
  }

  // get last element
  def init[A](l: List[A]): List[A] = l match {
    case Cons(h, Nil) => Cons(h, Nil)
    case Cons(h, t) => init(t)
    case Nil => Nil
  }

  def length[A](l: List[A]): Int = foldLeft(l, 0)((count: Int, element: A) => {count+1})

  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Cons(h, t) => {
      // not tail recursive
      // val z1 = f(z, h)
      // Cons(z1, foldLeft(t, z1)(f))

      // apply f before entering next iteration of foldLeft

      // symbolically, it would appear that every recursion has a tail,
      // albeit "heavier" or "lighter"

      // apparently, though, the compiler can figure out when a parent foldLeft
      // never needs to be accessed again
      foldLeft(t, f(z, h))(f)
    }
    case Nil => z
  }


  // The type annotation Nil:List[Int] is needed here, because otherwise Scala infers the B type parameter in foldRight as List[Nothing].


  def map[A,B](l: List[A])(f: A => B): List[B] =
    reverse(foldLeft(l, Nil: List[B]){
      (lb: List[B], a: A) => Cons[B](f(a), lb)
    })

  def mapByFoldRight[A,B](l: List[A])(f: A => B): List[B] =
    foldRight(l, Nil: List[B]){
      (a: A, lb: List[B]) => Cons[B](f(a), lb)
    }


  def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] =
    foldRight(l, Nil: List[B]){
      // make ":::" list concatenation syntactic sugar explicit
      // besides, that's only implemented for Std Library lists!
      (a: A, lb: List[B]) => List.append(f(a), lb)
    }

}

object TestList {

  // doesn't give desired result with "variadic" argument to method
  //val listA = List.apply(50 to 64)

  val listA = List.apply(65, 66, 67, 68, 69, 70, 85)

  val list1 = List.apply(65, 66, 67)
  val list2 = List.apply(68, 69, 70, 85)

  def main(args: Array[String]): Unit = {
    println(listA)

    val letters = List.map(listA)(_.toChar)

    println(letters)

    println("pass thru by fold left")
    println(List.passThru(listA))

    println("pass thru by fold left, reversed")
    println(List.reverse(List.passThru(listA)))


    println("pass thru by fold right")
    //println(List.foldRight(listA, Nil:List[Integer
    println(List.passThruFoldRight(listA))


    println("letters by map")
    println(List.map(listA)(_.toChar))

    println("letters by map fold right")
    println(List.mapByFoldRight(listA)(_.toChar))

    println("flatMap")
    println(
      List.flatMap(listA)(
        (i: Int) => List.apply(i, i+1, i+2)
      )
    )


  }

}


