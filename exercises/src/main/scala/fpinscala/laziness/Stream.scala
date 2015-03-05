package fpinscala.laziness

import Stream._
trait Stream[+A] {


  //foo
  //bar

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }
  // def take(n: Int): Stream[A] = this match {
  //   case Empty => Stream.empty[A]
  //   case Cons(h, t) => {


  def headOption: Option[A] = find((a: A) => true)
//  def _headOption: Option[A] = foldRight

  // could easily run forever
  def toList: List[A] = this match {
    case Empty => List[A]()
    case Cons(h, t) => h() :: t().toList
  }



  def drop(n: Int): Stream[A] = this match {
    case Empty => empty[A]
    case Cons(h, t) if n>0 => t().drop(n-1)
    case Cons(h, t) => t()
  }



  def forAll(p: A => Boolean): Boolean = foldRight(true)(
    (a, b) => p(a) && b
  )

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.


  // use fold right
  // foldRight(=>Stream[B])((A, =>Stream[B])=>Stream[B])

  // def map[B](f: A => B): Stream[B] = foldRight(empty[B])(
  //   (a, b) => Cons[B](f(a), b.map(f))
  // )

  def startsWith[B](s: Stream[B]): Boolean = sys.error("todo")
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty 
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)
  def constant[A](a: A): Stream[A] = Stream.cons(a, constant(a))
  def from(n: Int): Stream[Int] = Stream.cons(n, from(n+1))
  def fibs: Stream[Int] = fibs(0,1)
  def fibs(n0: Int, n1: Int): Stream[Int] = Stream.cons(n0+n1, fibs(n1, n0+n1))

  // def fibs: Stream[Int] = 
  //   Stream.cons(0,
  //     Stream.cons(1,
 

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) match {
      case None => empty[A]
      case Some((a, s)) => Stream.cons(a, unfold(s)(f))
    }

  def _constant[A](a: A): Stream[A] = unfold(a)((a1: A) => Some(a1,a1))
  def _from(n: Int): Stream[Int] = unfold(n)((n0: Int) => Some(n0, n0+1))
  def _fibs: Stream[Int] = unfold((0,1))(
    (tpl: (Int,Int)) => Some(tpl._1 + tpl._2, (tpl._2, tpl._1 + tpl._2))
  )



}
