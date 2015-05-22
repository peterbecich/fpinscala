package fpinscala.laziness

import Stream._
import fpinscala.monads.Monad
import fpinscala.monads.Functor

trait Stream[+A] {


  //foo
  //bar

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  /*
   Think about implementing fold left over a Stream.
   I think the same downside of List's fold left occurs here -- reversing.
   But you can't reverse an infinite stream...
   */
  // def foldLeft[B](z: => B)(f: (=> B, A) => B): B =
  //   this match {
  //     case Cons(h,t) => t().foldLeft(f(z,h()))(f)
  //     case _ => z
  //   }

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }
  def take(n: Int): fpinscala.laziness.Stream[A] = this match {
    case Empty => fpinscala.laziness.Stream.empty[A]
    case Cons(h, t) => Cons(h, ()=>take(n-1)) // is laziness preserved here?
  }

  def headOption: Option[A] = find((a: A) => true)
//  def _headOption: Option[A] = foldRight

  // could easily run forever
  def toList: List[A] = this match {
    case Empty => List[A]()
    case Cons(h, t) => h() :: t().toList
  }



  def drop(n: Int): fpinscala.laziness.Stream[A] = this match {
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
  // foldRight(=>fpinscala.laziness.Stream[B])((A, =>fpinscala.laziness.Stream[B])=>fpinscala.laziness.Stream[B])

  def map[B](f: A => B): fpinscala.laziness.Stream[B] = {
    def g(a: A, sb: => fpinscala.laziness.Stream[B]): fpinscala.laziness.Stream[B] =
      fpinscala.laziness.Stream.cons(f(a), map(f))
    //                                       ^ f(a) not calculated
    //                                until function called;
    //                                signature is: () => fpinscala.laziness.Stream[B]

    // 
    foldRight(empty[B])(g)
  }


  /*
   Learn about covariance, invariance and contravariance.
   An upper type bound T <: A declares that type variable T refers to a subtype of type A. 

   With fpinscala.laziness.Stream[A]:

   pattern type is incompatible with expected type;
   found   : fpinscala.laziness.Empty.type
   required: fpinscala.laziness.Stream[A]
   Note: Nothing <: A 
   (and fpinscala.laziness.Empty.type <:
   fpinscala.laziness.Stream[Nothing]), 
   but trait fpinscala.laziness.Stream is invariant in type A.
   You may wish to define A as +A instead. (SLS 4.5)

   With fpinscala.laziness.Stream[+A]:

   covariant type A occurs in contravariant position in type fpinscala.laziness.Stream[A] of value stream2
   def append(stream2: fpinscala.laziness.Stream[A]): fpinscala.laziness.Stream[A] = {
   ^
   stream2 being contravariant means:
   Given
   A is a supertype of B  (Number is a supertype of Double)
   A >: B

   fpinscala.laziness.Stream[A] is a supertype of fpinscala.laziness.Stream[B] 
   (fpinscala.laziness.Stream[Number] supertype of fpinscala.laziness.Stream[Double])
   fpinscala.laziness.Stream[A] >: fpinscala.laziness.Stream[B]

   An error is forcing stream2 to be contravariant
   stream2: fpinscala.laziness.Stream[A] is a ***subtype*** of 
   stream2: fpinscala.laziness.Stream[B]
   (stream2: fpinscala.laziness.Stream[Number] is a subtype of stream2: fpinscala.laziness.Stream[Double])

   stream2: fpinscala.laziness.Stream[A] <: stream2: fpinscala.laziness.Stream[B]
   
   */
  def append(stream2: fpinscala.laziness.Stream[A]):
      fpinscala.laziness.Stream[A] = {
    // type
    // (fpinscala.laziness.A,
    // scala.<byname>[fpinscala.laziness.Stream[A]]) =>
    // fpinscala.laziness.Stream[A]
    def f(a: A, sa: => fpinscala.laziness.Stream[A]):
        fpinscala.laziness.Stream[A] = 
      fpinscala.laziness.Stream.cons(a, sa)

    /*
    how is a lazy argument specified in an anonymous function?
    val f: (A, => fpinscala.laziness.Stream[A]) => fpinscala.laziness.Stream[A] = 
      (a: A, sa: => fpinscala.laziness.Stream[A]) => fpinscala.laziness.Stream.cons(a, sa)
    val f: (A, fpinscala.laziness.Stream[A]) => fpinscala.laziness.Stream[A] = 
      (a: A, sa: fpinscala.laziness.Stream[A]) => fpinscala.laziness.Stream.cons(a, sa)

    regarding foldRight below
    type
    (scala.<byname>[B]) =>
    (scala.Function2[A, <byname>[B], B]) =>
    fpinscala.laziness.B

    filled in with actual types
    type
    (scala.<byname>[fpinscala.laziness.Stream[A]]) =>
    (scala.Function2[A, <byname>[fpinscala.laziness.Stream[A]], fpinscala.laziness.Stream[A]]) =>
    fpinscala.laziness.Stream[A]
     */
    val out: fpinscala.laziness.Stream[A] = foldRight(stream2)(f)
    out
  }
  def flatMap[B](f: A => fpinscala.laziness.Stream[B]): fpinscala.laziness.Stream[B] = {
    def g(a: A, sb: => fpinscala.laziness.Stream[B]) = f(a).append(sb)
    foldRight(empty[B])(g)
  }

  def startsWith(sa2: fpinscala.laziness.Stream[A]): Boolean = {
    val sa1: fpinscala.laziness.Stream[A] = this
    /* perhaps references to Stream in this object have been intercepted
     by Collections' Stream:
     found: fpinscala.monads.Monad[scala.collection.immutable.Stream]  
     required: fpinscala.monads.Monad[fpinscala.laziness.Stream]

     
     */
    val streamMonad: Monad[fpinscala.laziness.Stream] =
      fpinscala.monads.Monad.streamMonad
    val product: fpinscala.laziness.Stream[Tuple2[A,A]] = streamMonad.product(sa1, sa2)
    product.forAll{
      (tpl: Tuple2[A,A]) => tpl._1 == tpl._2
    }
  }

  def zip[B](sb: fpinscala.laziness.Stream[B]): fpinscala.laziness.Stream[(A,B)] = {
    val sa: fpinscala.laziness.Stream[A] = this
    val streamMonad = fpinscala.monads.Monad.streamMonad
    val product: fpinscala.laziness.Stream[Tuple2[A,B]] = streamMonad.product(sa, sb)
    product
  }


}
case object Empty extends fpinscala.laziness.Stream[Nothing]
case class Cons[+C](h: () => C, t: () => fpinscala.laziness.Stream[C]) extends fpinscala.laziness.Stream[C]

object Stream {
  def cons[C](hd: => C, tl: => fpinscala.laziness.Stream[C]): fpinscala.laziness.Stream[C] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }
  /*
   ^^^^^^^^^
   Section 5.2.1

   We typically want to cache the values of a Cons node, once they are forced. If we use the Cons data constructor directly, for instance, this code will actually compute expensive(x) twice:

   val x = Cons(() => expensive(x), tl) 
   val h1 = x.headOption 
   val h2 = x.headOption 

   We typically avoid this problem by defining smart constructors, which is what we call a function for constructing a data type that ensures some additional invariant or provides a slightly different signature than the “real” constructors used for pattern matching. By convention, smart constructors typically lowercase the first letter of the corresponding data constructor. Here, our cons smart constructor takes care of memoizing the by-name arguments for the head and tail of the Cons. This is a common trick, and it ensures that our thunk will only do its work once, when forced for the first time. Subsequent forces will return the cached lazy val: 
   
   def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {   
     lazy val head = hd   
     lazy val tail = tl   
     Cons(() => head, () => tail)
   } 

   The empty smart constructor just returns Empty, but annotates Empty as a Stream[A], which is better for type inference in some cases.

   We can see how both smart constructors are used in the Stream.apply function.  Recall that Scala uses subtyping to represent data constructors, but we almost always want to infer Stream as the type, not Cons or Empty. Making smart constructors that return the base type is a common trick. 

   def apply[A](as: A*): Stream[A] =   
     if (as.isEmpty) empty   
     else cons(as.head, apply(as.tail: _*)) 

   Again, Scala takes care of wrapping the arguments to cons in thunks, so the as.head and apply(as.tail: _*) expressions won’t be evaluated until we force the Stream.
 */
  def empty[C]: fpinscala.laziness.Stream[C] = Empty

  def apply[A](as: A*): fpinscala.laziness.Stream[A] =
    if (as.isEmpty) empty 
    else cons(as.head, apply(as.tail: _*))

  val ones: fpinscala.laziness.Stream[Int] = fpinscala.laziness.Stream.cons(1, ones)
  def constant[A](a: A): fpinscala.laziness.Stream[A] = fpinscala.laziness.Stream.cons(a, constant(a))
  def from(n: Int): fpinscala.laziness.Stream[Int] = fpinscala.laziness.Stream.cons(n, from(n+1))
  def fibs: fpinscala.laziness.Stream[Int] = fibs(0,1)
  def fibs(n0: Int, n1: Int): fpinscala.laziness.Stream[Int] = fpinscala.laziness.Stream.cons(n0+n1, fibs(n1, n0+n1))

  // def fibs: Stream[Int] = 
  //   Stream.cons(0,
  //     Stream.cons(1,
 

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): fpinscala.laziness.Stream[A] =
    f(z) match {
      case None => empty[A]
      case Some((a, s)) => fpinscala.laziness.Stream.cons(a, unfold(s)(f))
    }

  def _constant[A](a: A): fpinscala.laziness.Stream[A] = unfold(a)((a1: A) => Some(a1,a1))
  def _from(n: Int): fpinscala.laziness.Stream[Int] = unfold(n)((n0: Int) => Some(n0, n0+1))
  def _fibs: fpinscala.laziness.Stream[Int] = unfold((0,1))(
    (tpl: (Int,Int)) => Some(tpl._1 + tpl._2, (tpl._2, tpl._1 + tpl._2))
  )



}
