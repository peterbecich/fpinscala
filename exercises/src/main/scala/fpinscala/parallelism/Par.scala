package fpinscala.parallelism

import java.util.concurrent._

object Par {
  type Par[A] = ExecutorService => Future[A]
  
  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a) // `unit` is represented as a function that returns a `UnitFuture`, which is a simple implementation of `Future` that just wraps a constant value. It doesn't use the `ExecutorService` at all. It's always done and can't be cancelled. Its `get` method simply returns the value that we gave it.
  
  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true 
    def get(timeout: Long, units: TimeUnit) = get 
    def isCancelled = false 
    def cancel(evenIfRunning: Boolean): Boolean = false 
  }
  
  def map2[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C] = // `map2` doesn't evaluate the call to `f` in a separate logical thread, in accord with our design choice of having `fork` be the sole function in the API for controlling parallelism. We can always do `fork(map2(a,b)(f))` if we want the evaluation of `f` to occur in a separate thread.
    (es: ExecutorService) => {
      val af = a(es) 
      val bf = b(es)
      UnitFuture(f(af.get, bf.get)) 

      // This implementation of `map2` does _not_ respect timeouts, and eagerly waits for the returned futures. This means that even if you have passed in "forked" arguments, using this map2 on them will make them wait. It simply passes the `ExecutorService` on to both `Par` values, waits for the results of the Futures `af` and `bf`, applies `f` to them, and wraps them in a `UnitFuture`.

      // In order to respect timeouts, we'd need a new `Future` implementation that records the amount of time spent evaluating `af`, then subtracts that time from the available time allocated for evaluating `bf`.
    }
  
  def fork[A](a: => Par[A]): Par[A] = // This is the simplest and most natural implementation of `fork`, but there are some problems with it--for one, the outer `Callable` will block waiting for the "inner" task to complete. Since this blocking occupies a thread in our thread pool, or whatever resource backs the `ExecutorService`, this implies that we're losing out on some potential parallelism. Essentially, we're using two threads when one should suffice. This is a symptom of a more serious problem with the implementation, and we will discuss this later in the chapter.
    es => es.submit(new Callable[A] { 
      def call: A = a(es).get
    })

/*
  Java's library transcribed to Scala

  class ExecuterService {
    def submit[A](a: Callable[A]): Future[A]
  }
  trait Callable[A] { def call: A }
  trait Future[A] {
    def get: A
    def get(timeout: Long, unit: TimeUnit): A
    def cancel(evenIfRunning: Boolean): Boolean
    def isDone: Boolean
    def isCancelled: Boolean
  }
 */
  def map[A,B](pa: Par[A])(f: A => B): Par[B] = 
    map2(pa, unit(()))((a,_) => f(a))


  // def sequence[A](ps: List[Par[A]]): Par[List[A]] = {
  //   es => {
  //     ps.foldRight(Par.unit(List[A]())(es)){
  //       (pla: Par[List[A]], pa: Par[A]) => {
  //         Par.unit(pa(es).get() :: pla(es).get())(es)
  //       }
  //     }

  //   }
  // }

  def parMap[A,B](ps: List[A])(f: A=>B): Par[List[B]] = {
    Par.map(Par.unit(ps))(_.map(f))
    
  }

  def sortPar(parList: Par[List[Int]]) = map(parList)(_.sorted)

  def equal[A](e: ExecutorService)(p: Par[A], p2: Par[A]): Boolean = 
    p(e).get == p2(e).get

  def delay[A](fa: => Par[A]): Par[A] = 
    es => fa(es)

  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    es => 
      if (run(es)(cond).get) t(es) // Notice we are blocking on the result of `cond`.
      else f(es)

  // def split[A](par: Par[Tuple2[A,A]]): Tuple2[Par[A],Par[A]] = 
  //   es => {
  //     val (leftFuture, rightFuture) = par(es)
  //     (leftFuture, rightFuture)
  //   }
      

  /* Gives us infix syntax for `Par`.4 */
  implicit def toParOps[A](p: Par[A]): ParOps[A] = new ParOps(p)

  class ParOps[A](p: Par[A]) {


  }
}

object Examples {
  import Par._

  // does not use Parallel types
  def sum(ints: IndexedSeq[Int]): Int = // `IndexedSeq` is a superclass of random-access sequences like `Vector` in the standard library. Unlike lists, these sequences provide an efficient `splitAt` method for dividing them into two parts at a particular index.
    if (ints.size <= 1)
      ints.headOption getOrElse 0 // `headOption` is a method defined on all collections in Scala. We saw this function in chapter 3.
    else { 
      val (l,r) = ints.splitAt(ints.length/2) // Divide the sequence in half using the `splitAt` function.
      sum(l) + sum(r) // Recursively sum both halves and add the results together.
    }
  implicit def toParInts(ints: IndexedSeq[Int]): Par[IndexedSeq[Int]] = 
    Par.unit(ints)

  // def parSum(parInts: Par[IndexedSeq[Int]]): Par[Int] = {
  //   (service: ExecutorService) => {
  //     if (Par.map(parInts)((is: IndexedSeq[Int]) => is.size)(service).get() <= 1)
  //       Par.map(parInts)((is: IndexedSeq[Int]) => is.headOption.getOrElse(0))(service)

  //     else {
  //       val parSplit: Par[Tuple2[IndexedSeq[Int], IndexedSeq[Int]]] = 
  //         Par.map(parInts)(
  //           (is: IndexedSeq[Int]) => is.splitAt(
  //             Par.map(parInts)(_.size / 2)(service).get()
  //           )
  //         )



  //   }
  // }

  def main(args: Array[String]): Unit = {
    println(Examples.sum(1 to 10))

    // val vec = scala.collection.immutable.Vector(1 to 10)
    val vec = (1 to 10).toVector
    println(Examples.sum(vec))


  }

}
