package fpinscala.parallelism

import java.util.concurrent.{Callable, CountDownLatch, ExecutorService}
import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent.ExecutorService
import java.util.concurrent.Executors

object Nonblocking {

  trait Future[+A] {
    // Unit (side effect) not visible to user of Par
    private[parallelism] def apply(k: A => Unit): Unit
  }

  type Par[+A] = ExecutorService => Future[A]

  object Par {

    // Blocks
    def run[A](es: ExecutorService)(p: Par[A]): A = {
      val ref = new java.util.concurrent.atomic.AtomicReference[A] // A mutable, threadsafe reference, to use for storing the result
      val latch = new CountDownLatch(1) // A latch which, when decremented, implies that `ref` has the result
      p(es) { a => ref.set(a); latch.countDown } // Asynchronously set the result, and decrement the latch
      latch.await // Block until the `latch.countDown` is invoked asynchronously
        ref.get // Once we've passed the latch, we know `ref` has been set, and return its value
    }

    def unit[A](a: A): Par[A] =
      es => new Future[A] {
        def apply(cb: A => Unit): Unit =
          cb(a)
      }

    /** A non-strict version of `unit` */
    def delay[A](a: => A): Par[A] =
      es => new Future[A] {
        def apply(cb: A => Unit): Unit =
          cb(a)
      }

    def fork[A](a: => Par[A]): Par[A] =
      es => new Future[A] {
        def apply(cb: A => Unit): Unit =
          eval(es)(a(es)(cb))
      }

    /**
     * Helper function for constructing `Par` values out of calls to non-blocking continuation-passing-style APIs.
     * This will come in handy in Chapter 13.
     */
    def async[A](f: (A => Unit) => Unit): Par[A] = es => new Future[A] {
      def apply(k: A => Unit) = f(k)
    }

    /**
     * Helper function, for evaluating an action
     * asynchronously, using the given `ExecutorService`.
     */
    def eval(es: ExecutorService)(r: => Unit): Unit =
      es.submit(new Callable[Unit] { def call = r })


    def map2[A,B,C](p: Par[A], p2: Par[B])(f: (A,B) => C): Par[C] =
      es => new Future[C] {
        def apply(cb: C => Unit): Unit = {
          var ar: Option[A] = None
          var br: Option[B] = None
          val combiner = Actor[Either[A,B]](es) {
            case Left(a) =>
              if (br.isDefined) eval(es)(cb(f(a,br.get)))
              else ar = Some(a)
            case Right(b) =>
              if (ar.isDefined) eval(es)(cb(f(ar.get,b)))
              else br = Some(b)
          }
          p(es)(a => combiner ! Left(a))
          p2(es)(b => combiner ! Right(b))
        }
      }

    // specialized version of `map`
    def map[A,B](p: Par[A])(f: A => B): Par[B] =
      es => new Future[B] {
        def apply(cb: B => Unit): Unit =
          p(es)(a => eval(es) { cb(f(a)) })
      }

    def lazyUnit[A](a: => A): Par[A] =
      fork(unit(a))

    def asyncF[A,B](f: A => B): A => Par[B] =
      a => lazyUnit(f(a))

    def sequenceRight[A](as: List[Par[A]]): Par[List[A]] =
      as match {
        case Nil => unit(Nil)
        case h :: t => map2(h, fork(sequence(t)))(_ :: _)
      }

    def sequenceBalanced[A](as: IndexedSeq[Par[A]]): Par[IndexedSeq[A]] = fork {
      if (as.isEmpty) unit(Vector())
      else if (as.length == 1) map(as.head)(a => Vector(a))
      else {
        val (l,r) = as.splitAt(as.length/2)
        map2(sequenceBalanced(l), sequenceBalanced(r))(_ ++ _)
      }
    }

    def sequence[A](as: List[Par[A]]): Par[List[A]] =
      map(sequenceBalanced(as.toIndexedSeq))(_.toList)

    // exercise answers

    /*
     * We can implement `choice` as a new primitive.
     *
     * `p(es)(result => ...)` for some `ExecutorService`, `es`, and
     * some `Par`, `p`, is the idiom for running `p`, and registering
     * a callback to be invoked when its result is available. The
     * result will be bound to `result` in the function passed to
     * `p(es)`.
     *
     * If you find this code difficult to follow, you may want to
     * write down the type of each subexpression and follow the types
     * through the implementation. What is the type of `p(es)`? What
     * about `t(es)`? What about `t(es)(cb)`?
     */
    def choice[A](p: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
      es => new Future[A] {
        def apply(cb: A => Unit): Unit =
          p(es) { b =>
            if (b) eval(es) { t(es)(cb) }
            else eval(es) { f(es)(cb) }
          }
      }

    def choiceN[A](p: Par[Int])(ps: List[Par[A]]): Par[A] = ???

    def choiceViaChoiceN[A](a: Par[Boolean])(ifTrue: Par[A], ifFalse: Par[A]): Par[A] =
      ???

    def choiceMap[K,V](p: Par[K])(ps: Map[K,Par[V]]): Par[V] =
      ???

    // see `Nonblocking.scala` answers file. This function is usually called something else!
    def chooser[A,B](p: Par[A])(f: A => Par[B]): Par[B] =
      ???

    def flatMap[A,B](p: Par[A])(f: A => Par[B]): Par[B] =
      ???

    def choiceViaChooser[A](p: Par[Boolean])(f: Par[A], t: Par[A]): Par[A] =
      ???

    def choiceNChooser[A](p: Par[Int])(choices: List[Par[A]]): Par[A] =
      ???

    def join[A](p: Par[Par[A]]): Par[A] =
      ???

    def joinViaFlatMap[A](a: Par[Par[A]]): Par[A] =
      ???

    def flatMapViaJoin[A,B](p: Par[A])(f: A => Par[B]): Par[B] =
      ???

    /* Gives us infix syntax for `Par`. */
    implicit def toParOps[A](p: Par[A]): ParOps[A] = new ParOps(p)

    // infix versions of `map`, `map2`
    class ParOps[A](p: Par[A]) {
      def map[B](f: A => B): Par[B] = Par.map(p)(f)
      def map2[B,C](b: Par[B])(f: (A,B) => C): Par[C] = Par.map2(p,b)(f)
      def zip[B](b: Par[B]): Par[(A,B)] = p.map2(b)((_,_))
    }
  }

  object Examples {

    // does not use Parallel types
    def sum(ints: IndexedSeq[Int]): Int =
      if (ints.size <= 1)
        ints.headOption getOrElse 0
      else {
        val (l,r) = ints.splitAt(ints.length/2)
        sum(l) + sum(r)
      }
    implicit def toParInts(ints: IndexedSeq[Int]): Par[IndexedSeq[Int]] =
      Par.unit(ints)

    def parSum(parInts: Par[IndexedSeq[Int]]): Par[Int] = {
      (service: ExecutorService) => {
        println("thread "+Thread.currentThread().getId())
        // block to get length of parInts Par[IndexedSeq]
        // It's a first step that must be waited for
        // with this method of summation, by recursively splitting
        val length: Int = Par.map(parInts){
          (is: IndexedSeq[Int]) => is.size
        }(service).get()

        if(length <= 1){
          val parHead: Par[Int] = Par.map(parInts){
            (is: IndexedSeq[Int]) =>
            is.headOption.getOrElse(0)
          }

          parHead(service): Future[Int]

        } else {
          val halfLength: Int = length / 2

          val parSplit:
              Par[Tuple2[IndexedSeq[Int], IndexedSeq[Int]]] =
            Par.map(parInts){
              (is: IndexedSeq[Int]) => is.splitAt(halfLength)
            }

          val parSeqLeft: Par[IndexedSeq[Int]] = Par.map(parSplit){
            (tup: Tuple2[IndexedSeq[Int], IndexedSeq[Int]]) =>
            tup._1
          }
          val parSeqRight: Par[IndexedSeq[Int]] = Par.map(parSplit){
            (tup: Tuple2[IndexedSeq[Int], IndexedSeq[Int]]) =>
            tup._2
          }

          Par.map2(parSeqLeft, parSeqRight){
            (seqLeft: IndexedSeq[Int],
              seqRight: IndexedSeq[Int]) => {
              println("left: "+seqLeft+"\t right: "+seqRight)
            }
          }(service) // .get() not necessary

          val parIntLeft: Par[Int] = parSum(parSeqLeft)
          val parIntRight: Par[Int] = parSum(parSeqRight)

          val parMerged: Par[Int] = Par.map2(
            parIntLeft, parIntRight
          ){
            (intLeft: Int, intRight: Int) => intLeft + intRight
          }
          parMerged(service): Future[Int]
        }
      }
    }

    // use fork in this method
    def parSum3(parInts: Par[IndexedSeq[Int]]): Par[Int] = {
      (service: ExecutorService) => {
        println("thread "+Thread.currentThread().getId())
        val length: Int = Par.map(parInts){
          (is: IndexedSeq[Int]) => is.size
        }(service).get()

        if(length <= 1){
          val parHead: Par[Int] = Par.map(parInts){
            (is: IndexedSeq[Int]) =>
            is.headOption.getOrElse(0)
          }

          parHead(service): Future[Int]

        } else {
          val halfLength: Int = length / 2
          val parSplit:
              Par[Tuple2[IndexedSeq[Int], IndexedSeq[Int]]] =
            Par.map(parInts){
              (is: IndexedSeq[Int]) => is.splitAt(halfLength)
            }

          val parSeqLeft: Par[IndexedSeq[Int]] = Par.map(parSplit){
            (tup: Tuple2[IndexedSeq[Int], IndexedSeq[Int]]) =>
            tup._1
          }
          val parSeqRight: Par[IndexedSeq[Int]] = fork( Par.map(parSplit){
            (tup: Tuple2[IndexedSeq[Int], IndexedSeq[Int]]) =>
            tup._2
          }
          )

          fork(Par.map2(parSeqLeft, parSeqRight){
            (seqLeft: IndexedSeq[Int],
              seqRight: IndexedSeq[Int]) => {
              println("left: "+seqLeft+"\t right: "+seqRight)
            }
          }
          )(service)


          val parIntLeft: Par[Int] = parSum3(parSeqLeft)
          val parIntRight: Par[Int] = fork(parSum3(parSeqRight))

          val parMerged: Par[Int] = Par.map2(
            parIntLeft, parIntRight
          ){
            (intLeft: Int, intRight: Int) => intLeft + intRight
          }

          parMerged(service): Future[Int]
        }
      }
    }



    def main(args: Array[String]): Unit = {

      println("non-blocking Par implementation examples")
      val service = Executors.newFixedThreadPool(5)
      println(Thread.currentThread())
      val vec = (1 to 10).toVector
      println("no use of Par: " + Examples.sum(vec))

      val parInt: Par[Int] = Examples.parSum(vec)
      // start computation asynchronously
      val runParInt: Future[Int] = Par.run(service)(parInt)

      // block and wait for result with .get
      println("use of Par: " + runParInt.get())

      // doesn't freeze with many threads!  The reason is
      // highly informative...

      val service3 = Executors.newFixedThreadPool(50)

      val parInt3: Par[Int] = Par.fork(Examples.parSum3(vec))
      // start computation asynchronously
      val runParInt3: Future[Int] = Par.run(service3)(parInt3)

      // block and wait for result with .get
      println("use of Par with fork: " + runParInt3.get())
      // it freezes on this print statement, when all the work
      // is done!

    }


  }
}
