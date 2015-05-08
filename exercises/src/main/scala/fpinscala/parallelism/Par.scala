package fpinscala.parallelism

import java.util.concurrent._
import scala.language.implicitConversions

object Par {
  type Par[A] = ExecutorService => Future[A]
  
  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = {
    println("run: "+s.toString())
    a(s)
  }

  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a) // `unit` is represented as a function that returns a `UnitFuture`, which is a simple implementation of `Future` that just wraps a constant value. It doesn't use the `ExecutorService` at all. It's always done and can't be cancelled. Its `get` method simply returns the value that we gave it.
  
  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true 
    def get(timeout: Long, units: TimeUnit) = get //Future.get
    def isCancelled = false 
    def cancel(evenIfRunning: Boolean): Boolean = false 
  }
  
  def map2[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C] = // `map2` doesn't evaluate the call to `f` in a separate logical thread, in accord with our design choice of having **** `fork` be the sole function in the API for controlling parallelism ****. We can always do `fork(map2(a,b)(f))` if we want the evaluation of `f` to occur in a separate thread.
    (es: ExecutorService) => {
      val af = a(es) 
      val bf = b(es)
      UnitFuture(f(af.get, bf.get)) 

      // This implementation of `map2` does _not_ respect timeouts, and eagerly waits for the returned futures. This means that even if you have passed in "forked" arguments, using this map2 on them will make them wait. It simply passes the `ExecutorService` on to both `Par` values, waits for the results of the Futures `af` and `bf`, applies `f` to them, and wraps them in a `UnitFuture`.

      // In order to respect timeouts, we'd need a new `Future` implementation that records the amount of time spent evaluating `af`, then subtracts that time from the available time allocated for evaluating `bf`.
    }
  
  def fork[A](a: => Par[A]): Par[A] = // This is the simplest and most natural implementation of `fork`, but there are some problems with it--for one, the outer `Callable` will block waiting for the "inner" task to complete. Since this blocking occupies a thread in our thread pool, or whatever resource backs the `ExecutorService`, this implies that we're losing out on some potential parallelism. Essentially, we're using two threads when one should suffice. This is a symptom of a more serious problem with the implementation, and we will discuss this later in the chapter.
  {
    (es: ExecutorService) => es.submit(new Callable[A] {
      // not appearing because in separate thread??
      // println(es.toString())
      def call: A = {
        //println(Thread.currentThread())
        a(es).get
      }
    }): Future[A]
  }: ExecutorService => Future[A]

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

  // does this require blocking?
  //def flatMap[A, B](pa: Par[A])(f: A => Par[B]): Par[B] =


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

  def split[A](par: Par[Tuple2[A,A]]): Tuple2[Par[A],Par[A]] = {
    val parLeft: Par[A] = this.map(par)((tpl: Tuple2[A,A]) => tpl._1)
    val parRight: Par[A] = this.map(par)((tpl: Tuple2[A,A]) => tpl._2)
    (parLeft, parRight)
  }


  /* Gives us infix syntax for `Par`.4 */
  implicit def toParOps[A](p: Par[A]): ParOps[A] = new ParOps(p)

  class ParOps[A](p: Par[A]) {


  }
}

object Examples {
  import Par._

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

        /*
        error made here
        [error]  found   : fpinscala.parallelism.Par.Par[Int]
        [error]     (which expands to)  java.util.concurrent.ExecutorService => java.util.concurrent.Future[Int]
        [error]  required: java.util.concurrent.Future[Int]

        //parHead // fulfills return type, Par[Int]

         inside of scope of (service) => {...},
         Future required to be returned.  Whoops...
         Error message leaves you to figure out its scope.

         */
        parHead(service): Future[Int]

      } else {
        val halfLength: Int = length / 2
        /*
         Use Par's combinators to recurse through the two
         IndexedSeqs, and then sum them.

         Pipeline:
         Par[Tuple2[IndexedSeq[Int], IndexedSeq[Int]]] => 
         Par[Tuple2[Int, Int]] =>
         Par[Int]
         
         */

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





        /*
         Obviously this would be a big side effect, but we can
         get away with it.

        */
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
        /* printing parMerged would require "getting" the 
         FutureInt.

         For some reason I feel this would be less efficient
         than our printing of the left and right Indexed Seqs
         */
        parMerged(service): Future[Int]
      }
    }
  }


  // def parSum2(parInts: Par[IndexedSeq[Int]]): Par[Int] = {

  //   // Instead of extracting length from Par[Seq],
  //   // figure out how to use combinators to do this
  //   // val length: Int = Par.map(parInts){
  //   //   (is: IndexedSeq[Int]) => is.size
  //   // }(service).get()

  //   val parLength: Par[Int] = Par.map(parInts){
  //     (is: IndexedSeq[Int]) => is.size
  //   }


  //   /*
  //    Not possible because inner return type of map2, Int,
  //    prevents us from recursing with Par!

  //    */
  //   Par.map2(parInts, parLength){
  //     (seqInts: IndexedSeq[Int], length: Int) => {
  //       if (length <= 1){
  //         seqInts.headOption.getOrElse(0)
  //       } else {
  //         val halfLength: Int = length / 2
  //         val split:
  //             Tuple2[IndexedSeq[Int], IndexedSeq[Int]] =
  //             seqInts.splitAt(halfLength)


  //         // adding these two together with '+' would
  //         // defeat the purpose of the exercise
  //         val seqLeft: IndexedSeq[Int] = split._1
  //         val seqRight: IndexedSeq[Int] = split._2


  //         // uses implicit conversion
  //         val parIntLeft: Par[Int] = parSum(seqLeft)
  //         val parIntRight: Par[Int] = parSum(seqRight)

  //         val parMerged: Par[Int] = Par.map2(
  //           parIntLeft, parIntRight
  //         ){
  //           (intLeft: Int, intRight: Int) => intLeft + intRight
  //         }

  //         //parMerged: Par[Int]
  //         // I don't think there is a way of getting this value
  //         // without an implicit (or instantiated) Executor
  //         // Both are "verboden"
  //         merged: Int
  //       }
  //     }: Int
  //   }: Par[Int]
  // }: Par[Int]

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

    val service = Executors.newFixedThreadPool(5)
    println(Thread.currentThread())
    val vec = (1 to 10).toVector
    println("no use of Par: " + Examples.sum(vec))

    val parInt: Par[Int] = Examples.parSum(vec)
    // start computation asynchronously
    val runParInt: Future[Int] = Par.run(service)(parInt)

    // block and wait for result with .get
    println("use of Par: " + runParInt.get())

    service.shutdown()
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
    service3.shutdown()

  }

}
