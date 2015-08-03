package fpinscala.localeffects

import fpinscala.monads._

object Mutable {
  def quicksort(xs: List[Int]): List[Int] = if (xs.isEmpty) xs else {
    val arr = xs.toArray
    def swap(x: Int, y: Int) = {
      val tmp = arr(x)
      arr(x) = arr(y)
      arr(y) = tmp
    }
    def partition(l: Int, r: Int, pivot: Int) = {
      val pivotVal = arr(pivot)
      swap(pivot, r)
      var j = l
      for (i <- l until r) if (arr(i) < pivotVal) {
        swap(i, j)
        j += 1
      }
      swap(j, r)
      j
    }
    def qs(l: Int, r: Int): Unit = if (l < r) {
      val pi = partition(l, r, l + (r - l) / 2)
      qs(l, pi - 1)
      qs(pi + 1, r)
    }
    qs(0, arr.length - 1)
    arr.toList
  }
}


// a monad with primitives unit and flatMap
sealed trait ST[S,A] { self =>
  // A protected member is only accessible from subclasses of the class in which the member is defined. 
  protected def run(s: S): (A,S)
  def map[B](f: A => B): ST[S,B] = new ST[S,B] {
    def run(s: S) = {
      val (a, s1) = self.run(s)
      (f(a), s1)
    }
  }
  def flatMap[B](f: A => ST[S,B]): ST[S,B] = new ST[S,B] {
    def run(s: S) = {
      val (a, s1) = self.run(s)
      f(a).run(s1)
    }
  }
}

object ST {
  def apply[S,A](a: => A): ST[S,A] = {
    lazy val memo = a
    new ST[S,A] {
      def run(s: S) = (memo, s)
    }
  }
  def runST[A](st: RunnableST[A]): A =
    st[Null].run(null)._1
}

sealed trait STRef[S,A] {
  protected var cell: A
  def read: ST[S,A] = ST(cell)
  def write(a: => A): ST[S,Unit] = new ST[S,Unit] {
    def run(s: S) = {
      cell = a
      ((), s)
    }
  }
}

object STRef {
  def apply[S,A](a: A): ST[S, STRef[S,A]] = ST(new STRef[S,A] {
    var cell = a
  })
}

trait RunnableST[A] {
  def apply[S]: ST[S,A]
}

// Scala requires an implicit Manifest for constructing arrays.
sealed abstract class STArray[S,A](implicit manifest: Manifest[A]) {
  protected def array: Array[A]
  def stSize: ST[S,Int] = ST(array.size)

  // Write a value at the give index of the array
  def write(i: Int, a: A): ST[S,Unit] = new ST[S,Unit] {
    def run(s: S): (Unit,S) = {
      array(i) = a
      ((), s)
    }
  }

  // Read the value at the given index of the array
  def read(i: Int): ST[S,A] = ST(array(i))

  // Turn the array into an immutable list
  def freeze: ST[S,List[A]] = ST(array.toList)
  // freezeArray seems to garble output
  def freezeArray: ST[S,Array[A]] = ST(array)

  // fill an existing array, up to this existing array's size
  def fill(xs: Map[Int,A]): ST[S,Unit] = {
    //val mapSize: Int = xs.size
    //xs.foldLeft(z: B)(op: Function2[B, Tuple2[Int, A], B])

    val initialEffect: ST[S,Unit] = ST(())
    val foldEffect: ST[S,Unit] =
      xs.foldLeft(initialEffect){
        (effect: ST[S,Unit], keyValue: (Int,A)) => {
          val key = keyValue._1
          val value = keyValue._2
          stSize.flatMap{(size: Int) =>
            if(key<size)
              effect.flatMap(_ => write(key,value))
            else effect
          }
        }
      }
    foldEffect
  }
 //   STArray.fromList(xs.values.toList)


  def swap(i: Int, j: Int): ST[S,Unit] =
    read(i).flatMap(x =>
      read(j).flatMap(y =>
        write(i, y).flatMap(_ =>
          write(j, x)
        )
      )
    )

  // for {
  //   x <- read(i)
  //   y <- read(j)
  //   _ <- write(i, y)
  //   _ <- write(j, x)
  // } yield ()
}

object STArray {
  // Construct an array of the given size filled with the value v
  def apply[S,A:Manifest](sz: Int, v: A): ST[S, STArray[S,A]] =
    ST(new STArray[S,A] {
      lazy val array = Array.fill(sz)(v)
    })

  def fromList[S,A:Manifest](xs: List[A]): ST[S, STArray[S,A]] =
    ST(new STArray[S,A] {
      lazy val array = xs.toArray
    })

  // def partition[S](arr: STArray[S,Int],
  //   n: Int, r: Int, pivot: Int): ST[S,Int]

  // def qs[S](a: STArray[S,Int], n: Int, r: Int): ST[S,Unit]


}

import scala.collection.mutable.HashMap
sealed abstract class STHashMap[S,K,V](
  implicit manifestK: Manifest[K], manifestV: Manifest[V]
) {
  protected def hashmap: HashMap[K,V]
  def stSize: ST[S,Int] = ST(hashmap.size)
  def write(key: K, value: V): ST[S,Unit] = new ST[S,Unit] {
    def run(s: S) = {
      hashmap(key) = value
      ((), s)
    }
  }

  def read(key: K): ST[S,Option[V]] = ST {
    hashmap.get(key)
  }

  def freeze: ST[S,HashMap[K,V]] = ST {
    hashmap
  }

  // unlike 'fill' on STArray, expand the hash map if necessary
  def fill(map: Map[K,V]): ST[S,Unit] = {
    val initialEffect: ST[S,Unit] = ST(())
    val foldEffect: ST[S,Unit] =
      map.foldLeft(initialEffect){
        (effect: ST[S,Unit], keyValue: (K,V)) => {
          val key = keyValue._1
          val value = keyValue._2
          effect.flatMap(_ => write(key,value))
        }
      }
    foldEffect
  }
  
}


object Immutable {
  def noop[S] = ST[S,Unit](())

  def partition[S](a: STArray[S,Int], l: Int, r: Int, pivot: Int): ST[S,Int] = ???

  def qs[S](a: STArray[S,Int], l: Int, r: Int): ST[S, Unit] = ???

  // def quicksort(xs: List[Int]): List[Int] =
  //   if (xs.isEmpty) xs else ST.runST(new RunnableST[List[Int]] {
  //     def apply[S] = for {
  //       arr    <- STArray.fromList(xs)
  //       size   <- arr.size
  //       _      <- qs(arr, 0, size - 1)
  //       sorted <- arr.freeze
  //     } yield sorted
  // })
  def quicksort(xs: List[Int]): List[Int] =
    if (xs.isEmpty) xs else ST.runST(new RunnableST[List[Int]] {
      def apply[S] =
        STArray.fromList(xs).flatMap(arr =>
          arr.stSize.flatMap(size =>
            qs(arr, 0, size-1).flatMap(_ =>
              arr.freeze.flatMap(sorted =>
                ST(sorted)
              )
            )
          )
        )
    }
    )


}

import scala.collection.mutable.HashMap

object STTests {
  import ST._
  import STRef._
  //import fpinscala.localeffects.RunnableST

  def main(args: Array[String]): Unit = {
    println("state tag, state token, state thread, or state transition")

    // page 259
    // fpinscala.localeffects.ST[Nothing, Tuple2[Int, Int]]
    val out = for {
      r1 <- STRef[Nothing,Int](1)
      r2 <- STRef[Nothing,Int](1)
      x <- r1.read
      y <- r2.read
      _ <- r1.write(y+1)
      _ <- r2.write(x+1)
      a <- r1.read
      b <- r2.read
    } yield (a,b)
    println(out)


    // fpinscala.localeffects.ST[Nothing, STRef[Nothing, Tuple2[Int, Int]]]
    val out2 =
      STRef[Nothing,Int](1).flatMap(r1 =>
        STRef[Nothing,Int](1).flatMap(r2 =>
          r1.read.flatMap(x =>
            r2.read.flatMap(y =>
              r1.write(y+1).flatMap(_ =>
                r2.write(x+1).flatMap(_ =>
                r1.read.flatMap(a =>
                  r2.read.flatMap(b => STRef.apply((a,b)))
                )
              )
              )
            )
          )
        )
      )
    println("example on page 259 with for-comp made explicit")
    println(out2)
    //println(out2.read)

    println("runnable ST")
    val p = new RunnableST[(Int,Int)] {
      def apply[S]: ST[S,(Int,Int)] =
        STRef(1).flatMap(r1 =>
          STRef(2).flatMap(r2 =>
            r1.read.flatMap(x =>
              r2.read.flatMap(y =>
                r1.write(y+1).flatMap(_ =>
                  r2.write(x+1).flatMap(_ =>
                    r1.read.flatMap(a =>
                      r2.read.flatMap(b =>
                        ST((a,b))
                      )
                    )
                  )
                )
              )
            )
          )
        )
    }
    println(p)
    println("calling ST.runST(p)")
    val out3: (Int,Int) = ST.runST(p)
    println(out3)

    
    
  }
}

object STArrayTests {
  import ST._
  import STRef._
  import STArray._
  import Immutable._
  //import fpinscala.localeffects.RunnableST


  val chars = (65 to 79).map(_.toChar).toList
  val simpleMap = Map(1->'a',5->'p',2->'q')
  //val st: ST[Nothing, STArray[Nothing, Char]] = fromList(chars)
  def st[S] = STArray.fromList[S,Char](chars)

  /*
   simple scope enforcement example

   def Variable[W]

   object Foo {
     def container[X] = {
       val cannotEscape = Variable[X]
       // X does not exist outside of the container
     }
   }


   */

  def main(args: Array[String]): Unit = {
    println("filling an STArray with a map")
    println("map: ")
    println(simpleMap)
    println("list turned into STArray:")
    println(chars)
    println("ST containing STArray:")
    println(st)
    // to print the array, we need
    // ST[Nothing, STArray[Nothing, Char]] =>
    //   ST[Nothing, Array[Char]]
    //val runnablePrint1 = printArr(st)
    val runnablePrint1 = new RunnableST[Array[Char]] {
      def apply[P]: ST[P,Array[Char]] = {
        val stArr: ST[P,Array[Char]] = st.flatMap(stArr => stArr.freezeArray)
        stArr
      }
    }
    val runnablePrint2 = new RunnableST[List[Char]] {
      def apply[P]: ST[P,List[Char]] = {
        st.flatMap(stArr => stArr.freeze)
      }
    }

    println("using a runnable to extract array; ST[...,Array[Char]].freezeArr")
    val arr: Array[Char] = ST.runST(runnablePrint1)
    println(arr)

    println("extract list; ST[...,Array[Char]].freeze")
    val list: List[Char] = ST.runST(runnablePrint2)
    println(list)
    // method run in trait ST cannot be accessed in fpinscala.localeffects.ST[Nothing,Unit]  Access to protected method run not permitted because  enclosing object STArrayTests in package localeffects is not a subclass of   trait ST in package localeffects where target is defined
    // frozen1.flatMap(listChar => ST(println(listChar))).run





    println("filled")
    // val stFilled = st.flatMap(stArr => stArr.fill(simpleMap))
    // println(stFilled)

    // val runnablePrint3 = new RunnableST[List[Char]] {
    //   def apply[P]: ST[P,List[Char]] = {
    //     st.flatMap(stArr => stArr.freeze)
    //     // ^^ note, reference to original ST persists
    //     // not using stFilled
    //   }
    // }

    // val filledList = ST.runST(runnablePrint3)

    // println(filledList)


    println("---------------------------")
    println("STArray and quick sort")
    import scala.util.Random
    val random = Seq.fill(10)(Random.nextInt(20))
    println("sort this:")
    println(random)
    // val sorted = quicksort(random.toList)
    // println(sorted)
  }
}


