package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {
  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(left, right) => 1 + size(left) + size(right)
  }

  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(value) => value
    case Branch(left, right) => (maximum(left)).max(maximum(right))
  }

  def depth[A](t: Tree[Int]): Int = t match {
    case Leaf(_) => 1
    case Branch(left, right) => 1 + (depth(left)).max(depth(right))
  }

  def map[A,B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(value) => Leaf(f(value))
    case Branch(left, right) => Branch(map(left)(f), map(right)(f))
  }


  /*
   A branch contains no A element to operate on; f is only called on leaves
   */
  // def fold[A,B](t: Tree[A], z: B)(f: (A,B) => B): B = t match {
  //   case Leaf(value) => f(value, z)
  //   case Branch(left, right) => {
  //     val leftZ = fold(left, z)(f)
  //     val rightZ = fold(right, leftZ)(f)
  //     rightZ
  //   }
  // }
  // def fold[A,B](t: Tree[A], z: B)(f: (Tree[A],B) => B): B = t match {
  //   case leaf: Leaf[A] => f(leaf, z) // returns B
  //   case branch: Branch[A] => { // returns B
  //     val thisB = 
  //     val leftB = fold(left, z)(f)
  //     val rightB = fold(right, leftZ)(f)
  //     rightZ
  //   }
  // }

  // def _size[A](t: Tree[A]): Int = fold(t, 0){
  //   (a: A, b: Int) => b + 1  // how to count parent Branch?
  // }
  // // how to get rid of sentinel value?
  // def _maximum(t: Tree[Int]):Int = fold(t, -1000){
  //   (a: Int, b: Int) => a.max(b)
  // }
  // def _depth[A](t: Tree[A]): Int = fold(t, 0){
  //   (a: A, maxDepth: Int) => 


  /* 
  Like `foldRight` for lists, `fold` receives a "handler" for each of the data constructors of the type, and recursively accumulates some value using these handlers. As with `foldRight`, `fold(t)(Leaf(_))(Branch(_,_)) == t`, and we can use this function to implement just about any recursive function that would otherwise be defined by pattern matching.
  */
  def fold[A,B](t: Tree[A])(f: A => B)(g: (B,B) => B): B = t match {
    case Leaf(a) => f(a)
    case Branch(l,r) => g(fold(l)(f)(g), fold(r)(f)(g))
  }
  
  def sizeViaFold[A](t: Tree[A]): Int = 
    fold(t)(a => 1)(1 + _ + _)
  
  def maximumViaFold(t: Tree[Int]): Int = 
    fold(t)(a => a)(_ max _)
  
  def depthViaFold[A](t: Tree[A]): Int = 
    fold(t)(a => 0)((d1,d2) => 1 + (d1 max d2))
  /*
  Note the type annotation required on the expression `Leaf(f(a))`. Without this annotation, we get an error like this: 
  
  type mismatch;
    found   : fpinscala.datastructures.Branch[B]
    required: fpinscala.datastructures.Leaf[B]
       fold(t)(a => Leaf(f(a)))(Branch(_,_))
                                      ^  
  
  This error is an unfortunate consequence of Scala using subtyping to encode algebraic data types. Without the annotation, the result type of the fold gets inferred as `Leaf[B]` and it is then expected that the second argument to `fold` will return `Leaf[B]`, which it doesn't (it returns `Branch[B]`). Really, we'd prefer Scala to infer `Tree[B]` as the result type in both cases. When working with algebraic data types in Scala, it's somewhat common to define helper functions that simply call the corresponding data constructors but give the less specific result type:  
    
   ****   Similar to Stream constructors in next chapter ****

    def leaf[A](a: A): Tree[A] = Leaf(a)
    def branch[A](l: Tree[A], r: Tree[A]): Tree[A] = Branch(l, r)
  */
  def mapViaFold[A,B](t: Tree[A])(f: A => B): Tree[B] = 
    fold(t)(a => Leaf(f(a)): Tree[B])(Branch(_,_))


  // watch out for an endless loop here
  // def flatMapViaFold[A,B](t: Tree[A])(f: A => Tree[B]): Tree[B] = 
  //   fold(t)(a => f(a): Tree[B])(Branch(_,_))



}
