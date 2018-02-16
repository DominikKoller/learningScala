// A class of objects to represent a set
object IntSetApp extends App {
  println("IntSet")
  var set1 = Set(2, 3, 4)
  var set2 = Set(4, 2, 3)
  println("Set1: " + set1)
  println("Set2: " + set1)
  println("size set 1: "+ set1.size)
  println(set1.subsetOf(set2))
  println(set1.subsetOf(Set(8,3,6,1)))

}

case class Set[A](){
  // TODO Ask why the private class is defined in the companion object
  // Which I changed

  // State: S : P(Int) (where "P" represents power set)
  // Init: S = {}
  // Abstraction function:
  // State: S = {x | x <- [head.next, head.next.next, ... , null)

  // Datatype invariant:
  // head = Node(null.instanceOf[A], x) where x = null or x element of S

  // reason to use dummy header node:
  // it is convenient to have a findPrev function
  // in which case it is inconvenient to have null as the empty set
  private val head: Node = Node(null.asInstanceOf[A], null) // emptyset
  // TODO ask when I can use _ and when I should use null

  private def foreach(f: A=>Unit): Unit = {
    var i: Node = head.next

    while(i!=null) {
      f(i.datum)
      i=i.next
    }
  }

  // Using findPrev to find the node before some condition holds,
  // since this makes removing elements much easier
  private def findPrev(e: A): Node = findPrev(i => i == e)

  private def findPrev(p: A=>Boolean): Node = {
    var i = head
    while(i.next != null && !p(i.next.datum))
      i = i.next
    i
  }

  // Convert the set to a string.
  // Post: S = S0
  override def toString : String = {
    var result = "{"

    foreach(e => result+= e+ ", ")

    // cut last comma
    if(result != "{")
      result = result.dropRight(2)

    result += "}"
    result
  }

  /** Add element e to the set
    * Post: S = S_0 U {e}
    * returns e not element of S_0*/
  def add(e: A): Boolean = {
    if(!contains(e)) {
      unsafeAdd(e)
      true
    }
    else
      false
  }

  // Adding without checking for doubles
  // Pre: e not element of S
  // Post: S = S_0 U {e}
  private def unsafeAdd(e: A): Unit = {
    head.next = Node(e, head.next)
  }

  /** Length of the list
    * Post: S = S_0 && returns #S */
  def size : Int = {
    var i = 0
    foreach(_=> i+=1)
    i
  }

  /** Does the set contain e?
    * Post: S = S_0 && returns (e in S) */
  def contains(e: A) : Boolean = {
    val prev = findPrev(e)
    prev.next != null
  }

  /** Return any member of the set.  (This is comparable to the operation
    * "head" on scala.collection.mutable.Set, but we'll use a name that does
    * not suggest a particular order.)
    * Changed conditions to use Option type:
    * Pre: no preconditions
    * Post: S = S_0 &&
    * if S == {} returns None
    * Else returns Some(e) s.t. e in S */
  def any : Option[A] = {
    if(head.next != null)
      Some(head.next.datum)
    else None
  }

  /** Does this equal that?
    * Post: S = S_0 && returns that.S = S */
  // There is probably a faster implementation of this
  // also: this fails to check whether the generic types match!
  // TODO find out how to fix this
  override def equals(that: Any) : Boolean = that match {
    case s: Set[A] =>
      subsetOf(s) && s.subsetOf(this)
    case _ => false
  }

  /** Remove e from the set; result says whether e was in the set initially
    * Post S = S_0 - {e} && returns (e in S_0) */
  def remove(e: A) : Boolean = {
    val prev = findPrev(e)
    if(prev.next == null)
      false
    else {
      prev.next = prev.next.next
      true
    }
  }

  /** Test whether this is a subset of that.
    * Post S = S_0 && returns S subset-of that.S */
  def subsetOf(that: Set[A]) : Boolean = {
    forAll(i => that.contains(i))
  }

  def forAll(p: A => Boolean): Boolean = {
    !forAny(i => !p(i))
  }

  def forAny(p: A => Boolean): Boolean = {
    val prev = findPrev(e => p(e))
    prev.next != null
  }

  // ----- optional parts below here -----

  def copy() : Set[A] = {
    val result = new Set[A]()
    foreach(i => result.unsafeAdd(i))
    result
  }

  /** return union of this and that.
    * Post: S = S_0 && returns res s.t. res.S = this U that.S */
  def union(that: Set[A]) : Set[A] = {
    val result = copy()
    that.foreach(i => result.add(i))
    result
  }

  /** return intersection of this and that.
    * Post: S = S_0 && returns res s.t. res.S = this intersect that.S */
  def intersect(that: Set[A]) : Set[A] = {
    val result = new Set[A]()
    foreach(i => {
      if (that.contains(i))
        result.unsafeAdd(i)
    })
    result
  }

  /** map
    * Post: S = S_0 && returns res s.t. res.S = {f(x) | x <- S} */
  def map[B](f: A => B): Set[B] = {
    val result = new Set[B]()
    foreach(e => result.add(f(e))) // do not use unsafeadd! Might map to same element
    result
  }

  /** filter
    * Post: S = S_0 && returns res s.t. res.S = {x | x <- S && p(x)} */
  def filter(p : A => Boolean) : Set[A] = {
    val result = new Set[A]()
    foreach(e => if (p(e)) result.unsafeAdd(e))
    result
  }

  /** The type of nodes defined in the linked list */
  private case class Node(datum: A, var next: Node)
}

// The companion object
object Set{

  // TODO try to imitate the scala Collection Framework with collections LinkedList and BinaryTree
//  private trait Collection[A] {
//    def foreach(f: A => Unit): Unit
//
//    def map[B] (f: A=> B): Collection[B] // sorry cannot help it, should be whateverthistypeis[B]
//    def filter(p: A=> Boolean): this.type
//
//    def += (e: A): this.type
//    def -= (e: A): this.type
//
//    def size(): Int = {
//      var i=0;
//      foreach(_ => i+=1)
//      i
//    }
//  }

  /** Factory method for sets.
    * This will allow us to write, for example, IntSet(3,5,1) to
    * create a new set containing 3, 5, 1 -- once we have defined
    * the main constructor and the add operation.
    * post: returns res s.t. res.S = {x1, x2,...,xn}
    *       where xs = [x1, x2,...,xn ] */
  def apply[A](xs: A*) : Set[A] = {
    val s = new Set[A]; for(x <- xs) s.add(x); s
  }
}