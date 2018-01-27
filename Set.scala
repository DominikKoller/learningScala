// A class of objects to represent a set
object IntSetApp extends App {
  println("IntSet")
  var set = Set(2, 3, 4)
  println(set)
  println("size: "+ set.size)
}
// TODO Invariants, Class Abstraction, justification for decisions

class Set[A]{

  // The following lines just define some aliases, so we can subsequently
  // write "Node" rather than "IntSet.Node".
  // TODO Ask why the private class is defined in the companion object
  private type Node[A] = Set.Node[A]
  // Constructor
  // TODO understand. What?
  // private def IntList(datum: Int, next: IntList) = new IntSet.LinkedList(datum, next)

  // private secondary constructor
  private def this(root: Set.Node[A]) = {
    this()
    this.root = root
  }
  // State: S : P(Int) (where "P" represents power set)

  // Init: S = {}
  private var root: Node[A] = null // emptyset

  private def foreach(f: A=>Unit): Unit = {
    var iterator: Node[A] = root

    while(iterator!=null) {
      f(iterator.datum)
      iterator=iterator.next
    }
  }

  // Convert the set to a string.
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
    * Post: S = S_0 U {e} */
  def add(e: A) = {
    if(!contains(e))
      root = new Node(e, root)
  }

  // Adding without checking for doubles
  private def unsafeAdd(e: A) = {
    root = new Node(e, root)
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
    var found = false
    // TODO breakable
    foreach(i => found = found || i==e)
    found
  }

  /** Return any member of the set.  (This is comparable to the operation
    * "head" on scala.collection.mutable.Set, but we'll use a name that does
    * not suggest a particular order.)
    * Pre: S != {}
    * Post: S = S_0 && returns e s.t. e in S */
  def any : Option[A] = {
    if(root != null)
      Some(root.datum)
    else None
  }

  /** Does this equal that?
    * Post: S = S_0 && returns that.S = S */
  override def equals(that: Any) : Boolean = that match {
    case s: Set[A] =>
      subsetOf(s) && s.subsetOf(this)
    case _ => false
  }

  /** Remove e from the set; result says whether e was in the set initially
    * Post S = S_0 - {e} && returns (e in S_0) */
  def remove(e: Int) : Boolean = {
    var current = root
    var previous: Node[A] = null
    var found = false

    while(current != null && !found){
      if(current.datum == e){
        previous.next = current.next
        current = current.next
        found = true
      }
      else {
        previous = current
        current = current.next
      }
    }
    found
  }

  /** Test whether this is a subset of that.
    * Post S = S_0 && returns S subset-of that.S */
  def subsetOf(that: Set[A]) : Boolean = {
    forAll(i => that.contains(i))
  }

  def forAll(p: A => Boolean): Boolean = {
    var foundFalse = false
    // TODO breakable
    foreach(i => foundFalse = foundFalse || !p(i))
    !foundFalse
  }

  def forAny(p: A => Boolean): Boolean = {
    !forAll(i => !p(i))
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
    foreach(e => result.unsafeAdd(f(e)))
    result
  }

  /** filter
    * Post: S = S_0 && returns res s.t. res.S = {x | x <- S && p(x)} */
  def filter(p : A => Boolean) : Set[A] = {
    val result = new Set[A]()
    foreach(e => if (p(e)) result.unsafeAdd(e))
    result
  }
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

  /** The type of nodes defined in the linked list */
  private class Node[A](val datum: A, var next: Node[A])

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
