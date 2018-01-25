// A class of objects to represent a set
object IntSetApp extends App {
  println("IntSet")
  var set = IntSet(2, 3, 4)
  println(set)
  println("size: "+ set.size)
}
// TODO Invariants, Class Abstraction, justification for decisions
// TODO concistency: some things are in the linkedlist with recursion, some in the supertype with loops

class IntSet{

  // The following lines just define some aliases, so we can subsequently
  // write "Node" rather than "IntSet.Node".
  // TODO Ask why the private class is defined in the companion object
  private type Node = IntSet.Node
  // Constructor
  // TODO understand. What?
  private def Node(datum: Int, next: Node) = new IntSet.Node(datum, next)

  // private secondary constructor
  private def this(r: IntSet.Node) = {
    this()
    root = r
  }
  // State: S : P(Int) (where "P" represents power set)

  // Init: S = {}
  private var root : Node = null // the set is empty

  // Convert the set to a string.
  override def toString : String = {
    var result = "{"

    foreach(i => result+= i +", ")

    // cut last comma
    if(result != "{")
      result = result.dropRight(2)

    result += "}"
    result
    // TODO Invariant
  }

  /** Add element e to the set
    * Post: S = S_0 U {e} */
  def add(e: Int) = {
    if(!contains(e))
      root = new Node(e, root)
  }

  // Adding without checking for doubles
  private def unsafeAdd(e: Int) = {
    root = new Node(e, root)
  }

  /** Length of the list
    * Post: S = S_0 && returns #S */
  def size : Int = {
    // could be implemented in the linkedList

    var i = 0;
    foreach(_ => i+=1)
    i
  }

  private def foreach(f: Int => Unit, breakCondition: () => Boolean): Unit = {
    var iterator = root
    while(iterator != null && !breakCondition()){
      f(iterator.datum)
      iterator = iterator.next
    }
  }

  private def foreach(f:Int=>Unit): Unit = foreach(f, () => false)

  /** Does the set contain e?
    * Post: S = S_0 && returns (e in S) */
  def contains(e: Int) : Boolean = {
    var found = false
    foreach(i => found = i==e, () => found)
    found
  }

  /** Return any member of the set.  (This is comparable to the operation
    * "head" on scala.collection.mutable.Set, but we'll use a name that does
    * not suggest a particular order.)
    * Pre: S != {}
    * Post: S = S_0 && returns e s.t. e in S */
  def any : Option[Int] = {
    if(root != null)
      return Some(root.datum)
    None
  }

  /** Does this equal that?
    * Post: S = S_0 && returns that.S = S */
  override def equals(that: Any) : Boolean = that match {
    case s: IntSet =>
      subsetOf(s) && s.subsetOf(this)
    case _ => false
  }

  /** Remove e from the set; result says whether e was in the set initially
    * Post S = S_0 - {e} && returns (e in S_0) */
  def remove(e: Int) : Boolean = {
    var current = root
    var previous: Node = null
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
  def subsetOf(that: IntSet) : Boolean = {
    forAll(i => that.contains(i))
  }

  def forAll(p: Int => Boolean): Boolean = {
    var foundFalse = false
    foreach(i => foundFalse = !p(i), () => foundFalse)
    !foundFalse
  }

  def forAny(p: Int => Boolean): Boolean = {
    !forAll(i => !p(i))
  }

  // ----- optional parts below here -----

  def copy() : IntSet = new IntSet(root.copy())

  /** return union of this and that.  
    * Post: S = S_0 && returns res s.t. res.S = this U that.S */
  def union(that: IntSet) : IntSet = {
    val result = copy()
    that.foreach(i => result.add(i))
    result
  }

  /** return intersection of this and that.  
    * Post: S = S_0 && returns res s.t. res.S = this intersect that.S */
  def intersect(that: IntSet) : IntSet = {
    val result = new IntSet()
    foreach(i => {
      if (that.contains(i))
        result.unsafeAdd(i)
    })
    result
  }

  /** map
    * Post: S = S_0 && returns res s.t. res.S = {f(x) | x <- S} */
  def map(f: Int => Int): IntSet = new IntSet(root.map(f))

  def mutatingMap(f: Int => Int) : Unit = root.mutatingMap(f)

  /** filter
    * Post: S = S_0 && returns res s.t. res.S = {x | x <- S && p(x)} */
  def filter(p : Int => Boolean) : IntSet = new IntSet(root.filter(p))
}

// The companion object
object IntSet{
  /** The type of nodes defined in the linked list */
  private class Node(var datum: Int, var next: Node){
    def copy() : Node = {
      if(next == null)
        new Node(datum, null)
      else
        new Node(datum, next.copy())
    }

    def map(f: Int => Int): Node = {
      if(next != null)
        new Node(f(datum), next.map(f))
      else
        new Node(f(datum), null)
    }

    def mutatingMap(f: Int => Int): Unit = {
      datum = f(datum)
      if(next != null)
        next.mutatingMap(f)
    }

    def mutatingMap(f: Int => Int, breakCondition: () => Boolean): Unit = {
      datum = f(datum)
      if(next != null && !breakCondition())
        next.mutatingMap(f, breakCondition)
    }

    def foreach(f: Int => Unit, breakCondition: () => Boolean): Unit = {
      f(datum)
      if(next != null && !breakCondition())
        next.foreach(f, breakCondition)
    }

    // could be slightly more efficient with own implementation
    def foreach(f:Int=>Unit): Unit = {
      f(datum)
      if(next != null)
        next.foreach(f)
    }

    def filter(p: Int=>Boolean): Node = {
      if(next != null)
        if(p(datum))
          new Node(datum, next.filter(p))
        else
          next.filter(p)
      else if(p(datum))
          new Node(datum, null)
      else  null
    }
  }

  /** Factory method for sets.
    * This will allow us to write, for example, IntSet(3,5,1) to
    * create a new set containing 3, 5, 1 -- once we have defined 
    * the main constructor and the add operation. 
    * post: returns res s.t. res.S = {x1, x2,...,xn}
    *       where xs = [x1, x2,...,xn ] */
  def apply(xs: Int*) : IntSet = {
    val s = new IntSet; for(x <- xs) s.add(x); s
  }
}
