/*
import org.scalatest._
//import scala.collection.mutable.ListBuffer
// built.sbt adds scalatest for specific files only

class Q1Tests extends FunSuite {
    test ("HashSetAdd") {
      val S = new scala.collection.mutable.HashSet[Int]
      assert(S.add(1))
      assert(S.contains(1))
      assert(!S.add(1))
      assert(S.contains(1))
      S.add(2)
      assert(S.size === 2)
      assert(S.contains(1))
      assert(S.contains(2))
      assert(! S.contains(3))

      // We cannot tell a difference when we've added an element twice
      // But we can check what add returns
    }

    test ("HashSetRemove") {
      val S = new scala.collection.mutable.HashSet[Int]
      S.add(1)
      S.remove(1)
      assert(S.size === 0)
      assert(! S.contains(1))
    }

    test ("HashSetContains") {
      val S = new scala.collection.mutable.HashSet[Int]
      assert(! S.contains(1))
      S.add(1)
      assert(S.contains(1))
      assert(! S.contains(2))
      S.add(2)
      assert(S.contains(2))
      assert(! S.contains(3))
      S.add(3)
      assert(S.contains(3))
    }

    test ("HashSetSizeAndEmpty") {
      val S = new scala.collection.mutable.HashSet[Int]
      assert(S.size === 0)
      assert(S.isEmpty)
      S.add(1)
      assert(S.size === 1)
      assert(!S.isEmpty && S.nonEmpty)
      S.add(2)
      assert(S.size === 2)
      assert(!S.isEmpty && S.nonEmpty)
      S.remove(2)
      assert(S.size === 1)
      assert(!S.isEmpty && S.nonEmpty)
      S.remove(1)
      assert(S.size === 0)
      assert(S.isEmpty)
    }

    // I'm not sure what is meant by 'when a precondition is violated'
    // Which precondition? Intercept which exception? HashSet doesn't throw exceptions as far as I know
    // Is the point that the test should make sure HashSet doesn't throw exceptions?
    // like so

    test ("exceptionWhatIAmConfused") {
      try {
        val S = new scala.collection.mutable.HashSet[Int]
        S.add(3)
        S.remove(4)
      }
      catch {
        case _:Exception =>
          fail("I can write extra messages here")
      }
      // but that would be very useless since the test fails anyway whenever an exception is thrown
    }
  }

object Sheet4 extends App {

  //Q2

  // state: [A]
  // init: state = []
  trait Stack[A] {
    // Add an element
    // push(x) = x:state
    def push(x:A): Unit
    // remove and return the most recently added element
    // pop: returns head state
    //      state = tail state
    // Precondition: state != []
    def pop(): A
    def isEmpty: Boolean
  }
  // Q3
  // a)
  // change state to S: {x| xePInt ^ 0<=x<N}
  // add Pre to add: 0<=elem<N

  // b)
  /** state: S : {x| xePInt ^ 0<=x<N}
    * init: S = {} */
  trait IntSetRange{
    /** returns the maximum element that can be a member of the set
      */
    def maxElem: Int
    /** Add elem to the set.
      * post: S = S0 ∪ {elem} */
    def add(elem: Int)
    /** Does the set contain elem?
      * pre: 0<=elem<N
      * post: S = S0 ∧ returns elem ∈ S */
    def isIn(elem: Int): Boolean
    /** Remove elem from the set.
      * post: S = S0 − {elem} */
    def remove(elem: Int)
    /** The size of the set.
      * post: S = S0 ∧ returns #S */
    def size : Int
  }

  class IntBitmap(val maxElem: Int) extends IntSetRange {

    private val FData = Array.fill[Boolean](maxElem-1)(false)

    def add(elem: Int): Unit = {
      FData(elem) = true
    }

    // Q4
    def head: Int = {
      for(i <- 0 to FData.length)
        if(FData(i))
          return i
      throw new IllegalStateException("Head called on Empty Set")
    }

    def isIn(elem: Int): Boolean = FData(elem)

    def remove(elem: Int): Unit = {
      FData(elem) = false
    }
    def size : Int = {
      var counter = 0
      for(i <- 0 to FData.length)
        if(FData(i))
          counter += 1
      counter
    }
  }
  // TODO Q3 Bitmap tests

  // a)
  // There is no definition of first in a set

  // b)
  // Should read: returns some element of this set
  // or: returns some x s.t. xeS
  // 'First' assumes something about the implementation
  // Also pre: S != {}

  // c) see above

  // Q5

  /** The state of a phone book, mapping names (Strings) to numbers (also
    * Strings).
    * state: book : String → String
    * init: book = {} */
  trait Book{
    /** Add the maplet name -> number to the mapping.
      * post: book = book0 ⊕ {name → number} */
    def store(name: String, number: String)
    /** Return the number stored against name.
      * pre: name ∈ dom book
      * post: book = book0 ∧ returns book(name) */
    def recall(name: String) : String
    /** Is name in the book?
      * post: book = book0 ∧ returns name ∈ dom book */
    def isInBook(name: String) : Boolean
    /** Delete the number stored against name (if it exists)
      * post: book = book0\{name} ^ returns whether name ∈ dom book0
      * */
    def delete(name: String) : Boolean
  }

  // Representing the phone book using a pair of arrays

  object ArraysBook extends Book{
    private val MAX = 1000 // max number of names we can store
    private val names = new Array[String](MAX)
    private val numbers = new Array[String](MAX)
    private var count = 0
    // These variables together represent the mapping
    // { names(i) -> numbers(i) | i <- [0..count) }
    // invariant: count <= MAX &&
    // entries in names[0..count) are distinct

    // Return the index i<count s.t. names(i) = name; or
    //              return count if no such index exists
    private def find(name: String) : Int = {
      // Invariant: name not in names[0..i) && i <= count
      var i = 0
      while(i < count && names(i) != name) i += 1
      i
    }

    /** Return the number stored against name */
    def recall(name: String) : String = {
      val i = find(name)
      assert(i<count)
      numbers(i)
    }
    /** Is name in the book? */
    def isInBook(name: String) : Boolean = find(name)<count


    /** Add the maplet name -> number to the mapping */
    def store(name: String, number: String) = {
      val i = find(name)
      if(i == count){
        assert(count < MAX); names(i) = name; count += 1
      }
      numbers(i) = number
    }

    def delete(name: String): Boolean = {
      val deleteIndex = find(name)
      if(deleteIndex == count)
        return false

      for(i <- deleteIndex until count-1){
        names(i) = names(i+1)
        numbers(i) = numbers(i+1)
      }
      count = count-1
      names(count) = null
      numbers(count) = null
      true
    }
  }

  // Q6

  object SortedArraysBook extends Book{
    private val MAX = 1000 // max number of names we can store
    private val names = Array.fill[Option[String]](MAX)(None)
    private val numbers = Array.fill[Option[String]](MAX)(None)
    private var count = 0
    // These variables together represent the mapping
    // { names(i) -> numbers(i) | i <- [0..count) }
    // invariant: count <= MAX &&
    // entries in names[0..count) are distinct and ordered

    // Return the index i<count s.t. names(i) = name; or
    //              return count if no such index exists
    private def find(name: String) : Option[Int] = {
      // Binary search
      // I: names[0..p) < name < names(q..count)
      var p = 0
      var q = count
      var i = 0
      while(q-p>1) {
        i = p+(p-q)/2
        if(names(i).get == name)
          return Some(i)
        else if (names(i).get > name)
          q = i
        else // names(i) < name
          p = i
      }
      None
    }

    /** Return the number stored against name */
    def recall(name: String) : Option[String] = find(name).map(i => numbers(i))

    /** Is name in the book? */
    def isInBook(name: String) : Boolean = find(name).isDefined

    /** Add the maplet name -> number to the mapping */
    def store(name: String, number: String): Unit = {
      find(name).foreach(n => {
        assert(count < MAX)
        for (i <- count - 1 to n + 1 by -1) {
          names(i) = names(i - 1)
          numbers(i) = numbers(i - 1)
        }
        names(n) = Some(name)
        numbers(n) = Some(name)
        count += 1
      })
    }

    def delete(name: String): Boolean = {
      return find(name) match {
          case Some(n) => {
            for(i <- n until count-1){
              names(i) = names(i+1)
              numbers(i) = numbers(i+1)
            }
            count = count-1
            names(count) = None
            numbers(count) = None
                true
              }
          case None => false
      }
    }
  }
  // TODO O notation of q6

  // Q7
  // state: bag : Int -> Int
  //        dom(bag) = [0..MAX)
  // Init: {}
  trait TIntegerBag {
    // the maximum integer you can put in the bag
    // Post: bag = bag0
    def max: Int
    // returns bag(x)
    // Post: bag = bag0
    def get(x: Int): Int
    // Post: bag = bag0\{r| r=(x,_)} U {(x,bag0(x))}
    def add(x: Int)
  }

  class IntegerBag(val max: Int) extends TIntegerBag {
    private val v = Array.fill[Int](max)(0);
    def add(x:Int):Unit = v(x) += 1
    def get(x:Int):Int = v(x)
  }

  // Q8
  // count occurrences of each number and only then write the result back

  def sortRange(A: Array[Int], max: Int): Unit = {
    val bag = new IntegerBag(max)
    for(n <- A)
      bag.add(n)
    var counter = 0
    for(i <- 0 until max)
      for(j <-  0 to bag(i)) {
        A(i+j) = i
      }
  }
}

// TODO tests, invariables

*/