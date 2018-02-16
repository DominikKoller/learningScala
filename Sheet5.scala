import org.scalatest._
//import scala.collection.mutable.ListBuffer
// built.sbt adds scalatest for specific files only

class Node(val datum: Int, var next: Node) {
  override def toString : String = {
    if (next == null)
      datum.toString
    else
      datum.toString + " -> " + next.toString
  }
}

object Q1 extends App {
  // Q1 a)
  var myList: Node = _
  for(i <- 0 to 12)
    myList = new Node(i, myList)
  // println(myList)
  // 0 -> 1 -> 2 -> 3 -> 4 -> 5 -> 6 -> 7 -> 8 -> 9 -> 10 -> 11 -> 12

  // c)
  // It is impossible to write an operation that 'reverses a list' (as opposed to 'returns a list reversed')
  // with this implementation of linked list
  // as that would mean changing the reference of the input parameter.
  // hence I will write a method that takes a list and returns a reversed version of the list, without creating new nodes
  def reverse(head: Node): Node = {
    var iterator: Node = head
    var newList:  Node = null

    // I: reverse(newList) ++ iterator == head
    // V: iterator.length
    // Pre: newList == null, iterator == head
    while(iterator != null) {
      val next = iterator.next
      iterator.next = newList
      newList = iterator
      iterator = next
    }
    // Post: reverse(newList) ++ iterator == head && iterator.length == 0
    //       => reverse(newList) == head
    //       => newList == reverse(head)
    newList
  }
  println(myList)
  // 12 -> 11 -> 10 -> 9 -> 8 -> 7 -> 6 -> 5 -> 4 -> 3 -> 2 -> 1 -> 0
  println(reverse(myList))
  // 0 -> 1 -> 2 -> 3 -> 4 -> 5 -> 6 -> 7 -> 8 -> 9 -> 10 -> 11 -> 12
}

// Q2

// Change store to
// /** Add the maplet name -> number to the mapping */
// def store(name: String, number: String): Unit = {
//   val n = find(name)
//
//   if(n.next == null) // store new info at the end of current list:
//     n.next = new LinkedListHeaderBook.Node(name, number, null)
//   else
//     n.next.number = number
// }

// Q3

// Book trait from the lectures

// The interface to the phone book
// Note I slightly changed the postcondition for add

// Each implementation of this trait represents a mapping from names
// (Strings) to numbers (also Strings).
trait Book{
  // State: book : String -|-> String
  // Init:  book = {}

  // Add the maplet name -> number to the mapping
  // POST: (book = book_0 \ {prev | prev = name -> _}) Union {name -> number}
  def store(name:String, number:String)

  // Return the number stored against name
  // PRE: name in dom book
  // POST book = book_0 && returns book(name)
  def recall(name:String) : String

  // Is name in the book?
  // POST: book = book_0 && returns name in dom book
  def isInBook(name: String): Boolean
}

class PhoneBook extends Book {

  override def toString: String = {
    if (head.next == null)
      "You got no friends"
    else
      head.next.toString
  }
  // State: book : String -|-> String
  // Init:  book = {}
  private val head:Node = new Node(Entry("?", "?"), null)

  private def findPrev(name: String): Node = {
    var i = head
    while (i.next != null && i.next.entry.name < name)
      i = i.next
    i
  }

  // Add the maplet name -> number to the mapping
  // POST: (book = book_0 \ {prev | prev = name -> _}) Union {name -> number}
  def store(name:String, number:String): Unit = {
    val prev = findPrev(name)
    if(prev.next == null)
      prev.next = new Node(Entry(name, number), null)
    else if (prev.next.entry.name == name)
      prev.next.entry.number = number
    else {
      val newNode = new Node(Entry(name, number), prev.next)
      prev.next = newNode
    }
  }

  // Return the number stored against name
  // PRE: name in dom book
  // POST book = book_0 && returns book(name)
  def recall(name:String) : String = {
    val prev = findPrev(name)
    assert(prev.next != null && prev.next.entry.name == name)
    prev.next.entry.number
  }

  // Is name in the book?
  // POST: book = book_0 && returns name in dom book
  def isInBook(name: String): Boolean = {
    val prev = findPrev(name)
    prev.next != null && prev.next.entry.name == name
  }

  // POST: book = book_0 \ {entry | entry = name -> number for any number}
  //        returns {name -> _} element of book_0
  def delete(name: String): Boolean = {
    val prev = findPrev(name)
    if (prev.next != null && prev.next.entry.name == name){
      prev.next = prev.next.next
      true
    }
    else
      false
  }

  // Tutorial question:
  // is there a reason to put private classes into the companion object?
  private case class Entry(name:String, var number: String) extends Ordered[Entry] {
    def compare(that: Entry): Int = name.compare(that.name)
    // tutorial question: is there a more elegant way to do this?
  }

  // I'm not sure this is the nicest way to do this, but I wanted to try out case classes
  // this was not very compatible for using a while loop, but I would like to find out what a good
  // scala design pattern would be for this

  // private abstract class Node
  // private case class HeaderNode(var next: Node) extends Node
  // private case class ValueNode(var entry: Entry, var next:Node) extends Node
  // private case class LastNode(var entry: Entry) extends Node

  private class Node(var entry: Entry, var next:Node) {
    override def toString : String = {
      if (next == null)
        entry.toString
      else
        entry.toString + " -> " + next.toString
    }
  }
}

object Q3 extends App {
  var book = new PhoneBook()
  book.store("Aaron", "999")
  book.store("Xaver", "333")
  println(book)
  // Entry(Aaron,999) -> Entry(Xaver,333)
  book = new PhoneBook()
  book.store("Xaver", "333")
  book.store("Aaron", "999")
  println(book.isInBook("Aaron"))
  // true
  println(book)
  // Entry(Aaron,999) -> Entry(Xaver,333)
  book.delete("Xaver")
  println(book.isInBook("Aaron"))
  // true
  println(book)
  // Entry(Aaron,999)
  book.delete("Aaron")
  println(book)
  // You got no friends
  println(book.isInBook("Aaron"))
  // false
}

// Q4
// a)
// The expected work to be done = 1*p0 + 2*p1 + ... + n*p(n-1) + n(1-(p0+p1+...+p(n-1)))
// which is minimized if higher p(i) terms are aligned with lower factors
// hence if p0 >= p1 >= ... >= p(n-1)
// are we supposed to give a rigorous proof for that? How?

// b)

// An implementation without a header dummy node and
// a (not necessarily sorted) linked list
// Find the node previous to the one to be recalled.
// Save prev.next as foundNode
// Delete prev.next in the list
// set foundNode.next to the list header
// set the list header to foundNode

// like so:
// val prev = findPrev(name)
// val foundNode = prev.next
// prev.next = foundNode.next //deleting
// foundNode.next = listHeader
// listHeader = foundNode

// Q5

// abstraction function:
// if end >= start
//    state: q = data[start..end)
// else
//    state: q = data[start..max) ++ data[0..end)

// also, add to pre
// q.length < max
class ArrayQueue extends Queue[Int] {
  val max = 100
  var start = 0
  var end = 0
  var data = new Array[Int](max)

  def enqueue(x: Int): Unit = {
    val newEnd = (end+1)%max
    assert(newEnd != start)
    data(newEnd) = x
    end = newEnd
  }
  def dequeue: Int = {
    assert(!isEmpty)
    val currentStart = start
    start = (start+1)%max
    data(currentStart)
  }
  def isEmpty: Boolean = start == end
  def isFull: Boolean = (end+1)%max != start
}

/** A queue of data of type A.
  * state: q : seq A
  * init: q = [] */
trait Queue[A]{
  /** Add x to the back of the queue
    * post: q = q0 ++ [x] */
  def enqueue(x: A)
  /** Remove and return the first element.
    * pre: q 6= []
    * post: q = tail q0 ∧ returns head q0
    * or post: returns x s.t. q0 = [x] ++ q0 */
  def dequeue: A
  /** Is the queue empty?
    * post: q = q0 ∧ returns q = [] */
  def isEmpty: Boolean
}

// Q6
// I will make a generic queue instead

// Abstraction function:
// state: q = [head, head.next, head.next.next, ... , null)
class myQueue[A] extends Queue[A] {
  private var head:Node = _
  private var tail:Node = _

  def enqueue(x: A): Unit = {
    if(head == null) {
      tail = Node(x, null)
      head = tail
    }
    else {
      tail.next = Node(x, null)
      tail = tail.next
    }
  }

  def dequeue: A = {
    assert(head != null)
    val value = head.value
    head = head.next
    if(head == null)
      tail = null
    value
  }

  def isEmpty: Boolean = head == null

  override def toString: String = {
    if(head == null)
      "emptyqueue"
    else
      head.toString
  }

  private case class Node(value: A, var next: Node) {
    override def toString: String = {
      if(next == null)
        value.toString
      else
        value.toString + " -> " + next.toString
    }
  }
  //private abstract class Node(var next: Node)
  //private case class HeaderNode(override var next: ValueNode) extends Node(next)
  //private case class ValueNode(value: A, override var next: ValueNode) extends Node(next)

  // again I could not find a satisfying way to work with case classes here
}

object Q6 extends App {
  val queue = new myQueue[Int]()
  println(queue)
  // emptyqueue
  queue.enqueue(12)
  println(queue)
  // 12
  queue.enqueue(532)
  // 12 -> 532
  println(queue)
  println("dequeue: " + queue.dequeue)
  // dequeue: 12
  println(queue)
  // 532
  println("dequeue: " + queue.dequeue)
  // dequeue: 532
  println(queue)
  // emptyqueue
}

// Q7

// State: q: seq A
// init: q = []

// Invariant:
// if q == [], head = tail = null
// else head = head q
//      tail = last q

// Abstraction function:
// q = [head, head.next, head.next.next, ... , null)
class DoubleEndedQueue[A] {
  private var head: Node = _
  private var tail: Node = _
  /** Is the queue empty? */
  // Post: q = q0
  // returns q0 == []
  def isEmpty: Boolean = head == null

  /** add x to the start of the queue. */
  // Post: q = x:q0
  def addLeft(x: A) = {
    if (head == null) {
      head = Node(x, null, null)
      tail = head
    }
    else {
      val newNode = Node(x, null, head)
      head = newNode
      head.next.prev = head
    }
  }

  /** get and remove element from the start of the queue. */
  // Pre: q != []
  // Post: q = tail q0
  def getLeft: A = {
    assert(head != null)
    val value = head.value
    head = head.next
    if(head != null)
      head.prev = null
    else
      tail = null
    value
  }

  /** add element to the end of the queue. */
  // q = q0++[x]
  def addRight(x: A) = {
    if (tail == null) {
      tail = Node(x, null, null)
      head = tail
    }
    else {
      val newNode = Node(x, tail, null)
      tail = newNode
      tail.prev.next = tail
    }
  }

  /** get and remove element from the end of the queue. */
  // Pre: q0 != []
  // Post: q = init q
  def getRight: A = {
    assert(tail != null)
    val value = tail.value
    tail = tail.prev
    if(tail != null)
      tail.next = null
    else
      head = null
    value
  }

  private case class Node(value: A, var prev: Node, var next: Node) {
    override def toString : String = {
      if (next == null)
        value.toString
      else
        value.toString + " <-> " + next.toString
    }
  }

  override def toString : String = {
    if (head == null)
      "emptyDoublyLinkedQueue"
    else
      "["+ head.toString + "]"
  }
}

object Q5 extends App {
  val doubleQueue = new DoubleEndedQueue[Int]()
  println(doubleQueue)
  // emptyDoublyLinkedQueue
  doubleQueue.addLeft(1492)
  println(doubleQueue)
  // [1492]
  doubleQueue.addLeft(71)
  println(doubleQueue)
  // [71 <-> 1492]
  doubleQueue.addRight(3862)
  println(doubleQueue)
  // [71 <-> 1492 <-> 3862]
  println("right: "+ doubleQueue.getRight)
  // right: 3862
  println(doubleQueue)
  // [71 <-> 1492]
  println("left: "+doubleQueue.getLeft)
  // left: 71
  println(doubleQueue.isEmpty)
  // false
  println("right: "+doubleQueue.getRight)
  // right: 1492
  println(doubleQueue)
  // emptyDoublyLinkedQueue
  println(doubleQueue.isEmpty)
  // true
}