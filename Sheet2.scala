import org.scalatest._
import scala.collection.mutable.ListBuffer
// built.sbt adds scalatest for specific files only

object Sheet2 extends App {
  //Q1
  // def swap(x: Int, y: Int) = { val t = x; x = y; y = t }
  // This throws an error since it reassigns the input parameter x
  // Input parameters in Scala are always immutable (val)
  // This is to make the life of developers easier: you never have to check whether an input parameter has been overwritten
  // This function probably tries to swap values of two variables.
  // As far as I know, such a function cannot be implemented in scala

  def swapEntries(a: Array[Int], i: Int, j: Int) = {
    val t = a(i); a(i) = a(j); a(j) = t
  }

  // This function swaps two entries in an array, hence the array
  // [0,1,...,i,...,j,...] will become
  // [0,1,...,j,...,i,...]
  // This is possible since no parameter variable has to be reassigned: the passed variable is immutable (cannot be reassigned) but the array it contains is not

  // Q2
  object SideEffects{
    var x = 3; var y = 5
    def nasty(x: Int) : Int = { y = 1; 2 * x }
    def main(args: Array[String]) = println(nasty(x) + y)
  }
  // This will call nasty(x), hence setting y == 1, which returns 6, and adds y, printing 7
  // if we replace nasty(x) + y with y + nasty(x), this will return 11

  // Q3 in class Sheet2Tests below

  // TODO Question 4

  // Q5 in class Sheet2Tests below

  // Q6
  // println(recurringPeriod("ABCABCA".toCharArray))
  // 3
  def recurringPeriod(s: Array[Char]): Int = {
    val N = s.size
    var n = 0
    var found = false
    // I: n<=N ^ found = recurringPeriod >= n
    // V: N-n
    while(n<N && !found) {
      n+=1
      if(s.slice(0, N-n).sameElements(s.slice(n, N)))
        found = true
    }
    return n
  }

  // Q7
  def exists(p: Int => Boolean, N : Int): Boolean = {
    var exists = false
    var i = 0
    // I: i<=N ^ exists(p, i) == false
    while(i<N && !exists){
      if(p(i)) exists = true
      i+=1
    }
    // This code will try each value from 0 to N
    // until it finds one that satisfies the existential
    // if it does, it will return true
    // otherwise, it will return false
    return exists

    // This would be a shorter version, with a for loop:
    //for(i <- 0 until N)
    //  if(p(i)) return true
    //return false
  }

  //Q8
  // a)
  // m = ceil(q/p)

  // b)
  // println(findReciprocals(5, 6))
  // List(2, 3)
  // println(findReciprocals(5231, 53321))
  // List(11, 139, 1663833, 88365288)

  def findReciprocals(pp: Int, qq: Int): List[Int] = {
    var p: Int = pp
    var q: Int = qq
    val reciprocals = new ListBuffer[Int]

    while(p > 0){
      val m = (q.toFloat/p).ceil.toInt
      p = p*m - q
      q = q*m
      reciprocals.append(m)
    }
    // we need to prove that
    // p * ceil(q/p) - q < p
    // ceil(q/p) - q/p < 1
    // ceil(q/p) < q/p + 1
    // qed

    reciprocals.toList
  }

  //Q9
  // TODO invariant, variant
  // println(questionNine(2)) // 0
  // println(questionNine(2.99)) // 0
  // println(questionNine(3)) // 1
  // println(questionNine(8)) // 1
  // println(questionNine(8.99)) // 1
  // println(questionNine(9)) // 2
  // println(questionNine(26)) // 2
  // println(questionNine(26.99)) // 2
  // println(questionNine(27)) // 3
  def questionNine(x: Double): Int = {
    require(x>=1)

    var y = 0
    var acc = 1
    while(acc <= x){
      y+=1
      acc*=3
    }
    y-1
  }

  // Q10
  // TODO Invariant, variant
  println(eval(Array(2, 2), 2)) // 2 + 2*2 = 6
  // 6.0
  println(eval(Array(2, 4, 3), 2)) // 2 + 4*2 + 3*4 = 22
  // 22.0
  def eval(a: Array[Double], x: Double): Double = {
    var lastX: Double = 1
    var sum: Double = 0
    for(i <- 0 until a.size) {
      sum += a(i)*lastX
      lastX *= x
    }
    sum
  }
}

class Sheet2Tests extends FunSuite {
  // Q3
  test("ArraySorted"){
    val digits=Array(3,6,2,3,6,2,3)
    assert(digits.sorted === Array(2,2,3,3,3,6,6))
  }
  test("ArraySortWith"){
    val digits=Array(3,6,2,3,6,2,3)
    assert(digits.sortWith(_>_) === Array(6,6,3,3,3,2,2))
  }
  test("Zip"){
    val digits=Array(3,6,2,3,6,2,3)
    val ones  =Array(1,1,1,1,1,1,1)
    assert(digits.zip(ones) === Array((3,1),(6,1),(2,1),(3,1),(6,1),(2,1),(3,1)))
  }

  // Q5

  /** Does pat appear as a substring of line? */
  def search(pat: Array[Char], line: Array[Char]) : Boolean = {
    val K = pat.size; val N = line.size
    // Invariant: I: found = (line[i..i+K) = pat[0..K) for
    // some i in [0..j)) and 0 <= j <= N-K
    var j = 0; var found = false
    while(j <= N-K && !found){
      // set found if line[j..j+K) = pat[0..K)
      // Invariant: line[j..j+k) = pat[0..k)
       var k = 0
       while(k<K && line(j+k)==pat(k)) k = k+1
       found = (k==K)
       j = j+1
       }
    // I && (j=N-K+1 || found)
    // found = ( line[i..i+K) = pat[0..K) for some i in [0..N-K+1) )
    found
  }

  // a) on line 6, replace false by true
  test("searchPat"){
    assert(search("not".toCharArray, "in there".toCharArray) === false)
  }

  // b) On line 7, replace <= by <
  test("searchPatLastCharacter") {
    assert(search("PAT".toCharArray, "abcdefPAT".toCharArray) === true)
  }

  // c) On line 7, replace N-K by N-K+1
  test("searchPat Line Overflow") {
    assert(search("PAT".toCharArray, "PA".toCharArray) === false)
  }

  // d) On line 10, replace 0 by 1
  test("searchPatFirstLetter") {
    assert(search("PAT".toCharArray, "FAT".toCharArray) === false)
  }

  // e) On line 11, replace < by <=
  test("searchPat Pattern Overflow") {
    assert(search("PAT".toCharArray, "PATTERN".toCharArray) === true)
  }

  // f) On line 12, replace == by >=
  // The postcondition of the while loop is k<=K, so this does not alter the outcome of the function
}
