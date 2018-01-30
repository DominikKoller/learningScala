import org.scalatest._
//import scala.collection.mutable.ListBuffer
// built.sbt adds scalatest for specific files only

object Sheet3 extends App {
  // Q1
  // If the array is not sorted, this procedure will test a maximum of log2(n) values in the array
  // if any of them is equals to the value we're searching for, it will return the position of this value
  // otherwise, it will return the position of the last value checked

  // N=0, we will never enter the loop and return 0

  // Arrays in Java cannot be larger than 2^31-1
  // But assuming they could, and a.size would give us a BigInt,
  // then the search would never terminate for arrays larger than 2^31-1 iff the value we're searching is above the integer range, since i would
  // still be an integer and never reach the value j but instead wrap around to -2^31+1

  // this could be fixed by declaring i as a BigInt.

  def search(a: Array[Int], x: Int): Int = {
    // I: a[0..i) < x <= a[j..N) && 0 <= i <= j <= N
    val N = a.size; var i=0; var j=N
    while(i<j){
      val m = (i+j)/2; // i<=m<j
      if(a(m)<x) i= m+1 else j = m
    }
    // I && j=j, so a[0..i) < x <= a[i..N)
    i
  }


  //Q2
  def sqr(x:Int) = x*x

  def tSqrt(n: Int): Int = {
    assert(n>=0)

    var a=0; var b=n+1
    // I: a^2 <= n < b^2 and a<b
    while(a+1<b){
      val t = a+(b-a)/3 // TODO write condition statement
      if(n<t*t) b = t
      else {
        val t2 = a+2*(b-a)/3
        if(n<t2*t2) {
          a = t
          b = t2
        }
        else a = t2
      }
    }
    // TODO write post
    a
  }

  // If the interval between a and b becomes 2 (smaller than three and bigger than 1)
  // t = a
  // b = t+2
  // a <= sqrt n < b
  // that means we fall through the first if condition and
  // t2 = a+1
  // then either sqrt n < t2, in this case the interval will be 1 and we will leave the loop
  // or else, the interval will also be 1 and we will leave the loop

  // Hence in this implementation the interval will never become 0 as it becomes small


  // Find a s.t. a^2 <= y < (a+1)^2.  Precondition: y >= 0.
  def lectureSqrt(y:Int) : Int = {
    assert(y>=0);
    // Invariant I: a^2 <= y < b^2 and a<b
    var a = 0; var b = y+1;
    while(a+1<b){
      val m = (a+b)/2; // a < m < b
      if(m*m<=y) a=m else b=m;
    }
    // I and a+1=b, so a^2 <= y < (a+1)^2
    return a
  }

  // Q3

  // a)
  // A simple binary search on integers from 0 to 1000 will do:
  // sample in the middle of an array of asc. integers from 0 to 1000
  // if not tooBig (sample) and not tooBig(sample+1), we've found the number
  // else if tooBig(sample) repeat with the array from 0 to sample, exclusive
  // else repeate with the array from sample (exclusive) until the end of the array

  // b)
  // start at an arbitrary integer position (this is interesting, we may start at any position)
  // if you are indecisive, pick position 1
  // sample it and check if it's too big
  // if it's not too big, repeat with 2*position
  // if it's too big, start the binary search like above from
  // position/2 up to position
  //
  // finding the first tooBig will take ceil(log2(X)) calls
  // the binary search is O(log n)

  // c) TODO

  // Q4 // TODO doesnt work
  def insertionSort(A: Array[Int]): Array[Int] = {
    var i = 1; var N = A.size
    while(i<N) {
      var temp = search(A.slice(0, i), A(i))
      swap(A, i, search(A.slice(0, i), A(i)))
      i+=1
      }
    A
  }

  def swap(A: Array[Int], a: Int, b: Int):Unit = {
    val t = A(a)
    A(a) = A(b)
    A(b) = t
  }

}

class Sheet3Tests extends FunSuite {
  //test("searchPat Pattern Overflow") {
  //  assert(search("PAT".toCharArray, "PATTERN".toCharArray) === true)
  //}

  import Sheet3._

  test("Square Root compare with lecture implementation") {
    for(i<-0 to 46000)
      assert(tSqrt(i) === lectureSqrt(i))
  }

  test("insertionSort compare with standard lib") {
    import util.Random.nextInt
    val A = Array.fill(100)(nextInt)
    var B = A.clone()

    insertionSort(A)
    B = B.sortWith(_<_)

    println(A.mkString(","))
    println(B.mkString(","))

    assert(A.sameElements(B))
  }

}