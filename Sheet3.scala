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
      val t = a+(b-a)/3
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
  // try at position
  // sample it and check if it's too big
  // if it's not too big, repeat with 2*position
  // if it's too big, start the binary search like above from
  // position/2 up to position
  //
  // finding the first tooBig will take ceil(log2(X)) calls
  // the binary search is O(log n)

  // HOWEVER this is not a satisfying solution to the actual problem
  // When we know _nothing_ about the value we are looking for except that it is a positive integer:
  // There is no justification for choosing to double the value at each step
  // If we triple the value at each step, that's better for larger but potentially worse for lower numbers
  // If we take steps of size n^2 that would be better for very large numbers
  // But much worse for lower numbers
  // Also, how do we justify the position 1 as a starting point?
  // It is certainly much more likely that the number we look for is to the right of 1, so we should move it up.. ?
  // Lesson: we cannot find an optimal algorithm for this problem that assumes nothing about the input

  // c) TODO

  // Q4
  def insertionSort(A: Array[Int]): Array[Int] = {
    var i = 1; var N = A.length
    // I: A[0..i) is sorted
    // V: N-i
    while(i<N) {
      val insertAt = search(A.slice(0,i), A(i))
      takeAndInsert(A, i, insertAt)
      i+=1
      }
    A
  }

  // there are no comparisons between elements of A in takeAndInsert
  // the While loop runs N times, with i ranging from 1 to N
  // binary search is run on an array of length i
  // binary search performs log2(n) comparisons in worst-case
  // Hence the number of comparisons is
  // sum(log2(i)) from 1 to N = log2(N!) = O(n log(n))

  // the overall running time must take the operations of takeAndInsert into account
  // which performs i operations inside the loop, hence we have
  // sum(log2(i)) + i) from 1 to N = O(n^2)

  def takeAndInsert(A: Array[Int], takeFrom: Int, insertAt: Int) = {
    require(insertAt <= takeFrom)
    require(takeFrom >= 0 && takeFrom < A.length)
    require(insertAt >= 0 && insertAt < A.length)

    val take = A(takeFrom)
    var i = takeFrom

    // I: A[i+1..takeFrom] = A0[i..takeFrom-1] ^ insertAt <= i <= takeFrom
    // V: i - insertAt
    while(i > insertAt){
      A(i) = A(i-1)
      i-=1
    }
    // Post: A[insertAt+1..takeFrom] = A0[insertAt..takeFrom-1]
    A(insertAt) = take
    // A(insertAt) = A0(takeFrom) ^ A[insertAt+1..takeFrom] = A0[insertAt..takeFrom-1]
  }

  // Q5 TODO optional

  // Q6

  // Given implementation of partition (except using the function swap):
  /** Partition the segment a[l..r)
    * @return k s.t. a[l..k) < a[k..r) and l <= k < r */
  def partition(a: Array[Int], l: Int, r: Int) : Int = {
    val x = a(l) // pivot
    // Invariant a[l+1..i) < x <= a[j..r) && l < i <= j <= r
    var i = l+1; var j = r
    while(i<j){
      if(a(i)<x)
        i += 1
      else{
        if(a(j-1) == 7)
          println("swapping number 7")
        if(a(i) == 7)
          println("swapping number 7 back")
        swap(a, i, j-1)
        j -= 1
      }
    }
    // swap pivot into position
    a(l) = a(i-1); a(i-1) = x
    i-1
  }

  // This example will even swap the element 7 three times:
  // This is because we're trying to swap the same element with itself

  //partition(Array(5, 6, 7), 0, 3)
  // swapping number 7
  // swapping number 7
  // swapping number 7 back

  // Now with 4 elements we don't try to swap 7 with itself, and we get
  // the inefficiency the question is talking about

  //partition(Array(4, 5, 6, 7), 0, 4)
  // swapping number 7
  // swapping number 7 back

  // The problem is that we don't check whether we really want to swap the element
  // at j-1 back. The solution is to check whether we want to do that
  // (ie check if it is smaller than x)
  // if we do, swap
  // if we do not, leave the array as it is and decrease j

  // Improved version:
  /** Partition the segment a[l..r)
    * @return k s.t. a[l..k) < a[k..r) and l <= k < r */
  def partitionImproved(a: Array[Int], l: Int, r: Int) : Int = {
    val x = a(l) // pivot
    // Invariant a[l+1..i) < x <= a[j..r) && l < i <= j <= r
    var i = l+1; var j = r
    while(i<j){
      if(a(i)<x)
        i += 1
      else if (a(j-1) <= x) {
        if(a(j-1) == 7)
          println("swapping number 7")
        if(a(i) == 7)
          println("swapping number 7 back")
        swap(a, i, j-1)
        j -= 1
      }
      else j-=1
    }
    // swap pivot into position
    a(l) = a(i-1); a(i-1) = x
    i-1
  }
  // The invariant stays exactly the same, as swapping i and j-1
  // will maintain the invariant if a(i) >= x and a(j-1) >= x
  // Tested below in the test class

  // partitionImproved(Array(4, 5, 6, 7), 0, 4)
  // now number 7 is never swapped at all (just as we want it to be)

  def swap(A: Array[Int], a: Int, b: Int):Unit = {
    val t = A(a)
    A(a) = A(b)
    A(b) = t
  }

  // Q7
  // the given QSort
  def QSort(A: Array[Int], l: Int, r: Int) : Unit = {
    if(r-l > 1){ // nothing to do if segment empty or singleton
      val k = partition(A, l,r)
      QSort(A, l,k); QSort(A, k+1,r)
    }
  }

  // val A = Array(4,6,2,3)
  // println(A.mkString(","))
  // WhileQSort(0,4)
  // println(A.mkString(","))
  // 4,6,2,3
  // 2,3,4,6

  def WhileQSort(A: Array[Int], l: Int, r: Int) : Unit = {
    var newL = l
    var k: Int = 0

    // I: A[l..newL] is sorted
    // V: r-newL
    while(r-newL > 1){
      k = partition(A, newL,r)
      WhileQSort(A, newL,k); //WhileQSort(k+1,r)
      newL = k+1
    }
    // Post: [l..r-1] is sorted, or [l..r) is sorted as required
  }

  // b)
  // In the code I've written,
  // If A is in decreasing order, then partition(newL,r) = r-1
  // And the recursive call is on A[0..r-1], which is also in decreasing order
  // hence the call stack will be
  // A[0..r]
  //  A[0..r-1]
  //    A[0..r-2]
  //      ...
  //        A[0..0]
  // Which is of depth N

  // In the given QSort, this may be the case with the first recursive call (the second will be optimized)

  // c)
  // We can limit the depth of the stack by selecting which of the two recursive calls in the original code
  // we make recursive in ours and which one we use the while loop for. The lengths of the array partitions
  // for the first one and the second one always add up to the current one, hence the maximum of the minimum
  // of the two is half the size of the current size
  // Hence if we say

  def LowStackQSort(A: Array[Int], l: Int, r: Int) : Unit = {
    if(r-l > 1){ // nothing to do if segment empty or singleton
      val k = partition(A, l,r)
      if(k < r-l / 2.0) {
        LowStackQSort(A, l,k)
        LowStackQSort(A, k+1,r)
      }
      else {
        LowStackQSort(A, k+1,r)
        LowStackQSort(A, l,k)
      }
    }
  }

  // we ensure that the first recursive call operates on at max half the size of the array relative to the current call
  // I will leave it at that and not implement this in a while loop because I'm a fan of compiler optimizations.
  // (And I just hope the Scala compiler is smart enough to understand that this if/else pattern is tail recursive)
  // Which might not actually be the case, hence TODO write this with a while loop

  // Q8
  // a) When all elements are identical, partition will always give us the index 0 with running time r-l = O(n)
  // hence the loop will run n times
  // This is the worst possible case
  // giving us a running time of O(n^2)

  // b) The more elements are equal, the more work partition has to do because it decides to swap entries if they are
  // equal to the pivot. We can get rid of this problem in part c.
  // However, if there are much more than half equal entries, the real problem is another one:
  // The problem is that every one of the equal entries will be on one side of the partition,
  // regardless of whether we use < or <= for the guard in partition
  // This means that when we split the work into two recursive calls, one will always be much larger than the other
  // meaning we get close to O(n^2) whenever the number of identical entries gets close to n

  // c)
  // As mentioned, we can make the algorithm more efficient by replacing < by <=, but this is not the main cause
  // of inefficiency: the main cause is that all identical entries 'land' on the same side of the partition

  // d)

  // val B = Array(2,6,2,1,6,3)
  // println(B.mkString(","))
  // println(partitionThree(B, 0, B.length))
  // println(B.mkString(","))
  // 2,6,2,1,6,3
  // (1,2)
  // 1,2,2,6,6,3


  /** Partition the segment a[l..r)
    * @return k s.t. a[l..k) < a[k..r) and l <= k < r */
  def partitionThree(A: Array[Int], l: Int, r: Int) : (Int, Int) = {

    val x = A(l)
    var i = l
    var j = l
    var k = r

    // I: A[l..i) < x
    //    A[i..j) = x
    //    A[k..r) > pivot

    // V: (k-j) * (i-k)
    // one of (k-j) or (i-k) will always decrease
    while(j<k && i<k) {
      if (A(j) == x)
        j += 1 // Maintains A[i..j) = x
      else if (A(i) < x) {
        if(i==j) j+=1
        i += 1 // Maintains A[l..i) < x ^ A[i..j) = x
      }
      else if (A(k - 1) > x)
        k -= 1 // Maintains A[k..r) > x
      else if (A(k - 1) == x) { // ^ A(j) > x
        swap(A, j, k - 1)
        j += 1
        k -= 1
      }
      else { // a(k-1) < x ^ A(i) >= x
        swap(A, i, k - 1)
        if (j == i) j += 1
        i += 1
        // do not decrease k, let the next iteration do that (or account for the case A(k-1) == x)
      }
    }
    (i, j-1)
  }

  // var A = Array(53,6,2,3,7,1,13,12)
  // FasterQSort(A, 0, A.length)
  // println(A.mkString(","))
  // 1,2,3,6,7,12,13,53

  def FasterQSort(A: Array[Int], l: Int, r: Int) : Unit = {
    if(r-l > 1){ // nothing to do if segment empty or singleton
      val (i, j) = partitionThree(A, l,r)
      FasterQSort(A, l, i); FasterQSort(A, j+1,r)
    }
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

  // Q6 Tests
  def assertIsPartitioned(A: Array[Int], pivotIndex: Int) = {
    for(i <- 0 until pivotIndex)
      assert(A(i) < A(pivotIndex))

    for(i <- pivotIndex until A.length)
      assert(A(i) >= A(pivotIndex))
  }

  test("partitioning singleton array") {
    val A = Array(0)
    val pivotIndex = partitionImproved(A, 0, A.length)
    assertIsPartitioned(A, pivotIndex)
  }

  test("partitioning small arrays") {
    val A = Array(0,1)
    val pivotIndexA = partitionImproved(A, 0, A.length)
    assertIsPartitioned(A, pivotIndexA)

    val B = Array(1,0)
    val pivotIndexB = partitionImproved(B, 0, B.length)
    assertIsPartitioned(B, pivotIndexB)
  }

  test("partitioning big random array") {
    import util.Random.nextInt
    val A = Array.fill(100)(nextInt)
    val pivotIndex = partitionImproved(A, 0, A.length)
    assertIsPartitioned(A, pivotIndex)
  }

  test("partitioning big increasing array") {
    val A = new Array[Int](100)
    for(i <- 0 until A.length)
      A(i) = i
    val pivotIndex = partitionImproved(A, 0, A.length)
    assertIsPartitioned(A, pivotIndex)
  }

  test("partitioning big decreasing array") {
    val A = new Array[Int](100)
    for(i <- 0 until A.length)
      A(i) = -i
    val pivotIndex = partitionImproved(A, 0, A.length)
    assertIsPartitioned(A, pivotIndex)
  }
}