object Sheet1 extends App {

  // Question 1
  // [Programming] Write short Scala functions which, on integer input:
  // (a) square the input;
  // (b) compute the remainder on dividing the input by 3; and
  // (c) find the largest perfect square no more than the input.
  // How would you convince your tutor that you have run your functions and that they give the
  // correct answers?
  // This question is taken from Functional programming last term where you may have seen
  // a “Sheet 0” which had this question in GHCi.

  // println(square(4))
  // 16
  def square(n: Int) :Int = {
    n * n
  }

  // println(remainder1(50))
  // 2
  // println (remainder1(-10))
  // -1
  // interesting convention to return a negative number here
  def remainder1(n: Int) :Int = {
    return (n % 3)
  }

  // println(remainder2(50))
  // 2
  // println(remainder2(-10))
  // -1
  def remainder2(n: Int) :Int = {
    if (-3 < n && n < 3)
      return n
    else if (n > 0)
      return remainder2(n - 3)
    else
      return remainder2(n + 3)
  }
  //println(remainder3(50))
  //2
  //println(remainder3(-10))
  //-1
  def remainder3(n: Int) :Int = {
    var i = n;
    // Precondition: i = n
    // I: i%3 = n%3
    // Variant: i
    while (i >= 3)
      // Test: i > 3
      i -= 3 // maintains I since m % 3 = (m-3) % 3 if (m-3) >= 0
    // Postcondition: i%3 = n%3 ^ i<3

    // I: i%3 = n%3 ^ i<3
    // Variant: abs(i)
    while (i <= -3)
      i += 3 // maintains I: m%3 = (m+3)%3 if (m+3) <= 0
    // Postcondition: i%3 = n%3 ^ i<=3 ^ i>-3
    // => i = n%3
    i
  }
  // I won't usually comment my code to death like this but thought the first examples with invariants can be overly explicit

  //println(largestSquare(2))
  //println(largestSquare(3))
  //println(largestSquare(4))
  //println(largestSquare(5))
  //println(largestSquare(99))
  //println(largestSquare(193))
  def largestSquare (n: Int) :Int = {
    var r = 0
    // I: r*r <= n
    while ((r+1)*(r+1)<=n)
      r += 1
    // Post: r*r <= n ^ (r+1)*(r+1) > n
    r*r
  }

  // Question 2

  // println(sum(Array(2,3,1,4)))
  // 5
  def sum(a : Array[Int]) : Int = {
    val n = a.size
    var total = 0;

    var i = n
    // Invariant I: total = sum(a[i..n)) && 0<=i<=n
    // Variant: i
    while(i > 0){
      // I && i>0
      total += a(i-1)
      // total = sum(a[i-1..n)) && i>0
      i -= 1
      // I
    }
    // I && i=0
    // total = sum(a[0..n))
    total
  }

  // Question 3

  // println(max(Array(2,3,1,4)))
  // 4
  def max (a: Array[Int]): Int = {
    val n = a.size
    require (n>0)
    var max = a(0)
    var i = 1

    // I: max = maximum [0..i)
    // V: n-i
    while (i<n) {
      if(max < a(i))
        max = a(i)
      i += 1
    }
    max
  }

  // Question 4
  // println(findSum(Array(1,2,3,4)))
  // 10
  def findSum(a : Array[Int]) : Int = {
    val n = a.size
    var total = 0; var i = 0
    while(i < n){
      total += a(i)
      i += 1
    }
    total
  }

  // Question 4
  // I am really not sure what this question is asking
  // It might be that we can simply map the args array to an int array?

  // so like
  // println(findSum(argss.map(_.toInt)))

  // but coming from Haskell, this looks very ugly
  // would be:
  // findSum . map(toInt) $ argss


  // Question 5
  // println(fib(2))
  // 1
  // println(fib(10))
  // 55
  def fib (n: Int): Int = {
    require(n>=0)
    if (n == 0 || n == 1)
      return n
    fib (n-1) + fib (n-2)
  }

  // fibPrintDepth(3)
  // fib(3)
  // | fib(2)
  // |  | fib(1)
  // |  | = 1
  // |  | fib(0)
  // |  | = 0
  // | = 1
  // | fib(1)
  // | = 1
  // = 2
  def fibPrintDepth(n: Int): Int = fibPrintDepth(n, 0)

  def fibPrintDepth(n: Int, d: Int): Int = {
    require(n>=0)
    for(i <- 0 until d)
      print(" | ")
    println("fib("+n+")")

    var value = n
    if (n>1)
      value = fibPrintDepth (n-1, d+1) + fibPrintDepth (n-2, d+1)

    for(i <- 0 until d)
      print(" | ")
    println("= "+value)
    value
  }
  // I couldn't do this without adding an additional depth parameter, I would really like to see how

  // Question 6

  // println(fibLoop(10))
  // 55
  def fibLoop (n: Int): Int = {
    var fibo = 1
    var fiboPrev = 0

    var i = 1
    // I: fibo = fib(i) ^ fiboPrev = fib(i-1)
    // Variant: n - i
    while (i<n) {
      val temp = fibo
      fibo = fibo + fiboPrev // by def fib(i+1)
      fiboPrev = temp // = fib(i)
      i += 1
      // fibo = fib(i) ^ fiboPrev = fib(i-1)
    }
    fibo
  }

  // Question 7

  // println(divMod(50.2, 7.2))
  // println(((50.2 / 7.2).toInt), 50.2 % 7.2)
  // (6,6.999999999999999)
  // (6,7.000000000000002)
  def divMod (x: Double, y: Double) : (Int, Double) = {
    require(x>=0)
    require(y> 0)

    var r = x
    var q = 0

    while (r >= y) {
      r -= y
      q += 1
    }
    (q,r)
  }

  // Question 8
  // a)
  // focus is not on efficiency

  // println(gcd (100,10))
  // 10
  // println(gcd (39,91))
  // 13
  println(gcd(39,91))
  def gcd (m: Int, n: Int): Int = {
    var i = 1
    var gcd = 1
    while (i<=m && i<=n){
      if(m%i==0 && n%i==0)
        gcd = i
      i+=1
    }

    var x = 0
    var y = 0

    while (gcd != m*x + n*y){
      x +=1
      y = 0
        while (-(m*x + n*y).abs < gcd)
          y-=1
    }
    println("gcd = mx + ny")
    println(gcd + " = " +m+"*"+x+" + "+n+"*"+y)
    println(gcd == m*x + n*y)

    gcd
  }

  // Question 9

  // println(countHits(Array(0,1,2,3,4)))
  // 5
  // println(countHits(Array(4,3,2,1,0)))
  // 1
  // println(countHits(Array(0,1,1,0,4)))
  // 3
  def countHits (a: Array[Int]): Int = {
    val n = a.length
    require(n>=2)
    var hits = 1
    var max = a(0)

    var i = 1
    // I: hits = hits in a[0..i)
    //    max = maximum a[0..i)

    while (i<n){
      if(a(i)>max)
        {
          hits += 1
          max = a(i)
        }
      i+=1
    }
    // I ^ i = n
    hits
  }
}