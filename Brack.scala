/** Import is for readLine so that we can write input directly to the program */
import scala.io.StdIn

object Brack{
	//Maximum length of word so we can define our arrays in dynamic programming
	val MAXWORD = 12

	//Operation to take 'A', 'B' and 'C' to corresponding Ints
  def LetterToInt(a: Char) : Int = {
		if(a == 'A' || a == 'B' || a == 'C'){
			return (a.toInt - 'A'.toInt);
		} else{
			println("Please only Letters from A,B,C.")
			sys.exit
		}
	}
	
  //Defining the op array for everything to use
  val op = Array.ofDim[Int](3,3)  
  op(0)(0) = 1; op(0)(1) = 1; op(0)(2) = 0
	op(1)(0) = 2; op(1)(1) = 1; op(1)(2) = 0
	op(2)(0) = 0; op(2)(1) = 2; op(2)(2) = 2

  /** Read file into array (discarding the EOF character) */
  def readFile(fname: String) : Array[Char] = 
    scala.io.Source.fromFile(fname).toArray.init

 
  /* Functions below here need to be implemented */


	//TASK 1
	//PossibleRec checks whether bracketing to something is possible recursively
	//Checks whether w[i,j) can be bracketed to z
	
	def PossibleRec(w: Array[Int], i: Int, j: Int, z:Int): Boolean = {
	// Can bracket w[i..j) if any of
	assert(i < j)
	if(i+1 == j)
		return w(i) == z

	for(x <- 0 to 2)
	for(y <- 0 to 2)
		if (op(x)(y) == z)	
			for(k <- i+1 to j-1)
				if(PossibleRec(w, i, k, x) && PossibleRec(w, k, j, y))
					return true
	false
	}

/*	
> scala Brack -PossibleRec
> BACB
Bracketing values for BACB
A is not possible
B is possible
C is possible
*/
	
	//TASK 2
	//NumberRec which checks the ways you get a result recursively
	//Computes number of ways w[i,j) can be bracketed to get z
	
	def NumberRec(w: Array[Int], i: Int, j: Int, z:Int): Int = {
	assert(i < j)
	if(i+1 == j)
		if (w(i) == z)
			return 1
		else
			return 0

	var counter = 0
	for(x <- 0 to 2)
	for(y <- 0 to 2)
		if (op(x)(y) == z)	
			for(k <- i+1 to j-1)
				counter += NumberRec(w, i, k, x) * NumberRec(w, k, j, y)
	counter
	} 

//Bracketing values for BACB
//A can be achieved in 0 ways
//B can be achieved in 1 way
//C can be achieved in 4 ways
	
	//TASK 3
	// Both will take O(n^3) operations in worst-case.
	// Proof on sheet
	// Timings seem right:

/*
tw15[~/Practicals/daa]$ time scala Brack -NumberRec testcase12
Bracketing values for BCABBACBACBA
A can be achieved in 22248 ways
B can be achieved in 15362 ways
C can be achieved in 21176 ways

real	2m17.844s
user	2m19.822s
sys	0m0.692s
tw15[~/Practicals/daa]$ time scala Brack -NumberRec testcase11
Bracketing values for ABCABBCABAB
A can be achieved in 3210 ways
B can be achieved in 10541 ways
C can be achieved in 3045 ways

real	0m21.970s
user	0m22.684s
sys	0m0.169s
tw15[~/Practicals/daa]$ time scala Brack -NumberRec testcase10
Bracketing values for ABBAABBAB
A can be achieved in 233 ways
B can be achieved in 947 ways
C can be achieved in 250 ways

real	0m1.108s
user	0m1.647s
sys	0m0.070s
*/
	
	//You may find the following class useful for Task 7
	// Binary tree class
	abstract class BinaryTree
	case class Node (left : BinaryTree, right : BinaryTree) extends BinaryTree
	case class Leaf (value : Char) extends BinaryTree

	//Printing for a binary tree
	def print_tree(t : BinaryTree): Unit = t match {
    case Leaf(c) => print(c)
    case Node(t1, t2) => {
      print("(")
      print_tree(t1)
      //print(")(")
      print_tree(t2)
      print(")")
    }
    case _ => "Tried to print null Tree"
  }

	//These arrays should hold the relevant data for dynamic programming
	var poss = Array.ofDim[Option[Boolean]](MAXWORD, MAXWORD, 3) // Initialized as null, which is kinda ugly TODO change
	var ways = Array.ofDim[Int](MAXWORD, MAXWORD, 3)
  var word = Array.ofDim[String](MAXWORD, MAXWORD, 3)
	var exp = Array.ofDim[BinaryTree](MAXWORD, MAXWORD, 3)


	//Task 4, 5, and 7(optional)

  def TabulatePoss(w: Array[Int], n: Int): Unit = {
    for {i <- 0 until n
         z <- 0 until 3}
      poss(i)(i + 1)(z) = Some(w(i) == z)

    for {j <- 2 to n
         i <- j - 2 to 0 by -1
         z <- 0 to 2} {
      var any = false
      for {x <- 0 to 2
           y <- 0 to 2
           if op(x)(y) == z
           k <- i + 1 to j - 1
           if poss(i)(k)(x).get && poss(k)(j)(y).get} { // throws an error if not yet calculated
        any = true
      }
      poss(i)(j)(z) = Some(any)
    }
  }

  def Tabulate(w: Array[Int], n: Int): Unit = {
    for {i <- 0 until n
         z <- 0 until 3
         if w(i) == z} {
      ways(i)(i + 1)(z) = 1
      word(i)(i + 1)(z) = ('A'.toInt + z).toChar.toString
      exp(i)(i + 1) (z) = Leaf(('A'.toInt + z).toChar)
    }

    for {j <- 2 to n
         i <- j - 2 to 0 by -1
         z <- 0 to 2
         x <- 0 to 2
         y <- 0 to 2
         if op(x)(y) == z
         k <- i + 1 to j - 1} {
      ways(i)(j)(z) += ways(i)(k)(x) * ways(k)(j)(y)
      if(ways(i)(k)(x) * ways(k)(j)(y) != 0) {
        word(i)(j)(z) = "(" + word(i)(k)(x) + word(k)(j)(y) + ")"
        exp(i)(j)(z) = Node(exp(i)(k)(x), exp(k)(j)(y))
      }
    }

    /*
    for(x<-word; y<-x; z<-y)
      println(z)
    println("A can be achieved:")
    println(word(0)(n)(0))
    println("B can be achieved like:")
    println(word(0)(n)(1))
    println("C can be achieved like:")
    println(word(0)(n)(2))
    */
  }


	// ---------- Extra, Lazy:

	class Lazy[T] (val gen: () => T) {
		private var value: Option[T] = None
		var got = false
		def get:T = {
			if(value.isEmpty)
				value = Some(gen())
			value.get
		}
	}

	class LazyArray[T: Manifest] (val length: Int, val gen: Int=> T) {
		private val values: Array[T] = new Array[T](length)

		def get(i: Int): T = {
			if(values(i) == null)
				values(i) = gen(i)
			values(i)
		}

		def foreach(f: T => Unit): Unit = {
			for(i <- 0 until length)
				f(get(i))
		}

		def map[T2: Manifest](f: T => T2): LazyArray[T2] =
			new LazyArray[T2](length, i => f(gen(i)))
	}

	class LazyArray2[T: Manifest](val size: (Int, Int), val gen: (Int, Int) => T) {

    val values:Array[Array[Option[T]]] = Array.tabulate[Option[T]](size._1, size._2)((_,_) => None)

		def get(x: Int)(y: Int): T = {
			if(values(x)(y).isEmpty)
				values(x)(y) = Some(gen(x, y))
			values(x)(y).get
		}

		def foreach(f: T => Unit): Unit = {
      for {x <- 0 until size._1
           y <- 0 until size._2}
        f(get(x)(y))
    }

			def map[T2: Manifest](f: T => T2): LazyArray2[T2] =
				new LazyArray2[T2](size, (x, y) => f(gen(x, y)))
	}

	class LazyArray3[T: Manifest](val size: (Int, Int, Int), val gen: (Int, Int, Int) => T) {

		// val values:Array[Array[Array[Option[T]]]] = Array.ofDim[Option[T]](size._1, size._2, size._3)
    val values:Array[Array[Array[Option[T]]]] = Array.tabulate[Option[T]](size._1, size._2, size._3)((_,_,_) => None)

		def get(x: Int)(y: Int)(z: Int): T = {
			if(values(x)(y)(z).isEmpty)
				values(x)(y)(z) = Some(gen(x, y, z))
			values(x)(y)(z).get
		}

		def foreach(f: T => Unit): Unit = {
      for {x <- 0 until size._1
           y <- 0 until size._2
           z <- 0 until size._3}
        f(get(x)(y)(z))
    }

			def map[T2: Manifest](f: T => T2): LazyArray3[T2] =
				new LazyArray3[T2](size, (x, y, z) => f(gen(x,y,z)))
	}

	var waysLazy: LazyArray3[Int] = _

	def Memoize(w: Array[Int], n: Int): Unit = {
		waysLazy = new LazyArray3[Int]((MAXWORD, MAXWORD, 3), (i, j, z) => {

			assert(i < j)
			if(i+1 == j) {
				if (w(i) == z)
					1
				else
					0
			}
			else {
				var counter = 0
				for {x <- 0 to 2
						 y <- 0 to 2
						 if op(x)(y) == z
						 k <- i + 1 to j - 1}
					counter += waysLazy.get(i)(k)(x) * waysLazy.get(k)(j)(y)
				counter
			}
		})
	}


	// Bracketing values for BACB
  // A cannot be achieved
  //   B can be achieved 1 way
  //   For example:
  //   C can be achieved 4 ways
  // For example:

  // Bracketing values for ABCABBCABAB
  // A can be achieved 3210 ways
  //   For example:
  //   B can be achieved 10541 ways
  // For example:
  //   C can be achieved 3045 ways
  // For example:

  // Both match with the recursive version (and it's muchmuch faster)

	// Task 6
	// The maximum word length will be MAXWORD-1, above that our array is not big enough

  // Will now take time <= T(n) = Omega(n^3)
  // Proof handwritten

  // Will take space = 3*n^2 (actually now space 2*100^2, but we could create the arrays accordingly to size

  // The recursive version will take less space, but will use stack space
  // The dynamic programming version will take much less time and scale much much better as we have seen

  // Here some tests:
  // TODO tests, do not have repl right now

/** The main method just selects which piece of functionality to run */
  def main(args: Array[String]) = {

    // string to print if error occurs
    val errString =
      "Usage: scala Brack -PossibleRec [file]\n"+
      "     | scala Brack -NumberRec [file]\n"+
      "     | scala Brack -Tabulate [file]\n"+
      "     | scala Brack -Memoize [file]\n"

		if (args.length > 2){
			println(errString)
			sys.exit
		}

    //Get the plaintext, either from the file whose name appears in position
    //pos, or from standard input
    def getPlain(pos: Int) =
      if(args.length==pos+1) readFile(args(pos)) else StdIn.readLine.toArray

    // Check there are at least n arguments
    def checkNumArgs(n: Int) = if(args.length<n){println(errString); sys.exit}

    // Parse the arguments, and call the appropriate function
    checkNumArgs(1)
		val plain = getPlain(1)
    val command = args(0)

		//Making sure the letters are of the right type
		val len = plain.length
		var plainInt = new Array[Int](len)
		if (len > MAXWORD){
			println("Word Too Long! Change MAXWORD")
			sys.exit;
		} else {
    	for (i <- 0 until len){
				plainInt(i) = LetterToInt(plain(i))
			}
		}

		//Executing appropriate command
    if(command=="-PossibleRec"){
		println("Bracketing values for "+ plain.mkString(""))
		for(i<-0 to 2){
			if(PossibleRec(plainInt, 0, len, i)){
				println(('A'.toInt + i).toChar + " is Possible");
			}
			else{
				println(('A'.toInt + i).toChar + " is not Possible");
			}
		}
    }
    else if(command=="-NumberRec"){
		var z: Int = 0
		println("Bracketing values for "+ plain.mkString(""))
		for(i<-0 to 2){
			z = NumberRec(plainInt, 0, len, i)
			if(z == 1){
				printf(('A'.toInt + i).toChar+ " can be achieved in %d way\n", z)
			}
			else{
				printf(('A'.toInt + i).toChar+ " can be achieved in %d ways\n", z)
			}
		}
    }

    else if(command=="-Tabulate"){
		Tabulate(plainInt,len)
		println("Bracketing values for "+ plain.mkString(""))
		for(v<-0 to 2){
		var z: Int = ways(0)(len)(v)
			if(z==0){
			println(('A'.toInt + v).toChar+ " cannot be achieved")
			}
			else if(z==1){
				printf(('A'.toInt + v).toChar+ " can be achieved %d way\n", z)
				printf("For example:")
				print_tree(exp(0)(len)(v))
				printf("\n")
			}
			else if (z > 1){
				printf(('A'.toInt + v).toChar+ " can be achieved %d ways\n", z)
				printf("For example:")
				print_tree(exp(0)(len)(v))
				printf("\n")
			}
		}
    }

    else if(command=="-Memoize"){
      Memoize(plainInt,len)
      println("Bracketing values for "+ plain.mkString(""))
      for(v<-0 to 2){
        var z: Int = waysLazy.get(0)(len)(v)
        if(z==0){
          println(('A'.toInt + v).toChar+ " cannot be achieved")
        }
        else if(z==1){
          printf(('A'.toInt + v).toChar+ " can be achieved %d way\n", z)
          printf("For example:")
          print_tree(exp(0)(len)(v))
          printf("\n")
        }
        else if (z > 1){
          printf(('A'.toInt + v).toChar+ " can be achieved %d ways\n", z)
          printf("For example:")
          print_tree(exp(0)(len)(v))
          printf("\n")
        }
      }
    }

    else println(errString)
  }
}