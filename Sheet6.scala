import org.scalatest._
import java.io.File

import scala.annotation.tailrec
//import scala.collection.mutable.ListBuffer
// built.sbt adds scalatest for specific files only

// Q1

// Dictionary class from lectures:
/** Each object of this class represents a dictionary, in which
  * words can be looked up.
  * @param fname the name of a file containing a suitable list
  * of words, one per line. */

class Dictionary(fname: String){
  /** A Set object holding the words */
  private val words = new scala.collection.mutable.HashSet[String]

  /** Initialise dictionary from fname */
  // NOT CASE SENSITIV!
  private def initDict(fname: String): Unit = {
    println(new File(".").getAbsolutePath())
    val allWords = scala.io.Source.fromFile(fname).getLines
    // Should word w be included?
    def include(w:String) = w.forall(_.isLower)
    for(w <- allWords; if include(w)) words += w.toLowerCase
    // println("Found "+words.size+" words")
  }

  override def toString: String = words.toString

  // Initialise the dictionary
  initDict(fname)

  /** test if w is in the dictionary */
  def isWord(w: String) : Boolean = words.contains(w.toLowerCase)
}

object S6Q1 extends App {
  var dict = new Dictionary("knuth_words.txt")

  def slice(s: String, from: Int, until: Int): String = {
    // similar to scalas iterableLike slice
    val lo    = math.max(from, 0)
    val hi    = math.min(math.max(until, 0), s.length)
    val elems = math.max(hi - lo, 0)
    val b     = new StringBuilder(elems)

    for(i <- lo until hi)
      b += s(i)
    b.result()
  }
  // println("MAHLZEIT".slice(1, 3))
  // // AH
  // println(slice("MAHLZEIT", 1, 3))
  // // AH

  def take(s: String, n: Int): String = slice(s, 0, n)
  // println("MAHLZEIT".take(3))
  // // MAH
  // println(take("MAHLZEIT", 3))
  // // MAH

  def drop(s: String, n: Int): String = slice(s, n, s.length)
  // println("MAHLZEIT".drop(3))
  // // LZEIT
  // println(drop("MAHLZEIT", 3))
  // // LZEIT

  def insertCharAt(s: String, c: Char, n: Int): String =
    take(s, n) + c + drop(s, n)

  // println(insertCharAt("MAHLZEIT", 'z', 0))
  // // zMAHLZEIT
  // println(insertCharAt("MAHLZEIT", 'z', 3))
  // // MAHzLZEIT
  // println(insertCharAt("MAHLZEIT", 'z', 8))
  // // MAHLZEITz

  def deleteCharAt(s: String, n: Int): String = {
    assert(n >= 0 && n < s.length) // should not work for n = s.length
    take(s, n) + drop(s, n+1)
  }

  // println(deleteCharAt("MAHLZEIT", 0))
  // // AHLZEIT
  // println(deleteCharAt("MAHLZEIT", 3))
  // // MAHZEIT
  // println(deleteCharAt("MAHLZEIT", 7))
  // // MAHLZEI

  // TODO for comparison try and find a non-recursive version of permutations
  // DOES NOT WORK:
  // def permutations(s: String): IndexedSeq[String] = {
  //   for{ i <- 0 until s.length-1
  //        j <- i until s.length }
  //       yield
  //         insertCharAt(deleteCharAt(s, i), s(i), j)
  // }

  // NOTE: does not yet look for equality in characters

  def permutationsRec(s: String): IndexedSeq[String] = {
    if(s.length <= 1)
      IndexedSeq(s)

    else
    for { i     <- 0 until s.length
          perms <- permutationsRec(deleteCharAt(s, i)) }
        yield
          s(i) + perms
  }
  // println(permutationsRec(""))
  // // Vector()
  // println(permutationsRec("A"))
  // // Vector(A)
  // println(permutationsRec("AB"))
  // // Vector(AB, BA)
  // println(permutationsRec("ABC"))
  // // Vector(ABC, ACB, BAC, BCA, CAB, CBA)


  // Now actually to question 1:
  def anagramsByPermutations(s: String, d: Dictionary): IndexedSeq[String] =
    permutationsRec(s).filter(d.isWord).map(_.toLowerCase)

  println(anagramsByPermutations("Dog", dict))
  // Vector(dog, god)
  println(anagramsByPermutations("listen", dict))
  // Vector(listen, inlets, silent, tinsel, enlist)
  println(anagramsByPermutations("Ancestries", dict))
}