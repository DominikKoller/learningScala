import org.scalatest._
// built.sbt adds scalatest for specific files only

class Practical2_Tests extends FunSuite {
  test("EmptySetEquality"){
    assert(new Set[Int] === new Set[Int])
  }

  test("EmptySetInequality"){
    assert(new Set[Int] !== new Set[Boolean])
  }

  test("EmptySetString"){
    val set = new Set[Int]
    assert(set.toString === "{}")
  }
}
