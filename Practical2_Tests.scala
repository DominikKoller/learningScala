import org.scalatest._
// built.sbt adds scalatest for specific files only

class Practical2_Tests extends FunSuite {
  test("ForAny"){
    assert(Set[Int]().forAny(_ => true) === false)
    assert(Set(1,2,3).forAny(_==1) === true)
    assert(Set(1,2,3).forAny(_ >3) === false)
  }

  test("ForAll"){
    assert(Set[Int]().forAll(_ => false) === true)
    assert(Set[Int]().forAll(_ => true) === true)
    assert(Set(1,2,3).forAll(_ < 4) === true)
    assert(Set(1,2,3).forAll(_ <3) === false)
    assert(Set(1,2,3).forAll(_ >4) === false)
  }

  test("Equals"){
    assert(Set[Int]() === Set[Int]())
    assert(Set[Int]() !== Set[Boolean]()) // FAILS!!
    assert(Set(1,2,3) === Set(3,1,2))
    assert(Set[Int]() !== Set(1,2))
    assert(Set(1,2,3) !== Set[Int]())
    assert(Set(1,2,5) !== Set(1,2))
    assert(Set(1,2) !== Set(1,2,4))
  }

  test("AddRemoveSize"){
    val s = Set[Int]()
    assert(s.size === 0)

    assert(s.add(5))
    assert(!s.add(5))
    assert(s.size === 1)

    assert(!s.remove(4))
    assert(s.size === 1)

    assert(s.remove(5))
    assert(s.size === 0)

    assert(s.add(1))
    assert(s.add(2))
    assert(s.size === 2)

    assert(s.remove(1))
    assert(s.remove(2))
    assert(s.size === 0)
  }

  test("contains"){
    val s = Set[Int]()
    for(i <- 0 to 100)
      assert(!s.contains(i))
    for(i <- 0 to 100)
      assert(s.add(i))
    for(i <- 0 to 100)
      assert(s.contains(i))
    for(i <- 0 to 100)
      assert(s.remove(i))
    for(i <- 0 to 100)
      assert(!s.contains(i))
    assert(s.size === 0)
  }

  test("any"){
    val s = Set[Int]()
    assert(s.any === None)
    s.add(1)
    assert(s.any === Some(1))
    s.add(2)
    assert(s.any === Some(1) || s.any === Some(2))
  }

  test("subset"){
    // not sure what to do with type parameters
    // I will compare int sets only
    // this is not enough for generic sets though
    // TODO find strategy for comparing generic sets
    assert(Set[Int]().subsetOf((Set[Int]())))
    assert(Set[Int]().subsetOf(Set(1)))
    assert(Set(1).subsetOf((Set(1))))
    assert(Set(1).subsetOf((Set(2,1))))

    assert(! Set(1).subsetOf(Set[Int]()))
    assert(! Set(1,2).subsetOf(Set(1)))
  }

  test("copy"){
    val s = Set(1,2,3)
    assert(s === s.copy)
    assert(s !== s.copy.add(4))
    assert(Set[Int]() === Set[Int]().copy)
  }

  test("union"){
    // emptyset union s
    assert(Set[Int]().union(Set[Int]()).size === 0)
    assert(Set[Int]().union(Set(1)) === Set(1))
    assert(Set[Int]().union(Set(1,2,3)) === Set(1,2,3))

    // s union emptyset
    assert(Set(1).union(Set[Int]()) === Set(1))
    assert(Set(1,2,3).union(Set[Int]()) === Set(1,2,3))

    // s1 union s2, non overlapping
    assert(Set(1,2).union(Set(3,4)) === Set(1,2,3,4))

    // overlapping
    assert(Set(1,2).union(Set(2,3)) === Set(1,2,3))
    assert(Set(1,2).union(Set(2,3)).size === 3)
  }

  test("intersection"){
    // emptyset intersect s
    assert(Set[Int]().intersect(Set[Int]()).size === 0)
    assert(Set[Int]().intersect(Set(1)) === Set[Int]())
    assert(Set[Int]().intersect(Set(1,2,3)) === Set[Int]())

    // s intersect emptyset
    assert(Set(1).intersect(Set[Int]()) === Set[Int]())
    assert(Set(1,2,3).intersect(Set[Int]()) === Set[Int]())

    // s1 intersect s2, non overlapping
    assert(Set(1,2).intersect(Set(3,4)) === Set[Int]())

    // overlapping
    assert(Set(1,2).intersect(Set(2,3)) === Set(2))
    assert(Set(1,2,3).intersect(Set(2,3,4)) === Set(2,3))

    // same things, different order, for good measure

    // s1 intersect s2, non overlapping
    assert(Set(2,1).intersect(Set(4,3)) === Set[Int]())

    // overlapping
    assert(Set(2,1).intersect(Set(2,3)) === Set(2))
    assert(Set(1,3,2).intersect(Set(4,3,2)) === Set(2,3))
  }

  test("map"){
    assert(Set(2,1,99,4).map(_>3) === Set(true,false))
    assert(Set(2,1,99,4).map(_>100) === Set(false))

    assert(Set(2,3,4).map(_.toString) === Set("3", "4", "2"))
  }

  test("filter"){
    assert(Set(2,3,99,4).filter(_<10) === Set(2,4,3))
    assert(Set(1,2,3).filter(_>10) === Set[Int]())
    assert(Set(1,2,3).filter(_<10) === Set(1,2,3))
  }

  test("EmptySetString"){
    val set = new Set[Int]
    assert(set.toString === "{}")
  }
}