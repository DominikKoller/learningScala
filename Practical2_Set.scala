class SlowSet[A] {
  private class Node(var datum: A, var next: Node)
  private var root: Node = null

  def add(datum: A): Unit = {
    root = new Node(datum, root)
  }

  def element(datum: A): Boolean = {
    var iterator = root
    var found = false

    while(!found && iterator != null) {
      found = iterator.datum == datum // == delegates to equals and tests for value equality
      iterator = iterator.next
    }
    found
  }


}
