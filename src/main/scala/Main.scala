import jps.{Coordinates, Dimensions, Rectangle}

object Main extends App {
    //    // create some entries
    //    val alice = Entry(Point(9.12F, -4.9F), "alice")
    //    val bob = Entry(Point(2.3F, 4.6F), "bob")
    //    val candice = Entry(Point(4.7F, -1.9F), "candice")
    //    val doug = Entry(Point(5.5F, -3.2F), "doug")
    //
    //    // build a tree with three points
    //    val tree1: RTree[String] = RTree(alice, bob, candice)
    //
    //    // add "doug"
    //    val tree2: RTree[String] = tree1.insert(doug)
    //
    //    // remove "bob"
    //    val tree3: RTree[String] = tree2.remove(bob)
    //
    //    // search from (0,-4) to (10,6), will find "doug"
    //    val bbox: Box = Box(0F, -4F, 10F, 6F)
    //    val results: Seq[Entry[String]] = tree3.search(bbox)
    //
    //    // we can also just ask how many matching entries exist
    //    val n: Int = tree3.count(bbox)
    //    assert(results.length == n)
    //
    //
    //    val tree = RTree.empty
    //    val tr : jps.RTree[Int] = jps.RTree[Int](jps.Node.newRoot, 0, 2, 50)
    //
    ////    val aaa: jps.Leaf[Int] = Leaf()
    //    case class Student(name: String, score: Int)
    //
    //    object ReduceLeftTest extends App {
    //
    //        val alex = Student("Alex", 83)
    //        val david = Student("David", 80)
    //        val frank = Student("Frank", 85)
    //        val julia = Student("Julia", 90)
    //        val kim = Student("Kim", 95)
    //
    //        val students = Seq(alex, david, frank, julia, kim)
    //
    //        def max(s1: Student, s2: Student): Student = if (s1.score > s2.score) s1 else s2
    //
    //        val topStudent = students.reduceLeft(max(_, _))
    //        println(s"${topStudent.name} had the highest score: ${topStudent.score}")
    //
    //    }

    val r1 = Rectangle(Coordinates(-4, 1), Dimensions(2, 2))
    val r2 = Rectangle(Coordinates(1, 5), Dimensions(1, 1))
    println(r1.enlargementToFit(r2))


}