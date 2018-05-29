import jps.{Coordinates, Dimensions, RTree, Rectangle}
import sext._ // for pretty printing

object Main extends App {
    val r1 = Rectangle(Coordinates(-4, 1), Dimensions(2, 2))
    val r2 = Rectangle(Coordinates(1, 5), Dimensions(1, 1))
    println(r1.enlargementToFit(r2))

    var rtree = RTree[String]()
    rtree = rtree.insert(Rectangle(Coordinates(-2,1), Dimensions(1,1)), "apple")
    rtree = rtree.insert(Rectangle(Coordinates(2,2), Dimensions(2,2)), "orange")
    println(rtree.treeString)
}