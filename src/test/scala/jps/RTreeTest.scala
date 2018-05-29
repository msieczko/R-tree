package jps

import org.scalatest.FlatSpec

class RTreeTest extends FlatSpec {

    "RTree contructor" should "create an empty tree with maximal bounds" in {
        val rtree = RTree[String]()
        assertResult(rtree.size)(0)
        assertResult(rtree.root)(Node.newRoot)
    }

    "Insert of rectangles on empty RTree" should "add new elements and update bounds" in {
        // 1 entry
        val appleRect = Rectangle(Coordinates(-2, 1), Dimensions(1, 1))
        val appleEntry = Entry(appleRect, "apple")
        var rtree = RTree[String](2, 3).insert(appleEntry)

        assertResult(rtree.size)(1)
        assertResult(rtree.root.children.size)(1)
        assert(rtree.root.children.contains(appleEntry))
        assertResult(rtree.root.bound)(appleRect)

        // 2 entries
        val orangeEntry = Entry(Rectangle(Coordinates(2, 2), Dimensions(2, 2)), "orange")
        rtree = rtree.insert(orangeEntry)

        assertResult(rtree.size)(2)
        assertResult(rtree.root.children.size)(2)
        assert(rtree.root.children.containsSlice(Vector(appleEntry, orangeEntry)))
        assertResult(rtree.root.bound)(Rectangle(Coordinates(-2, 1), Coordinates(4, 4)))

        // 3 entries
        val lemonEntry = Entry(Rectangle(Coordinates(-1, -4), Dimensions(2, 2)), "lemon")
        rtree = rtree.insert(lemonEntry)

        assert(rtree.root.children.containsSlice(Vector(appleEntry, orangeEntry, lemonEntry)))
        assertResult(rtree.root.bound)(Rectangle(Coordinates(-2, -4), Coordinates(4, 4)))

        // 4 entries, 3 nodes
        val peachEntry = Entry(Rectangle(Coordinates(3, -4), Dimensions(2, 1)), "peach")
        rtree = rtree.insert(peachEntry)

        assertResult(rtree.size)(4)
        assertResult(rtree.root.children.size)(2)
        assertResult(rtree.root.bound)(Rectangle(Coordinates(-2, -4), Coordinates(5, 4)))

        // SplitNode produced nodes with correct bounds...
        val upperRect = Rectangle(Coordinates(-2, 1), Coordinates(4, 4))
        val lowerRect = Rectangle(Coordinates(-1, -4), Coordinates(5, -2))
        val nodeBounds = rtree.root.children.map(_.bound)
        assert(nodeBounds.contains(upperRect))
        assert(nodeBounds.contains(lowerRect))

        // ...and appropriate entries assigned
        val nodeChildren = rtree.root.children.asInstanceOf[Vector[Node[String]]].map(_.children)
        if (nodeChildren(0).contains(lemonEntry) && nodeChildren(0).contains(peachEntry)) {
            assert(nodeChildren(1).contains(appleEntry))
            assert(nodeChildren(1).contains(orangeEntry))
        } else {
            assert(nodeChildren(1).contains(lemonEntry))
            assert(nodeChildren(1).contains(peachEntry))
        }
    }

    "Insert of points on empty RTree" should "add new elements and update bounds" in {
        // 1 entry
        val p1 = Point(Coordinates(2, 1))
        val p1Entry = Entry(p1, "p1")
        var rtree = RTree[String](2, 3).insert(p1Entry)

        assertResult(rtree.size)(1)
        assertResult(rtree.root.children.size)(1)
        assert(rtree.root.children.contains(p1Entry))
        assertResult(rtree.root.bound)(p1)

        // 2 entries
        val p2Entry = Entry(Point(Coordinates(-1, 3)), "p2")
        rtree = rtree.insert(p2Entry)

        assertResult(rtree.size)(2)
        assertResult(rtree.root.children.size)(2)
        assert(rtree.root.children.containsSlice(Vector(p1Entry, p2Entry)))
        assertResult(rtree.root.bound)(Rectangle(Coordinates(-1, 1), Coordinates(2, 3)))

        // 3 entries
        val p3Entry = Entry(Point(Coordinates(-1, -3)), "p3")
        rtree = rtree.insert(p3Entry)

        assert(rtree.root.children.containsSlice(Vector(p1Entry, p2Entry, p3Entry)))
        assertResult(rtree.root.bound)(Rectangle(Coordinates(-1, -3), Coordinates(2, 3)))

        // 4 entries, 3 nodes
        val p4Entry = Entry(Point(Coordinates(4, 5)), "p4")
        rtree = rtree.insert(p4Entry)

        assertResult(rtree.size)(4)
        assertResult(rtree.root.children.size)(2)
        assertResult(rtree.root.bound)(Rectangle(Coordinates(-1, -3), Coordinates(4, 5)))

        // SplitNode produced nodes with correct bounds...
        val leftRect = Rectangle(Coordinates(-1, -3), Coordinates(-1, 3))
        val rightRect = Rectangle(Coordinates(2, 1), Coordinates(4, 5))
        val nodeBounds = rtree.root.children.map(_.bound)
        assert(nodeBounds.contains(leftRect))
        assert(nodeBounds.contains(rightRect))

        // ...and appropriate entries assigned
        val nodeChildren = rtree.root.children.asInstanceOf[Vector[Node[String]]].map(_.children)
        if (nodeChildren(0).contains(p1Entry) && nodeChildren(0).contains(p4Entry)) {
            assert(nodeChildren(1).contains(p2Entry))
            assert(nodeChildren(1).contains(p3Entry))
        } else {
            assert(nodeChildren(1).contains(p1Entry))
            assert(nodeChildren(1).contains(p4Entry))
        }
    }


}
