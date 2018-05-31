package jps

import org.scalatest.FlatSpec

class RTreeTest extends FlatSpec {

    "RTree constructor" should "create an empty tree with maximal bounds" in {
        val rtree = RTree[String]()
        assertResult(rtree.size)(0)
        assertResult(rtree.root)(Node.newRoot)
    }

    "RTree constructor" should "not allow value of minEntries < 2" in {
        assertThrows[IllegalArgumentException] {
            val rtree = RTree[String](1, 50)
        }
    }

    "RTree constructor" should "not allow value of minEntries > (maxEntries + 1) / 2" in {
        assertThrows[IllegalArgumentException] {
            val rtree = RTree[String](3, 3)
        }
    }

    "RTree constructor" should "allow value of minEntries <= (maxEntries + 1) / 2" in {
        val rtree = RTree[String](2, 3)
    }

    "Insertion of rectangle entry" should "return a new RTree with the new entry added and updated bounds" in {
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

        //

        // 5 entries
        val bananaEntry = Entry(Rectangle(Coordinates(4, 3), Dimensions(1, 1)), "banana")
        rtree = rtree.insert(bananaEntry)
        assertResult(rtree.size)(5)
        assertResult(rtree.root.children.size)(2)
        assertResult(getLeafContainingEntry(rtree, bananaEntry).children.toSet)(Set(appleEntry, orangeEntry, bananaEntry))
        assertResult(getLeafContainingEntry(rtree, lemonEntry).children.toSet)(Set(peachEntry, lemonEntry))


        // 6 entries
        val raspberryEntry = Entry(Rectangle(Coordinates(12, 2), Dimensions(2, 2)), "raspberry")
        rtree = rtree.insert(raspberryEntry)
        assertResult(rtree.size)(6)
        assertResult(rtree.root.children.size)(3)
        assertResult(getLeafContainingEntry(rtree, appleEntry).children.toSet)(Set(appleEntry, orangeEntry))
        assertResult(getLeafContainingEntry(rtree, raspberryEntry).children.toSet)(Set(raspberryEntry, bananaEntry))
        assertResult(getLeafContainingEntry(rtree, lemonEntry).children.toSet)(Set(peachEntry, lemonEntry))


        // 7 entries
        val grapeEntry = Entry(Rectangle(Coordinates(-1, -6), Dimensions(1, 2)), "grape")
        rtree = rtree.insert(grapeEntry)
        assertResult(rtree.size)(7)
        assertResult(rtree.root.children.size)(3)
        assertResult(getLeafContainingEntry(rtree, appleEntry).children.toSet)(Set(appleEntry, orangeEntry))
        assertResult(getLeafContainingEntry(rtree, raspberryEntry).children.toSet)(Set(raspberryEntry, bananaEntry))
        assertResult(getLeafContainingEntry(rtree, lemonEntry).children.toSet)(Set(peachEntry, lemonEntry, grapeEntry))

    }

    "Insertion of point entry" should "return a new RTree with the new entry added and updated bounds" in {
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

    "Insertion of overlapping rectangles" should "return a new RTree with the new entries" in {
        val appleEntry = Entry(Rectangle(Coordinates(-2, 1), Dimensions(1, 1)), "apple")
        val orangeEntry = Entry(Rectangle(Coordinates(-2, 1), Dimensions(1, 1)), "orange")
        val lemonEntry = Entry(Rectangle(Coordinates(-2, 1), Dimensions(1, 1)), "lemon")
        val peachEntry = Entry(Rectangle(Coordinates(-2, 1), Dimensions(1, 1)), "peach")
        val bananaEntry = Entry(Rectangle(Coordinates(-2, 1), Dimensions(1, 1)), "banana")
        val grapeEntry = Entry(Rectangle(Coordinates(-2, 1), Dimensions(1, 1)), "grape")
        val raspberryEntry = Entry(Rectangle(Coordinates(-2, 1), Dimensions(1, 1)), "raspberry")
        var rtree = RTree[String](2, 3)
          .insert(appleEntry)
          .insert(orangeEntry)
          .insert(lemonEntry)
          .insert(peachEntry)
          .insert(bananaEntry)
          .insert(grapeEntry)
          .insert(raspberryEntry)


        assertResult(7)(rtree.size)
        assertResult(Set(appleEntry, orangeEntry, lemonEntry,
            peachEntry, bananaEntry, grapeEntry,
            raspberryEntry))(rtree.root.getAllEntries.toSet)
    }

    "Search" should "return a Vector of overlapping entries" in {
        val appleEntry = Entry(Rectangle(Coordinates(-2, 1), Dimensions(1, 1)), "apple")
        val orangeEntry = Entry(Rectangle(Coordinates(2, 2), Dimensions(2, 2)), "orange")
        val lemonEntry = Entry(Rectangle(Coordinates(-1, -4), Dimensions(2, 2)), "lemon")
        val peachEntry = Entry(Rectangle(Coordinates(3, -4), Dimensions(2, 1)), "peach")
        var rtree = RTree[String](2, 3)
          .insert(appleEntry)
          .insert(orangeEntry)
          .insert(lemonEntry)
          .insert(peachEntry)

        // Search bound contains searched entries
        var searchBound: Bound = Rectangle(Coordinates(-3, -5), Coordinates(1, 3))
        var result: Vector[Entry[String]] = rtree.search(searchBound)

        assertResult(result.size)(2)
        assert(result.contains(appleEntry))
        assert(result.contains(lemonEntry))

        // Search bound intersects searched entries
        searchBound = Rectangle(Coordinates(0, -3), Coordinates(3, 2))
        result = rtree.search(searchBound)

        assertResult(result.size)(3)
        assert(result.contains(lemonEntry))
        assert(result.contains(peachEntry))
        assert(result.contains(orangeEntry))
    }

    "Removal of rectangle entry" should "return a new RTtee with desired entry deleted and updated bounds" in {
        val appleEntry = Entry(Rectangle(Coordinates(-2, 1), Dimensions(1, 1)), "apple")
        val orangeEntry = Entry(Rectangle(Coordinates(2, 2), Dimensions(2, 2)), "orange")
        val lemonEntry = Entry(Rectangle(Coordinates(-1, -4), Dimensions(2, 2)), "lemon")
        val peachEntry = Entry(Rectangle(Coordinates(3, -4), Dimensions(2, 1)), "peach")
        var rtree = RTree[String](2, 3)
          .insert(appleEntry)
          .insert(orangeEntry)
          .insert(lemonEntry)
          .insert(peachEntry)

        rtree = rtree.remove(peachEntry)
        assertResult(rtree.size)(3)
        assertResult(rtree.root.children.size)(3)
        assertResult(rtree.root.children.toSet)(Set(appleEntry, orangeEntry, lemonEntry))
        assertResult(rtree.root.bound)(Rectangle(Coordinates(-2, -4), Coordinates(4, 4)))
    }

    "Removal of rectangle entries" should "return a new RTtee with desired entries deleted and updated bounds" in {
        val appleEntry = Entry(Rectangle(Coordinates(-2, 1), Dimensions(1, 1)), "apple")
        val orangeEntry = Entry(Rectangle(Coordinates(2, 2), Dimensions(2, 2)), "orange")
        val lemonEntry = Entry(Rectangle(Coordinates(-1, -4), Dimensions(2, 2)), "lemon")
        val peachEntry = Entry(Rectangle(Coordinates(3, -4), Dimensions(2, 1)), "peach")
        val bananaEntry = Entry(Rectangle(Coordinates(4, 3), Dimensions(1, 1)), "banana")
        val grapeEntry = Entry(Rectangle(Coordinates(-1, -6), Dimensions(1, 2)), "grape")
        val raspberryEntry = Entry(Rectangle(Coordinates(12, 2), Dimensions(2, 2)), "raspberry")
        var rtree = RTree[String](2, 3)
          .insert(appleEntry)
          .insert(orangeEntry)
          .insert(lemonEntry)
          .insert(peachEntry)
          .insert(bananaEntry)
          .insert(grapeEntry)
          .insert(raspberryEntry)


        rtree = rtree.remove(peachEntry)
        // 6 entries, 3 leaves
        assertResult(6)(rtree.size)
        assertResult(3)(rtree.root.children.size)
        assertResult(Set(grapeEntry, lemonEntry))(getLeafContainingEntry(rtree, grapeEntry).children.toSet)
        assertResult(Set(appleEntry, orangeEntry))(getLeafContainingEntry(rtree, appleEntry).children.toSet)
        assertResult(Set(raspberryEntry, bananaEntry))(getLeafContainingEntry(rtree, raspberryEntry).children.toSet)
        assertResult(Rectangle(Coordinates(-2, -6), Dimensions(16, 10)))(rtree.root.bound)
        assertResult(Rectangle(Coordinates(-1, -6), Dimensions(2, 4)))(getLeafContainingEntry(rtree, grapeEntry).bound)

        rtree = rtree.remove(appleEntry)
        // 5 entries, 2 leaves
        assertResult(5)(rtree.size)
        assertResult(2)(rtree.root.children.size)
        assertResult(Set(grapeEntry, lemonEntry))(getLeafContainingEntry(rtree, grapeEntry).children.toSet)
        assertResult(Set(bananaEntry, orangeEntry, raspberryEntry))(getLeafContainingEntry(rtree, bananaEntry).children.toSet)
        assertResult(Rectangle(Coordinates(-1, -6), Dimensions(15, 10)))(rtree.root.bound)
        assertResult(Rectangle(Coordinates(2, 2), Dimensions(12, 2)))(getLeafContainingEntry(rtree, orangeEntry).bound)

        rtree = rtree.remove(grapeEntry)
        // 4 entries, 2 leaves
        assertResult(4)(rtree.size)
        assertResult(2)(rtree.root.children.size)
        assertResult(Set(orangeEntry, lemonEntry))(getLeafContainingEntry(rtree, orangeEntry).children.toSet)
        assertResult(Set(bananaEntry, raspberryEntry))(getLeafContainingEntry(rtree, bananaEntry).children.toSet)
        assertResult(Rectangle(Coordinates(-1, -4), Dimensions(15, 8)))(rtree.root.bound)
        assertResult(Rectangle(Coordinates(-1, -4), Dimensions(5, 8)))(getLeafContainingEntry(rtree, orangeEntry).bound)

        rtree = rtree.remove(orangeEntry)
        // 3 entries, 1 leaf
        assertResult(3)(rtree.size)
        assertResult(3)(rtree.root.children.size)
        assertResult(Set(bananaEntry, raspberryEntry, lemonEntry))(rtree.root.children.toSet)
        assertResult(Rectangle(Coordinates(-1, -4), Dimensions(15, 8)))(rtree.root.bound)

        rtree = rtree.remove(lemonEntry)
        // 2 entries, 1 leaf
        assertResult(2)(rtree.size)
        assertResult(2)(rtree.root.children.size)
        assertResult(Set(bananaEntry, raspberryEntry))(rtree.root.children.toSet)
        assertResult(Rectangle(Coordinates(4, 2), Dimensions(10, 2)))(rtree.root.bound)

        rtree = rtree.remove(bananaEntry)
        // 1 entry, 1 leaf
        assertResult(1)(rtree.size)
        assertResult(1)(rtree.root.children.size)
        assertResult(Set(raspberryEntry))(rtree.root.children.toSet)
        assertResult(Rectangle(Coordinates(12, 2), Dimensions(2, 2)))(rtree.root.bound)

        rtree = rtree.remove(raspberryEntry)
        // 0 entries
        assertResult(0)(rtree.size)
        assertResult(Set())(rtree.root.children.toSet)
    }

    def getLeafContainingEntry[T](rtree: RTree[T], entry: Entry[T]): Leaf[T] = {
        rtree.root.children.asInstanceOf[Vector[Leaf[T]]].filter(_.children.contains(entry)).head
    }

    "Removal of non existing entry from empty tree" should "" in {
        val rtree = RTree[String]()
        assertThrows[NoSuchElementException] {
            rtree.remove(Entry(Rectangle(Coordinates(4, 3), Dimensions(1, 1)), "banana"))
        }
    }

    "Removal of non existing entry from non-empty tree" should "" in {
        val rtree = RTree[String]()
            .insert(Entry(Rectangle(Coordinates(-20, -20), Dimensions(1, 1)), "apple"))
        assertThrows[NoSuchElementException] {
            rtree.remove(Entry(Rectangle(Coordinates(4, 3), Dimensions(1, 1)), "banana"))
        }
    }
}
