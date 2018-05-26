package jps

object RTree {
    def apply[T](minEntries: Int, maxEntries: Int): RTree[T] = {
        RTree[T](Node.newRoot, 0, minEntries, maxEntries)
    }

    def apply[T]() :RTree[T] = {
        RTree[T](Node.newRoot, 0, 2, 50)
    }
}

case class RTree[T] (root: Node[T], size: Int, minEntries: Int, maxEntries: Int) {
    def insert(rectangle: Rectangle, value: T): RTree[T] = {
        insert(Entry[T](rectangle, value))
    }

    def insert(entry: Entry[T]): RTree[T] = {
        val destNode: Leaf[T] = chooseLeaf(root, entry).asInstanceOf[Leaf[T]]
        if (destNode.children.size > maxEntries) {
            val split: (Leaf[T], Leaf[T]) = splitNode(destNode)
            adjustTree(split._1, Some(split._2))
        } else {
            adjustTree(destNode, None)
        }
    }

    def chooseLeaf(root: Node[T], entry: Entry[T]): Node[T] = {
        val n = root
        n match {
            case n: Leaf[T] => n
            case n: Branch[T] => chooseLeaf(chooseSubtree(n.children, entry), entry)
        }
    }

    def splitNode(leaf: Leaf[T]): (Leaf[T], Leaf[T]) = {
        val children: Vector[Entry[T]] = leaf.children
        val (leftSeed, rightSeed): (Entry[T], Entry[T]) = linearPickSeeds(children)
        val remainingChildren = children.filter(c => c != leftSeed && c != rightSeed)

        ???
    }


    def linearPickSeeds(children: Vector[Entry[T]]): (Entry[T], Entry[T]) = {
        case class Params(leftMostLeftSide: Double,         //dimLb
                         leftMostRightSide: Double,         //dimMinUb
                         rightMostLeftSide: Double,         //dimMaxLb
                         rightMostRightSide: Double,        //dimUb
                         leftEntry: Option[Entry[T]],       //nMinLb
                         rightEntry: Option[Entry[T]])      //nMaxLb //TODO delete

        def lpsX(children: Vector[Entry[T]], s: Params): (Double, Entry[T], Entry[T]) = {
            val ch = children.head
            val leftMostLeftSide = Math.min(ch.rect.x, s.leftMostLeftSide)
            val (leftMostRightSide, leftEntry) = if (ch.rect.x2 < s.leftMostRightSide) (ch.rect.x2, Some(ch)) else (s.leftMostRightSide, s.leftEntry)
            val (rightMostLeftSide, rightEntry) = if (ch.rect.x > s.rightMostLeftSide) (ch.rect.x, Some(ch)) else (s.rightMostLeftSide, s.rightEntry)
            val rightMostRightSide = Math.max(ch.rect.x2, s.rightMostRightSide)
            val sides = Params(leftMostLeftSide, leftMostRightSide, rightMostLeftSide, rightMostRightSide, leftEntry, rightEntry)
            if (children.nonEmpty) {
                lpsX(children.tail, s)
            } else {
                val separation = Math.abs((leftMostRightSide - rightMostLeftSide) / (rightMostRightSide - leftMostLeftSide))
                (separation, s.leftEntry.get, s.rightEntry.get)
            }
        }

        def lpsY(children: Vector[Entry[T]], s: Params): (Double, Entry[T], Entry[T]) = {
            val ch = children.head
            val leftMostLeftSide = Math.min(ch.rect.y, s.leftMostLeftSide)
            val (leftMostRightSide, leftEntry) = if (ch.rect.y2 < s.leftMostRightSide) (ch.rect.y2, Some(ch)) else (s.leftMostRightSide, s.leftEntry)
            val (rightMostLeftSide, rightEntry) = if (ch.rect.y > s.rightMostLeftSide) (ch.rect.y, Some(ch)) else (s.rightMostLeftSide, s.rightEntry)
            val rightMostRightSide = Math.max(ch.rect.y2, s.rightMostRightSide)
            val sides = Params(leftMostLeftSide, leftMostRightSide, rightMostLeftSide, rightMostRightSide, leftEntry, rightEntry)
            if (children.nonEmpty) {
                lpsX(children.tail, s)
            } else {
                val separation = Math.abs((leftMostRightSide - rightMostLeftSide) / (rightMostRightSide - leftMostLeftSide))
                (separation, s.leftEntry.get, s.rightEntry.get)
            }
        }


        val initialSides = Params(Double.MaxValue, Double.MaxValue, Double.MinValue, Double.MinValue, None, None)
        val xSeeds = lpsX(children, initialSides)
        val ySeeds = lpsY(children, initialSides)
        if (xSeeds._1 > ySeeds._1) (xSeeds._2, xSeeds._3) else (ySeeds._2, ySeeds._3)
    }

    def adjustTree(l: Node[T], ll: Option[Node[T]]): RTree[T] = ???

    def chooseSubtree(children: Vector[Node[T]], entry: Entry[T]): Node[T] = {
        val childEnlPairs = children.map(c => (c, c.boundingRect.enlargementToFit(entry.rect)))
        val minVal = childEnlPairs.minBy(_._2)
        val leastEnlChildren = childEnlPairs.filter(_._2 == minVal._2).map(_._1)
        leastEnlChildren.minBy(child => child.boundingRect.area())
    }
}
