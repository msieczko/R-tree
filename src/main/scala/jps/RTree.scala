package jps

object RTree {
    def apply[T](minEntries: Int, maxEntries: Int): RTree[T] = {
        RTree[T](Node.newRoot[T], 0, minEntries, maxEntries)
    }

    def apply[T](): RTree[T] = {
        RTree[T](Node.newRoot[T], 0, 2, 50)
    }
}

case class RTree[T](root: Node[T], size: Int, minEntries: Int, maxEntries: Int) {
    def insert(bound: Bound, value: T): RTree[T] = {
        insert(Entry[T](bound, value))
    }

    def insert(entry: Entry[T]): RTree[T] = {
        val targetLeaf: Leaf[T] = chooseLeaf(root, entry).asInstanceOf[Leaf[T]]
        val newLeaf = targetLeaf :+ entry
        if (newLeaf.children.size > maxEntries) {
            val split: (Leaf[T], Leaf[T]) = splitNode(newLeaf)
            adjustTree(split._1, Some(split._2))
        } else {
            adjustTree(newLeaf, None)
        }
    }

    def chooseLeaf(root: Node[T], entry: Entry[T]): Node[T] = {
        val n = root
        n match {
            case n: Leaf[T] => n
            case n: Branch[T] => chooseLeaf(chooseSubtree(n.children, entry), entry)
        }
    }


    def linearPickNext(remainingChildren: Vector[Entry[T]]): (Entry[T], Vector[Entry[T]]) = {
        (remainingChildren.head, remainingChildren.tail)
    }

    def createLeaves(remainingChildren: Vector[Entry[T]], leftLeaf: Leaf[T], rightLeaf: Leaf[T]): (Leaf[T], Leaf[T]) = {
        if (remainingChildren.isEmpty) {
            return (leftLeaf, rightLeaf)
        }
        val (child, newRemaining) = linearPickNext(remainingChildren)
        //TODO check this condition:
        // if one group has so few entries that all remaining entries must be assigned to it in order for it to have
        // minimum number of entries, assign them and stop

        val leftEnlargement = leftLeaf.bound.enlargementToFit(child.bound)
        val rightEnlargement = rightLeaf.bound.enlargementToFit(child.bound)
        if (leftEnlargement > rightEnlargement) {
            createLeaves(newRemaining, leftLeaf, rightLeaf :+ child)
        } else if (leftEnlargement < rightEnlargement) {
            createLeaves(newRemaining, leftLeaf :+ child, rightLeaf)
        }

        val leftArea = leftLeaf.bound.area()
        val rightArea = rightLeaf.bound.area()
        if (leftArea > rightArea) {
            createLeaves(newRemaining, leftLeaf, rightLeaf :+ child)
        } else if (leftArea < rightArea) {
            createLeaves(newRemaining, leftLeaf :+ child, rightLeaf)
        }

        if (leftLeaf.children.size > rightLeaf.children.size) {
            createLeaves(newRemaining, leftLeaf, rightLeaf :+ child)
        } else {
            createLeaves(newRemaining, leftLeaf :+ child, rightLeaf)
        }
    }

    def splitNode(leaf: Leaf[T]): (Leaf[T], Leaf[T]) = {
        val children: Vector[Entry[T]] = leaf.children
        val (leftSeed, rightSeed): (Entry[T], Entry[T]) = linearPickSeeds(children)
        val remainingChildren = children.filter(c => c != leftSeed && c != rightSeed)
        val leftLeaf = Leaf(leftSeed)
        val rightLeaf = Leaf(rightSeed)
        createLeaves(remainingChildren, leftLeaf, rightLeaf)
    }


    def linearPickSeeds(children: Vector[Entry[T]]): (Entry[T], Entry[T]) = {
        case class Params(leftMostLeftSide: Double, //dimLb
                          leftMostRightSide: Double, //dimMinUb
                          rightMostLeftSide: Double, //dimMaxLb
                          rightMostRightSide: Double, //dimUb
                          leftEntry: Option[Entry[T]], //nMinLb
                          rightEntry: Option[Entry[T]]) //nMaxLb //TODO delete

        def lpsX(children: Vector[Entry[T]], s: Params): (Double, Entry[T], Entry[T]) = {
            val ch = children.head
            val leftMostLeftSide = Math.min(ch.bound.x, s.leftMostLeftSide)
            val (leftMostRightSide, leftEntry) = if (ch.bound.x2 < s.leftMostRightSide) (ch.bound.x2, Some(ch)) else (s.leftMostRightSide, s.leftEntry)
            val (rightMostLeftSide, rightEntry) = if (ch.bound.x > s.rightMostLeftSide) (ch.bound.x, Some(ch)) else (s.rightMostLeftSide, s.rightEntry)
            val rightMostRightSide = Math.max(ch.bound.x2, s.rightMostRightSide)
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
            val leftMostLeftSide = Math.min(ch.bound.y, s.leftMostLeftSide)
            val (leftMostRightSide, leftEntry) = if (ch.bound.y2 < s.leftMostRightSide) (ch.bound.y2, Some(ch)) else (s.leftMostRightSide, s.leftEntry)
            val (rightMostLeftSide, rightEntry) = if (ch.bound.y > s.rightMostLeftSide) (ch.bound.y, Some(ch)) else (s.rightMostLeftSide, s.rightEntry)
            val rightMostRightSide = Math.max(ch.bound.y2, s.rightMostRightSide)
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

    def adjustTree(n: Node[T], nn: Option[Node[T]]): RTree[T] = {
        if (n == root) {
            RTree(n, n.children.size, minEntries, maxEntries)
        }
    }

    def chooseSubtree(children: Vector[Node[T]], entry: Entry[T]): Node[T] = {
        val childEnlPairs = children.map(c => (c, c.bound.enlargementToFit(entry.bound)))
        val minVal: (Node[T], Double) = childEnlPairs.minBy(_._2)
        val leastEnlChildren = childEnlPairs.filter(_._2 == minVal._2).map(_._1)
        leastEnlChildren.minBy(child => child.bound.area())
    }
}
