package jps

object RTree {
    /**
      * Constructs an empty R-Tree with given parameters
      *
      * @param minEntries the minimum number of children in a node
      * @param maxEntries the maximum number of children in a node
      * @tparam T type of data in entries
      * @return an empty R-Tree with given parameters
      */
    def apply[T](minEntries: Int, maxEntries: Int): RTree[T] = {
        RTree[T](Node.newRoot[T], 0, minEntries, maxEntries)
    }

    /**
      * Constructs an empty R-Tree with default parameters values:
      *
      * <ul>
      * <li>`minEntries` - minimum number of children in a node set to 2</li>
      * <li>`maxEntries` - maximum number of children in a node set to 50</li>
      * </ul>
      *
      * @tparam T type of data in entries
      * @return an empty R-Tree with minimum number of children in a node set to 2
      *         and maximum number of children in a node set to 50
      */
    def apply[T](): RTree[T] = {
        RTree[T](Node.newRoot[T], 0, 2, 50)
    }
}

case class RTree[T](root: Node[T], size: Int, minEntries: Int, maxEntries: Int) {
    require((2 <= minEntries) && (minEntries <= (maxEntries + 1) / 2),
        "min/max number of entries must satisfy the condition: 2 <= minEntries <= (maxEntries + 1) / 2")

    /**
      * Removes the given entry from the R-Tree
      *
      * @param entry the entry to remove
      * @return new R-Tree without given entry
      */
    def remove(entry: Entry[T]): RTree[T] = {
        root.remove(entry, minEntries) match {
            case Some((q, Some(newRoot))) =>
                q.foldLeft(this.copy(newRoot, size - q.size - 1))(_ insert _)
            case Some((q, None)) =>
                q.foldLeft(RTree[T](minEntries, maxEntries))(_ insert _)
            case None =>
                throw new NoSuchElementException
        }
    }

    /**
      * Finds entries within bound<br />
      *
      * e.g.<code> val searchBound: Bound = Rectangle(Coordinates(-3, -5), Coordinates(1, 3))<br />
      *  val result: Vector[Entry[String]] = rtree.search(searchBound)</code>
      *
      * @param searchBound the bound to search within
      * @return entries within given bound
      */
    def search(searchBound: Bound): Vector[Entry[T]] = {
        searchTree(root, searchBound)
    }

    private def searchTree(node: Node[T], searchBound: Bound): Vector[Entry[T]] = {
        if (node.isInstanceOf[Leaf[T]]) {
            return node.children.filter(ch => ch.bound.overlap(searchBound))
                .map(ch => ch.asInstanceOf[Entry[T]])
        }
        node.children.filter(ch => ch.bound.overlap(searchBound))
            .flatMap(ch => searchTree(ch.asInstanceOf[Node[T]], searchBound))
    }

    /**
      * Inserts a new entry with given bound and value to the R-Tree
      *
      * @param bound the bound of the value
      * @param value the value to insert
      * @return new R-Tree with the `entry` element inserted
      */
    def insert(bound: Bound, value: T): RTree[T] = {
        insert(Entry[T](bound, value))
    }

    /**
      * Inserts a new entry to the R-Tree
      *
      * @param entry the entry to insert
      * @return new R-Tree with the `entry` element inserted
      */
    def insert(entry: Entry[T]): RTree[T] = {
        if (size == 0) {
            return this.copy(Leaf(entry), 1)
        }
        val newRoot = insert(root, entry) match {
            case Left(splitRoot) => Branch(Vector(splitRoot._1, splitRoot._2))
            case Right(oldRoot) => oldRoot
        }
        this.copy(newRoot, size + 1)
    }

    private def insert(node: Node[T], entry: Entry[T]): Either[(Node[T], Node[T]), Node[T]] = {
        def createNode(newChildren: Vector[HasBounds]): Node[T] = {
            newChildren(0) match {
                case _: Entry[T] => Leaf(newChildren.asInstanceOf[Vector[Entry[T]]])
                case _: Leaf[T] => Branch(newChildren.asInstanceOf[Vector[Leaf[T]]])
                case _: Branch[T] => Branch(newChildren.asInstanceOf[Vector[Branch[T]]])
            }
        }

        if (node.isInstanceOf[Leaf[T]]) {
            if (node.children.size < maxEntries) {
                return Right(node :+ entry)
            } else {
                return Left(splitNode(node :+ entry))
            }
        }
        val nodeToDescend = chooseSubtree(node.children.asInstanceOf[Vector[Node[T]]], entry)
        insert(nodeToDescend, entry) match {
            case Left(splitNodes) =>
                val newChildren = node.children.filter(n => n != nodeToDescend) :+ splitNodes._1 :+ splitNodes._2
                val newNode: Node[T] = createNode(newChildren)
                if (node.children.size == maxEntries) {
                    Left(splitNode(newNode))
                } else {
                    Right(newNode)
                }
            case Right(singleNode) => {
                val newChildren = node.children.filter(n => n != nodeToDescend) :+ singleNode
                Right(createNode(newChildren))
            }
        }
    }

    private def linearPickNext(remainingChildren: Vector[HasBounds]): (HasBounds, Vector[HasBounds]) = {
        (remainingChildren.head, remainingChildren.tail)
    }

    private def distribute(remainingChildren: Vector[HasBounds], leftNode: Node[T], rightNode: Node[T]): (Node[T], Node[T]) = {
        if (remainingChildren.isEmpty) {
            return (leftNode, rightNode)
        }

        if (leftNode.children.size >= minEntries && rightNode.children.size + remainingChildren.size == minEntries) {
            return (leftNode, rightNode ++ remainingChildren)
        } else if (rightNode.children.size >= minEntries && leftNode.children.size + remainingChildren.size == minEntries) {
            return (leftNode ++ remainingChildren, rightNode)
        }

        val (child, newRemaining) = linearPickNext(remainingChildren)

        val leftEnlargement = leftNode.bound.enlargementToFit(child.bound)
        val rightEnlargement = rightNode.bound.enlargementToFit(child.bound)
        if (leftEnlargement > rightEnlargement) {
            return distribute(newRemaining, leftNode, rightNode :+ child)
        } else if (leftEnlargement < rightEnlargement) {
            return distribute(newRemaining, leftNode :+ child, rightNode)
        }

        val leftArea = leftNode.bound.area()
        val rightArea = rightNode.bound.area()
        if (leftArea > rightArea) {
            return distribute(newRemaining, leftNode, rightNode :+ child)
        } else if (leftArea < rightArea) {
            return distribute(newRemaining, leftNode :+ child, rightNode)
        }

        if (leftNode.children.size > rightNode.children.size) {
            distribute(newRemaining, leftNode, rightNode :+ child)
        } else {
            distribute(newRemaining, leftNode :+ child, rightNode)
        }
    }

    private def splitNode(node: Node[T]): (Node[T], Node[T]) = {
        val children: Vector[HasBounds] = node.children
        val (leftSeed, rightSeed): (HasBounds, HasBounds) = linearPickSeeds(children)
        val remainingChildren = children.filter(c => c != leftSeed && c != rightSeed)
        val leftNode = leftSeed match {
            case leftSeed: Entry[T] => Leaf(leftSeed)
            case leftSeed: Leaf[T] => Branch(leftSeed)
            case leftSeed: Branch[T] => Branch(leftSeed)
        }
        val rightNode = rightSeed match {
            case rightSeed: Entry[T] => Leaf(rightSeed)
            case rightSeed: Leaf[T] => Branch(rightSeed)
            case rightSeed: Branch[T] => Branch(rightSeed)
        }
        distribute(remainingChildren, leftNode, rightNode)
    }


    private def linearPickSeeds(children: Vector[HasBounds]): (HasBounds, HasBounds) = {
        case class Params(leftMostLeftSide: Double,
                          leftMostRightSide: Double,
                          rightMostLeftSide: Double,
                          rightMostRightSide: Double,
                          leftEntry: Option[HasBounds],
                          rightEntry: Option[HasBounds])

        def lpsX(ch: HasBounds, remaining: Vector[HasBounds], p: Params): (Double, HasBounds, HasBounds) = {
            val leftMostLeftSide = Math.min(ch.bound.x, p.leftMostLeftSide)
            val (leftMostRightSide, leftEntry) = if (ch.bound.x2 < p.leftMostRightSide) (ch.bound.x2, Some(ch)) else (p.leftMostRightSide, p.leftEntry)
            val (rightMostLeftSide, rightEntry) = if (ch.bound.x > p.rightMostLeftSide) (ch.bound.x, Some(ch)) else (p.rightMostLeftSide, p.rightEntry)
            val rightMostRightSide = Math.max(ch.bound.x2, p.rightMostRightSide)
            val params = Params(leftMostLeftSide, leftMostRightSide, rightMostLeftSide, rightMostRightSide, leftEntry, rightEntry)
            if (remaining.nonEmpty) {
                lpsX(remaining.head, remaining.tail, params)
            } else {
                val separation = Math.abs((leftMostRightSide - rightMostLeftSide) / (rightMostRightSide - leftMostLeftSide))
                (separation, leftEntry.get, rightEntry.get)
            }
        }

        def lpsY(ch: HasBounds, remaining: Vector[HasBounds], p: Params): (Double, HasBounds, HasBounds) = {
            val leftMostLeftSide = Math.min(ch.bound.y, p.leftMostLeftSide)
            val (leftMostRightSide, leftEntry) = if (ch.bound.y2 < p.leftMostRightSide) (ch.bound.y2, Some(ch)) else (p.leftMostRightSide, p.leftEntry)
            val (rightMostLeftSide, rightEntry) = if (ch.bound.y > p.rightMostLeftSide) (ch.bound.y, Some(ch)) else (p.rightMostLeftSide, p.rightEntry)
            val rightMostRightSide = Math.max(ch.bound.y2, p.rightMostRightSide)
            val params = Params(leftMostLeftSide, leftMostRightSide, rightMostLeftSide, rightMostRightSide, leftEntry, rightEntry)
            if (remaining.nonEmpty) {
                lpsY(remaining.head, remaining.tail, params)
            } else {
                val separation = Math.abs((leftMostRightSide - rightMostLeftSide) / (rightMostRightSide - leftMostLeftSide))
                (separation, leftEntry.get, rightEntry.get)
            }
        }

        val initParams = Params(Double.MaxValue, Double.MaxValue, Double.MinValue, Double.MinValue, None, None)
        val xSeeds = lpsX(children.head, children.tail, initParams)
        val ySeeds = lpsY(children.head, children.tail, initParams)
        if (xSeeds._1 > ySeeds._1) (xSeeds._2, xSeeds._3) else (ySeeds._2, ySeeds._3)
    }

    private def chooseSubtree(children: Vector[Node[T]], entry: Entry[T]): Node[T] = {
        val childEnlPairs = children.map(c => (c, c.bound.enlargementToFit(entry.bound)))
        val minVal: (Node[T], Double) = childEnlPairs.minBy(_._2)
        val leastEnlChildren = childEnlPairs.filter(_._2 == minVal._2).map(_._1)
        leastEnlChildren.minBy(child => child.bound.area())
    }
}
