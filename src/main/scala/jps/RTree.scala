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
        if (size == 0) {
            return RTree(Leaf(entry), 1, minEntries, maxEntries)
        }
        val newRoot = insert(root, entry) match {
            case Left(splitRoot) => Branch(Vector(splitRoot._1, splitRoot._2))
            case Right(oldRoot) => oldRoot
        }
        RTree(newRoot, size + 1, minEntries, maxEntries)
    }

    def insert(node: Node[T], entry: Entry[T]): Either[(Node[T], Node[T]), Node[T]] = {
        val nodeToDescend = chooseSubtree(node.children.asInstanceOf[Vector[Node[T]]], entry)
        insert(nodeToDescend, entry) match {
            case Left(splitNodes) =>
                val newChildren = node.children.filter(n => n != nodeToDescend) :+ splitNodes._1 :+ splitNodes._2
                val newNode = newChildren(0) match {
                    case _: Entry[T] => Leaf(newChildren.asInstanceOf[Entry[T]])
                    case _: Leaf[T] => Branch(newChildren.asInstanceOf[Leaf[T]])
                    case _: Branch[T] => Branch(newChildren.asInstanceOf[Branch[T]])
                }
                if (node.children.size == maxEntries) {
                    Left(splitNode(newNode))
                } else {
                    Right(newNode)
                }
            case Right(singleNode) => Right(singleNode)
        }
    }

    def chooseLeaf(root: Node[T], entry: Entry[T]): Node[T] = {
        val n = root
        n match {
            case n: Leaf[T] => n
            case n: Branch[T] => chooseLeaf(chooseSubtree(n.children, entry), entry)
        }
    }


    def linearPickNext(remainingChildren: Vector[HasBounds]): (HasBounds, Vector[HasBounds]) = {
        (remainingChildren.head, remainingChildren.tail)
    }

    def distribute(remainingChildren: Vector[HasBounds], leftNode: Node[T], rightNode: Node[T]): (Node[T], Node[T]) = {
        if (remainingChildren.isEmpty) {
            return (leftNode, rightNode)
        }
        val (child, newRemaining) = linearPickNext(remainingChildren)
        //TODO check this condition:
        // if one group has so few entries that all remaining entries must be assigned to it in order for it to have
        // minimum number of entries, assign them and stop

        val leftEnlargement = leftNode.bound.enlargementToFit(child.bound)
        val rightEnlargement = rightNode.bound.enlargementToFit(child.bound)
        if (leftEnlargement > rightEnlargement) {
            distribute(newRemaining, leftNode, rightNode :+ child)
        } else if (leftEnlargement < rightEnlargement) {
            distribute(newRemaining, leftNode :+ child, rightNode)
        }

        val leftArea = leftNode.bound.area()
        val rightArea = rightNode.bound.area()
        if (leftArea > rightArea) {
            distribute(newRemaining, leftNode, rightNode :+ child)
        } else if (leftArea < rightArea) {
            distribute(newRemaining, leftNode :+ child, rightNode)
        }

        if (leftNode.children.size > rightNode.children.size) {
            distribute(newRemaining, leftNode, rightNode :+ child)
        } else {
            distribute(newRemaining, leftNode :+ child, rightNode)
        }
    }

    def splitNode(node: Node[T]): (Node[T], Node[T]) = {
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


    def linearPickSeeds(children: Vector[HasBounds]): (HasBounds, HasBounds) = {
        case class Params(leftMostLeftSide: Double, //dimLb
                          leftMostRightSide: Double, //dimMinUb
                          rightMostLeftSide: Double, //dimMaxLb
                          rightMostRightSide: Double, //dimUb
                          leftEntry: Option[HasBounds], //nMinLb
                          rightEntry: Option[HasBounds]) //nMaxLb //TODO delete

        def lpsX(children: Vector[HasBounds], s: Params): (Double, HasBounds, HasBounds) = {
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

        def lpsY(children: Vector[HasBounds], s: Params): (Double, HasBounds, HasBounds) = {
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
        ???
    }

    def chooseSubtree(children: Vector[Node[T]], entry: Entry[T]): Node[T] = {
        val childEnlPairs = children.map(c => (c, c.bound.enlargementToFit(entry.bound)))
        val minVal: (Node[T], Double) = childEnlPairs.minBy(_._2)
        val leastEnlChildren = childEnlPairs.filter(_._2 == minVal._2).map(_._1)
        leastEnlChildren.minBy(child => child.bound.area())
    }
}
