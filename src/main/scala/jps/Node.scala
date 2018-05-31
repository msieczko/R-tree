package jps

trait HasBounds {
    def bound: Bound
}

sealed abstract class Node[T] extends HasBounds {
    def :+(child: HasBounds): Node[T]

    def ++(children: Vector[HasBounds]): Node[T]

    def bound: Bound

    def children: Vector[HasBounds]

    def remove(entry: Entry[T], minEntries: Int): Option[(Vector[Entry[T]], Option[Node[T]])]

    def getAllEntries: Vector[Entry[T]]
}

object Node {
    def newRoot[T]: Leaf[T] = Leaf(Vector.empty, Rectangle.maxRect)

    def getBound[T](children: Vector[HasBounds]): Bound = {
        def create(child: HasBounds, remainingChildren: Vector[HasBounds], bound: Bound): Bound = {
            val newX = Math.min(child.bound.x, bound.x)
            val newY = Math.min(child.bound.y, bound.y)
            val newX2 = Math.max(child.bound.x2, bound.x2)
            val newY2 = Math.max(child.bound.y2, bound.y2)
            val newBound = Rectangle(Coordinates(newX, newY), Coordinates(newX2, newY2))

            if (remainingChildren.nonEmpty) {
                create(remainingChildren.head, remainingChildren.tail, newBound)
            } else {
                newBound
            }
        }

        create(children.head, children.tail, children.head.bound)
    }
}

case class Leaf[T](children: Vector[Entry[T]], bound: Bound) extends Node[T] {
    override def :+(entry: HasBounds): Leaf[T] = {
        Leaf(children :+ entry.asInstanceOf[Entry[T]])
    }

    override def ++(children: Vector[HasBounds]): Node[T] = {
        Leaf(this.children ++ children.asInstanceOf[Vector[Entry[T]]])
    }

    override def getAllEntries: Vector[Entry[T]] = children

    override def remove(entry: Entry[T], minEntries: Int): Option[(Vector[Entry[T]], Option[Node[T]])] = {
        val entryToRemove: Option[Entry[T]] = children.find(_ == entry)
        if (entryToRemove.isEmpty) {
            None
        } else if (children.size == minEntries) {
            Some(children.filter(_ != entryToRemove.get), None)
        } else {
            Some(Vector.empty, Some(Leaf(children.filter(_ != entryToRemove.get))))
        }
    }
}

object Leaf {
    def apply[T](singleChild: Entry[T]): Leaf[T] = {
        apply(Vector[Entry[T]](singleChild))
    }

    def apply[T](children: Vector[Entry[T]]): Leaf[T] = {
        if (children.nonEmpty) {
            Leaf(children, Node.getBound(children))
        } else {
            Node.newRoot[T]
        }
    }
}

case class Branch[T](children: Vector[Node[T]], bound: Bound) extends Node[T] {
    override def :+(child: HasBounds): Node[T] = {
        Branch(children :+ child.asInstanceOf[Node[T]])
    }

    override def ++(children: Vector[HasBounds]): Node[T] = {
        Branch(this.children ++ children.asInstanceOf[Vector[Node[T]]])
    }

    override def getAllEntries: Vector[Entry[T]] = {
        children.flatMap(_.getAllEntries)
    }

    override def remove(entry: Entry[T], minEntries: Int): Option[(Vector[Entry[T]], Option[Node[T]])] = {
        children.zipWithIndex.foreach { case (child, i) =>
            if (child.bound.overlap(entry.bound)) {
                child.remove(entry, minEntries) match {
                    case Some((q, Some(node))) =>
                        return Some(q, Some(Branch((children.take(i) :+ node) ++ children.drop(i + 1))))
                    case Some((q, None)) =>
                        if (children.size == minEntries) {
                            val otherChildrensEntries = children.filter(_ != child).flatMap(_.getAllEntries)
                            return Some(q ++ otherChildrensEntries, None)
                        }
                        return Some(q, Some(Branch(children.filter(_ != child))))
                    case None =>
                }
            }
        }
        None
    }
}

object Branch {
    def apply[T](singleChild: Node[T]): Branch[T] = {
        apply(Vector[Node[T]](singleChild))
    }

    def apply[T](children: Vector[Node[T]]): Branch[T] = {
        if (children.nonEmpty) {
            Branch(children, Node.getBound(children))
        } else {
            ??? //this should never happen
        }
    }
}

/**
  * Constructs a new entry
  *
  * @param bound the bound of the entry
  * @param value the value of the entry
  * @tparam T the type of the value
  */
case class Entry[T](bound: Bound, value: T) extends HasBounds
