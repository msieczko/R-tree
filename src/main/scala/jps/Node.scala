package jps

trait HasBounds {
    def bound: Bound
}

sealed abstract class Node[T] extends HasBounds {
    def :+(child: HasBounds): Node[T]
    def ++(children: Vector[HasBounds]): Node[T]
    def bound: Bound
    def children: Vector[HasBounds]
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
}

object Leaf {
    def apply[T](singleChild: Entry[T]): Leaf[T] = {
        apply(Vector[Entry[T]](singleChild))
    }

    def apply[T](children: Vector[Entry[T]]): Leaf[T] = {
        if (children.nonEmpty) {
            Leaf(children, Node.getBound(children))
        } else {
            Node.newRoot[T] //TODO check for correctness
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
}

object Branch {
    def apply[T](singleChild: Node[T]): Branch[T] = {
        apply(Vector[Node[T]](singleChild))
    }

    def apply[T](children: Vector[Node[T]]): Branch[T] = {
        if (children.nonEmpty) {
            Branch(children, Node.getBound(children))
        } else {
            ???
            //TODO this should never happen
        }
    }
}

case class Entry[T](bound: Bound, value: T) extends HasBounds
