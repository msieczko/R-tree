package jps

trait HasBounds {
    def bound: Bound
}

sealed abstract class Node[T] extends HasBounds {
    def bound: Bound
    def children: Vector[HasBounds]
}

object Node {
    def newRoot[T]: Leaf[T] = Leaf(Vector.empty, Rectangle.maxRect)
}

case class Leaf[T](children: Vector[Entry[T]], bound: Bound) extends Node[T]

object Leaf {
    def apply[T](singleChild: Entry[T]): Leaf[T] = {
        apply(Vector[Entry[T]](singleChild))
    }

    def apply[T](children: Vector[Entry[T]]): Leaf[T] = {
        if (children.nonEmpty) {
            Leaf(children, getBound(children))
        } else {
            Node.newRoot[T] //TODO check for correctness
        }
    }

    private def getBound[T](children: Vector[Entry[T]]): Bound = {
        def create(child: Entry[T], remainingChildren: Vector[Entry[T]], bound: Bound): Bound = {
            val newX = Math.min(child.bound.x, bound.x)
            val newY = Math.min(child.bound.y, bound.y)
            val newX2 = Math.max(child.bound.x2, bound.x2)
            val newY2 = Math.max(child.bound.y2, bound.y2)
            val newBound = Rectangle(Coordinates(newX, newY), Coordinates(newX2, newY2))

            if (remainingChildren.nonEmpty) {
                create(children.head, children.tail, newBound)
            } else {
                newBound
            }
        }

        create(children.head, children.tail, children.head.bound)
    }


}

case class Branch[T](children: Vector[Node[T]], bound: Bound) extends Node[T]

case class Entry[T](bound: Bound, value: T) extends HasBounds
