package jps

object RTree {
    def instance[T]: RTree[T] = new RTree[T](Node.newRoot, 0)
}

case class RTree[T](root: Node[T], size: Int) {
    def insert(rectangle: Rectangle, value: T): RTree[T] = {
        insert(Entry[T](rectangle, value))
    }

    def insert(entry: Entry[T]): RTree[T] = {
        val destNode: Node[T] = chooseLeaf(root, entry)
    }

    def chooseLeaf(root: Node[T], entry: Entry[T]): Node[T] = {
        val n = root
        n match {
            case n : Leaf[T] => n
            case n : Branch[T] => chooseChild(n.children, entry)
        }
    }

    def chooseChild(children: Vector[Node[T]], entry: Entry[T]): Node[T] = {
        val enlargements = children.map(c => (c, c.boundingRect.enlargementToFit(entry.rectangle)))
        val minVal = enlargements.minBy(_._2)
        val cos = enlargements.filter(_._2 == minVal._2).map(_._1)
        cos.minBy(child => child.boundingRect.area())
    }


//    val r = root.insert(entry) match {
//        case Left(rs) => Branch(rs, rs.foldLeft(Box.empty)(_ expand _.box))
//        case Right(r) => r
//    }
}
