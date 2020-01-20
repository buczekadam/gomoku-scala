package game

case class Tree[A](data: A, nodes: LazyList[Tree[A]])
