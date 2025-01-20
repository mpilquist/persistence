package persistence

import scala.math.Ordering

enum SplayTree[K: Ordering, V]:
  case Empty[K: Ordering, V]() extends SplayTree[K, V]

  case Node[K: Ordering, V](
    left: SplayTree[K, V],
    k: K,
    v: V,
    right: SplayTree[K, V]
  ) extends SplayTree[K, V]

  private inline def ordK: Ordering[K] = summon[Ordering[K]]

  def size: Int = this match
    case Empty() => 0
    case Node(l, _, _, r) => 1 + l.size + r.size

  def head: Option[(K, V)] = this match
    case Empty() => None
    case Node(_, k, v, _) => Some((k, v))

  def iterator: Iterator[(K, V)] = this match
    case Empty() => Iterator.empty
    case Node(l, k, v, r) =>
      l.iterator ++ Iterator(k -> v) ++ r.iterator

  def get(k: K): Option[(SplayTree[K, V], V)] =
    def loop(t: SplayTree[K, V]): Option[List[Node[K, V]]] = t match
      case Empty() => None
      case n @ Node(_, k2, v, _) if ordK.equiv(k, k2) => 
        Some(List(n))
      case n @ Node(l, k2, _, _) if ordK.lt(k, k2) => 
        loop(l).map(n :: _)
      case n @ Node(_, _, _, r) => 
        loop(r).map(n :: _)
    loop(this).map: path =>
      val (target, ancestry) = shift(path)
      splay(target, ancestry) -> target.v

  def put(k: K, v: V): SplayTree[K, V] = splayPath(insert(k, v))

  def +(kv: (K, V)): SplayTree[K, V] = put(kv(0), kv(1))

  private def insert(k: K, v: V): List[Node[K, V]] = this match
    case Empty() => List(Node(Empty(), k, v, Empty()))
    case Node(l, k2, v2, r) =>
      if summon[Ordering[K]].lteq(k, k2) then
        val c = l.insert(k, v)
        Node(c.head, k2, v2, r) :: c
      else
        val c = r.insert(k, v)
        Node(l, k2, v2, c.head) :: c

  private def shift(
    path: List[Node[K, V]]
  ): (Node[K, V], List[Either[Node[K, V], Node[K, V]]]) =
  
    def loop(
      rem: List[Node[K, V]], 
      n: Node[K, V], 
      ancestry: List[Either[Node[K, V], Node[K, V]]]
    ): (Node[K, V], List[Either[Node[K, V], Node[K, V]]]) =
      rem match
        case Nil => (n, ancestry)
        case hd :: tl =>
          if (n.left eq hd) then loop(tl, hd, Left(n) :: ancestry)
          else loop(tl, hd, Right(n) :: ancestry)

    loop(path.tail, path.head, Nil)

  private def splayPath(path: List[Node[K, V]]): SplayTree[K, V] =
    val shifted = shift(path)
    splay(shifted(0), shifted(1))

  private def splay(node: Node[K, V], ancestry: List[Either[Node[K, V], Node[K, V]]]): SplayTree[K, V] =
    ancestry match
      case Nil => node

      case Left(root) :: Nil =>
        // left zig
        Node(node.left, node.k, node.v, Node(node.right, root.k, root.v, root.right))

      case Right(root) :: Nil =>
        // right zig
        Node(Node(root.left, root.k, root.v, node.left), node.k, node.v, node.right)

      case Left(parent) :: Left(grandparent) :: tl =>
        // left zig zig
        val newNode = Node(node.left, node.k, node.v,
          Node(node.right, parent.k, parent.v,
            Node(parent.right, grandparent.k, grandparent.v, grandparent.right)))
        splay(newNode, tl)

      case Right(parent) :: Right(grandparent) :: tl =>
        // right zig zig
        val newNode = Node(
          Node(
            Node(grandparent.left, grandparent.k, grandparent.v, parent.left),
            parent.k, parent.v,
            node.left
          ),
          node.k, node.v,
          node.right
        )
        splay(newNode, tl)

      case Right(parent) :: Left(grandparent) :: tl =>
        // left zig zag
        val newNode = Node(
          Node(parent.left, parent.k, parent.v, node.left),
          node.k, node.v,
          Node(node.right, grandparent.k, grandparent.v, grandparent.right))
        splay(newNode, tl)


      case Left(parent) :: Right(grandparent) :: tl =>
        // right zig zag
        val newNode = Node(
          Node(grandparent.left, grandparent.k, grandparent.v, node.left),
          node.k, node.v,
          Node(node.right, parent.k, parent.v, parent.right)
        )
        splay(newNode, tl)

object SplayTree:
  def empty[K: Ordering, V]: SplayTree[K, V] = Empty()
