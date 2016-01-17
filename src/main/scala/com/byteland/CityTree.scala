package com.byteland

case class NodeCity[T](id: T, connected: List[CityTree[T]] = Nil) extends CityTree[T]
case class LeafCity[T](id: T) extends CityTree[T]

trait CityTree[+T] {

  import scala.annotation.tailrec

  def getId: T = this match {
    case n: NodeCity[T] ⇒ n.id
    case l: LeafCity[T] ⇒ l.id
  }

  def getConnected: List[CityTree[T]] = this match {
    case n: NodeCity[T] ⇒ n.connected
    case l: LeafCity[T] ⇒ Nil
  }

  def isLeaf: Boolean = this match {
    case n: NodeCity[T] => false
    case l: LeafCity[T] => true
  }

  private case class EvalCity[A](id: A) extends CityTree[A]

  @tailrec
  private def foldLoop[A, B](a: List[CityTree[A]], z: B)(f: (B, A) ⇒ B)(o: (NodeCity[A], List[CityTree[A]]) ⇒ List[CityTree[A]]): B = a match {
    case (n: NodeCity[A]) :: tl ⇒ foldLoop(o(n, tl), z)(f)(o) // never directly evaluate nodes, function o will create new accumulator
    case (l: LeafCity[A]) :: tl ⇒ foldLoop(tl, f(z, l.id))(f)(o) // always evaluate Leaf
    case (e: EvalCity[A]) :: tl ⇒ foldLoop(tl, f(z, e.id))(f)(o) // always evaluate Eval
    case _                      ⇒ z // will be Nil (empty list)
  }

  def foldPreOrder[B](z: B)(f: (B, T) ⇒ B): B = {
    foldLoop(List(this), z)(f) { (n, tl) ⇒ EvalCity(n.id) :: n.connected ::: tl }
  }

  def foldPostOrder[B](z: B)(f: (B, T) ⇒ B): B = {
    foldLoop(List(this), z)(f) { (n, tl) ⇒ n.connected ::: EvalCity(n.id) :: tl }
  }

  def foldLevelOrder[B](z: B)(f: (B, T) ⇒ B): B = {
    foldLoop(List(this), z)(f) { (n, tl) ⇒ (EvalCity(n.id) :: tl) ::: n.connected }
  }

  def fold[B](z: B)(f: (B, T) ⇒ B): B = foldPreOrder(z)(f)

  def size: Int = fold(0) { (sum, _) ⇒ sum + 1 }

  def height: Int = {
    def loop[A](t: CityTree[A]): Int = t match {
      case l: LeafCity[A] ⇒ 1
      case n: NodeCity[A] ⇒ n.getConnected.map(loop(_)).max + 1
      case _              ⇒ 0
    }
    loop(this) - 1
  }

  def leafCount: Int = {
    @tailrec
    def loop[A](t: List[CityTree[A]], z: Int): Int = t match {
      case (l: LeafCity[A]) :: tl ⇒ loop(tl, z + 1)
      case (n: NodeCity[A]) :: tl ⇒ loop(n.getConnected ::: tl, z)
      case _ :: tl                ⇒ loop(tl, z)
      case _                      ⇒ z
    }
    loop(List(this), 0)
  }

  def subTrees: List[CityTree[T]] = {
    @tailrec
    def loop[A](t: List[CityTree[A]], z: List[CityTree[A]]): List[CityTree[A]] = t match {
      case (l: LeafCity[A]) :: tl ⇒ loop(tl, z)
      case (n: NodeCity[A]) :: tl ⇒ loop(n.getConnected ::: tl, n :: z)
      case _ :: tl                ⇒ loop(tl, z)
      case _                      ⇒ z
    }
    loop(List(this), Nil)
  }

  def findById[B >: T](cityId: B): Option[CityTree[T]] = {
    @tailrec
    def loop[A](t: List[CityTree[A]], z: Option[CityTree[A]]): Option[CityTree[A]] = t match {
      case (l: LeafCity[A]) :: tl if l.getId == cityId ⇒ Some(l)
      case (l: LeafCity[A]) :: tl ⇒ loop(tl, z)
      case (n: NodeCity[A]) :: tl if n.getId == cityId ⇒ Some(n)
      case (n: NodeCity[A]) :: tl ⇒ loop(n.getConnected ::: tl, z)
      case _ :: tl                ⇒ loop(tl, z)
      case _                      ⇒ z
    }
    loop(List(this), None)
  }

  def toSeq: Seq[T] = fold(List[T]()) { (l, v) ⇒ v :: l }.reverse

  def toSeqPreOrder: Seq[T] = foldPreOrder(List[T]()) { (l, v) ⇒ v :: l }.reverse
  def toSeqPostOrder: Seq[T] = foldPostOrder(List[T]()) { (l, v) ⇒ v :: l }.reverse
  def toSeqLevelOrder: Seq[T] = foldLevelOrder(List[T]()) { (l, v) ⇒ v :: l }.reverse

  def lastPreOrder: T = toSeqPreOrder.last
  def lastPostOrder: T = toSeqPostOrder.last
  def lastLevelOrder: T = toSeqLevelOrder.last

  def nthPreOrder(n: Int): T = toSeqPreOrder(n)
  def nthPostOrder(n: Int): T = toSeqPostOrder(n)
  def nthLevelOrder(n: Int): T = toSeqLevelOrder(n)

}
