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
    case n: NodeCity[T] ⇒ false
    case l: LeafCity[T] ⇒ true
  }

  private case class EvalCity[A](id: A) extends CityTree[A]

  /**
   * Private function to manage all fold operations with parameter changes.
   * @param a tree to be folded
   * @param z accumalator of the fold operation
   * @param f function of the fold operation
   * @param eval function for creating new acculamator according to the aim of traverse
   *          by converting NodeCity to EvalCity with connected cities.
   * @tparam A [[CityTree]] type param
   * @tparam B Accumulator type param
   * @return folded result
   */
  @tailrec
  private def foldLoop[A, B](a: List[CityTree[A]], z: B)(f: (B, A) ⇒ B)(eval: (NodeCity[A], List[CityTree[A]]) ⇒ List[CityTree[A]]): B = a match {
    case (n: NodeCity[A]) :: tl ⇒ foldLoop(eval(n, tl), z)(f)(eval)
    case (l: LeafCity[A]) :: tl ⇒ foldLoop(tl, f(z, l.id))(f)(eval)
    case (e: EvalCity[A]) :: tl ⇒ foldLoop(tl, f(z, e.id))(f)(eval)
    case _                      ⇒ z
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

  def toSeqPreOrder: Seq[T] = foldPreOrder(List[T]()) { (l, v) ⇒ v :: l }.reverse
  def toSeqPostOrder: Seq[T] = foldPostOrder(List[T]()) { (l, v) ⇒ v :: l }.reverse
  def toSeqLevelOrder: Seq[T] = foldLevelOrder(List[T]()) { (l, v) ⇒ v :: l }.reverse

  /**
   * size of the tree in terms of node count
   * @return node count
   */
  def size: Int = foldPreOrder(0) { (sum, _) ⇒ sum + 1 }

  /**
   * Return the height of the tree. From root to deepest leaf.
   * @return height
   */
  def height: Int = {
    def loop[A](t: CityTree[A]): Int = t match {
      case l: LeafCity[A] ⇒ 1
      case n: NodeCity[A] ⇒ n.getConnected.map(loop(_)).max + 1
      case _              ⇒ 0
    }
    loop(this) - 1
  }

  /**
   * Return the leaf count of the tree.
   * @return
   */
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

  /**
   * Gather the sub-trees.
   * @return the sub-trees of the tree.
   */
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

  /**
   * Return the city with given id.
   * @param cityId to be searched in tree
   * @tparam B [[CityTree]] type param
   * @return Option of city.
   */
  def findById[B >: T](cityId: B): Option[CityTree[T]] = {
    @tailrec
    def loop[A](t: List[CityTree[A]], z: Option[CityTree[A]]): Option[CityTree[A]] = t match {
      case (l: LeafCity[A]) :: tl if l.getId == cityId ⇒ Some(l)
      case (l: LeafCity[A]) :: tl ⇒ loop(tl, z)
      case (n: NodeCity[A]) :: tl if n.getId == cityId ⇒ Some(n)
      case (n: NodeCity[A]) :: tl ⇒ loop(n.getConnected ::: tl, z)
      case _ :: tl ⇒ loop(tl, z)
      case _ ⇒ z
    }
    loop(List(this), None)
  }

}
