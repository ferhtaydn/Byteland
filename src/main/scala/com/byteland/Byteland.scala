package com.byteland

import scala.annotation.tailrec
import scala.collection.mutable

//noinspection ScalaStyle
object Byteland {

  def findStepCount(globalTree: CityTree[String], globalBlacklist: mutable.Set[mutable.Set[String]]): Int = {

    /**
     * Filter already unified cities.
     * First, check the local blacklist with tree root id and
     * Second, check the global black list to filter with tree root id and its connected city ids.
     * @param tree tree to be filtered
     * @param localBlacklist set of connected cities in the current evaluation step.
     * @return filtered tree
     */
    def filterAlreadyUnifiedCities(tree: CityTree[String], localBlacklist: Set[String]): List[CityTree[String]] = {
      val treeId = tree.getId.split("-").head
      if (localBlacklist.contains(treeId)) {
        Nil
      } else {
        tree.getConnected.filterNot(c ⇒ localBlacklist.contains(c.getId)).filterNot { c ⇒
          globalBlacklist.exists(set ⇒ set.contains(treeId) && set.contains(c.getId))
        }
      }
    }

    /**
     * Remove a city (t) from the connected list of tree (tree) after connection operation has done.
     * @param tree currently unifying tree
     * @param t unified with root city. Removing from connected list.
     * @return the updated list list of connected cities
     */
    def remainingConnectedCities(tree: CityTree[String], t: CityTree[String]): List[CityTree[String]] = {
      tree.getConnected.filterNot(c ⇒ c.getId == t.getId)
    }

    /**
     * Create the id of the new tree from the unified a and b
     * @param a unified with b
     * @param b unified with a
     * @return
     */
    def unifiedId(a: CityTree[String], b: CityTree[String]): String = s"${a.getId}-${b.getId}"

    /**
     * Updating the local and global black lists. They hold the currently unified cities.
     * @param c unified with b
     * @param b unified with c
     * @param globalBlacklist a set of unified cities until now.
     * @param localBlacklist a set of unified cities in that session.
     */
    def updateBlacklists(c: CityTree[String], b: CityTree[String],
      globalBlacklist: mutable.Set[mutable.Set[String]],
      localBlacklist: mutable.Set[String]): Unit = {

      val treeIds = c.getId.split("-")
      val itemIds = b.getId.split("-")

      /**
       * many mutation on [[mutable.Set]] of blacklists.
       */
      val itemSet = globalBlacklist.find(set ⇒ set.contains(itemIds.head)) match {
        case None      ⇒ mutable.Set(itemIds: _*)
        case Some(set) ⇒ globalBlacklist.remove(set); set
      }

      globalBlacklist.find(set ⇒ set.contains(treeIds.head)) match {
        case None      ⇒ globalBlacklist += (itemSet ++ treeIds)
        case Some(set) ⇒ globalBlacklist.remove(set); globalBlacklist += (set ++ itemSet)
      }

      localBlacklist ++= globalBlacklist.find(set ⇒ set.contains(itemIds.head)).getOrElse(mutable.Set.empty[String])

    }

    /**
     * The main logic for unification of sub-trees.
     * @param c the tree to be unified with its connected cities.
     * @param globalBlacklist a set of unified cities until now.
     * @param localBlacklist a set of unified cities in that session.
     * @return unified city
     */
    def unify(c: CityTree[String], globalBlacklist: mutable.Set[mutable.Set[String]],
      localBlacklist: mutable.Set[String]): CityTree[String] = {

      filterAlreadyUnifiedCities(c, localBlacklist.toSet).headOption match {
        case None ⇒ c
        case Some(t) ⇒ t match {
          case a: LeafCity[String] ⇒
            updateBlacklists(c, a, globalBlacklist, localBlacklist)
            remainingConnectedCities(c, a) match {
              case Nil ⇒ LeafCity(unifiedId(c, a))
              case cs  ⇒ NodeCity(unifiedId(c, a), cs)
            }
          case b: NodeCity[String] ⇒
            updateBlacklists(c, b, globalBlacklist, localBlacklist)
            NodeCity(unifiedId(c, b), remainingConnectedCities(c, b) ::: b.getConnected)
        }
      }
    }

    /**
     * Checker to decide complete the unification of Byteland
     * @return stop or continue boolean flag.
     */
    def unificationCompleted: Boolean = {
      globalBlacklist.size == 1 && (globalBlacklist.head.size == globalTree.size)
    }

    /**
     * Main logic of the unification of global tree.
     * @param subtrees of the glboal tree of Byteland
     * @param step holds the unification step count
     * @return step
     */
    def loop(subtrees: List[CityTree[String]], step: Int): Int = {
      val localBlacklist = mutable.Set.empty[String]
      val res = subtrees.map(unify(_, globalBlacklist, localBlacklist)).filterNot(_.isLeaf)
      if (unificationCompleted) step else loop(res, step + 1)
    }

    loop(globalTree.subTrees, 1)

  }

  /**
   * Generates the tree data structure of input data.
   *
   * e.g.
   * numberOfCities: 9
   * routes: List(0, 1, 1, 1, 1, 0, 2, 2)
   *
   *           0
   *         /  \
   *        6   1
   *          / \ \  \
   *         2  3  4  5
   *        / \
   *       7  8
   *
   * @param numberOfCities city count of country
   * @param routes list of routes between cities
   * @return optional tuple that holds the rootId and big/complete tree
   */
  def generateTree(numberOfCities: Int, routes: List[String]): Option[(String, CityTree[String])] = {

    /**
     * Generate the complete tree
     * @param routedCities the map of node cities
     * @param map the map of current state of tree
     * @return a map of sub-trees. The longest one is the original/complete tree.
     */
    @tailrec
    def loop(routedCities: Map[String, List[String]], map: Map[String, CityTree[String]]): Map[String, CityTree[String]] = {

      /**
       * Starts with generating the deepest nodes.
       * e.g. Map(2 -> List(7, 8))
       */
      val nodesWithOnlyLeafs = routedCities.filter {
        case (id, connected) ⇒
          !map.keySet.contains(id) && connected.forall(map.keySet)
      }

      if (nodesWithOnlyLeafs.nonEmpty) {
        val nodes = nodesWithOnlyLeafs.map {
          case (id, connected) ⇒
            val leafs = connected.flatMap(i ⇒ map.get(i))
            id -> NodeCity(id, leafs)
        }
        loop(routedCities, map ++ nodes)
      } else {
        map
      }
    }

    /**
     * generate a map of connected cities
     * e.g. routedCities: Map(0 -> List(1, 6), 1 -> List(2, 3, 4, 5), 2 -> List(7, 8))
     */
    val routedCities = routes.zip((1 to routes.length).map(_.toString)).groupBy(_._1).mapValues(list ⇒ list.map(_._2))
    require(routedCities.exists(x ⇒ !x._2.contains(x._1)), "a city can not have route to itself")

    /**
     * start with adding leaf cities to the map
     * e.g. Leafs: 7, 8, 3, 4, 5, 6
     */
    val leafCities = (0 until numberOfCities).map(_.toString).filterNot(routedCities.keySet).map(id ⇒ id -> LeafCity(id))

    loop(routedCities, leafCities.toMap).find { case (id, tree) ⇒ tree.size == numberOfCities }

  }
}
