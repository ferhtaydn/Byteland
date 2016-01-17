package com.byteland

import scala.annotation.tailrec
import scala.collection.mutable

//noinspection ScalaStyle
object Byteland {

  def findStepCount(globalTree: CityTree[String], globalBlacklist: mutable.Set[mutable.Set[String]]): Int = {

    def filterBlackListed(tree: CityTree[String], localBlacklist: Set[String]): List[CityTree[String]] = {
      val treeId = tree.getId.split("-").head
      tree.getConnected.filterNot(c ⇒ localBlacklist.contains(c.getId) || localBlacklist.contains(treeId)).filterNot { c ⇒
        globalBlacklist.exists(set ⇒ set.contains(treeId) && set.contains(c.getId))
      }

    }

    def removeItem(tree: CityTree[String], t: CityTree[String]): List[CityTree[String]] = {
      tree.getConnected.filterNot(c ⇒ c.getId == t.getId)
    }

    def unifiedId(a: CityTree[String], b: CityTree[String]): String = s"${a.getId}-${b.getId}"

    def updateBlacklist(tree: CityTree[String], b: CityTree[String],
      globalBlacklist: mutable.Set[mutable.Set[String]],
      localBlacklist: mutable.Set[String]): Unit = {

      val treeIds = tree.getId.split("-")
      val itemIds = b.getId.split("-")

      val itemSet = globalBlacklist.find(set ⇒ set.contains(itemIds.head)) match {
        case None      ⇒ mutable.Set(itemIds: _*)
        case Some(set) ⇒ globalBlacklist.remove(set); set
      }
      // WARN: A lot of mutation operation
      globalBlacklist.find(set ⇒ set.contains(treeIds.head)) match {
        case None      ⇒ globalBlacklist += (itemSet ++ treeIds)
        case Some(set) ⇒ globalBlacklist.remove(set); globalBlacklist += (set ++ itemSet)
      }

      //localBlacklist ++= treeIds.flatMap(globalBlacklist(_))
      localBlacklist ++= globalBlacklist.find(set ⇒ set.contains(itemIds.head)).getOrElse(mutable.Set.empty[String])

    }

    def run(tree: CityTree[String], globalBlacklist: mutable.Set[mutable.Set[String]],
      localBlackList: mutable.Set[String]): CityTree[String] = {

      val filtered = filterBlackListed(tree, localBlackList.toSet)

      val result = filtered.headOption match {
        case None ⇒ tree
        case Some(t) ⇒ t match {
          case a: LeafCity[String] ⇒
            updateBlacklist(tree, a, globalBlacklist, localBlackList)
            removeItem(tree, a) match {
              case Nil ⇒ LeafCity(unifiedId(tree, a))
              case cs  ⇒ NodeCity(unifiedId(tree, a), cs)
            }
          case b: NodeCity[String] ⇒
            updateBlacklist(tree, b, globalBlacklist, localBlackList)
            NodeCity(unifiedId(tree, b), removeItem(tree, b) ::: b.getConnected)
        }
      }

      result

    }

    def checkComplete: Boolean = {
      globalBlacklist.size == 1 && (globalBlacklist.head.size == globalTree.size)
    }

    def loop(subtrees: List[CityTree[String]], step: Int): Int = {
      val localBlacklist = mutable.Set.empty[String]
      val res = subtrees.map(run(_, globalBlacklist, localBlacklist)).filterNot(_.isLeaf)
      if (checkComplete) step else loop(res, step + 1)
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
