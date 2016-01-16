package com.byteland

import scala.collection.mutable

//noinspection ScalaStyle
object Main extends App {

  val inputs = console()

  inputs.foreach {

    case (_, (numberOfCities, routes)) ⇒

      val (_, tree) = generateTree(numberOfCities, routes).get

      val globalBlacklist = mutable.Map.empty[String, Set[String]].withDefaultValue(Set.empty[String])

      println(findStepCount(tree, globalBlacklist))

  }

  def findStepCount(globalTree: CityTree[String], globalBlacklist: scala.collection.mutable.Map[String, Set[String]]): Int = {

    def filterBlackListed(tree: CityTree[String], localBlacklist: mutable.Map[String, Set[String]]): List[CityTree[String]] = {
      tree.getConnected.filterNot(c => localBlacklist.values.toSet.flatten.contains(c.getId))
    }

    def removeItem(tree: CityTree[String], t: CityTree[String]): List[CityTree[String]] = {
      tree.getConnected.filterNot(c => c.getId == t.getId)
    }

    def unifiedId(a: CityTree[String], b: CityTree[String]): String = s"${a.getId}-${b.getId}"

    def updateBlacklist(tree: CityTree[String], b: CityTree[String],
                        globalBlacklist: mutable.Map[String, Set[String]],
                        localBlacklist: mutable.Map[String, Set[String]]): Unit = {
      val treeIds = tree.getId.split("-")
      val itemIds = b.getId.split("-")


      /*println(s"treeIds: ${treeIds.toList}")
      println(s"itemIds: ${itemIds.toList}")
      println(s"globalBlacklist: $globalBlacklist")
      println(s"localBlacklist: $localBlacklist")*/

      globalBlacklist(treeIds.head) = globalBlacklist.getOrElse(treeIds.head,  treeIds.toSet) ++ globalBlacklist.getOrElse(itemIds.head, itemIds.toSet)
      localBlacklist(treeIds.head) = globalBlacklist(treeIds.head)
    }

    def run(tree: CityTree[String], globalBlackList: mutable.Map[String, Set[String]],
            localBlackList: mutable.Map[String, Set[String]]): CityTree[String] = {

      val filtered = filterBlackListed(tree, localBlackList)

      /*println(s"tree: $tree")
      println(s"blacklist: $globalBlackList")
      println(s"filtered: $filtered")*/

      val result = filtered.headOption match {
        case None => tree
        case Some(t) => t match {
          case a: LeafCity[String] =>
            updateBlacklist(tree, a, globalBlackList, localBlackList)
            removeItem(tree, a) match {
              case Nil => LeafCity(unifiedId(tree, a))
              case cs => NodeCity(unifiedId(tree, a), cs)
            }
          case b: NodeCity[String] =>
            updateBlacklist(tree, b, globalBlackList, localBlackList)
            NodeCity(unifiedId(tree, b), removeItem(tree, b) ::: b.getConnected)
        }
      }

      println(s"result: $result")
      println()
      result

    }

    def updateGlobalBlacklist(): Unit = {
      println("updating global black list")
      globalBlacklist.foreach { case (k, v) =>
        v.filterNot(_.equals(k)) foreach { case v2 =>
          globalBlacklist(k) ++= globalBlacklist(v2)
        }
      }
      globalBlacklist.foreach(println)
    }

    def checkComplete(cityCount: Int): Boolean = {
      updateGlobalBlacklist()
      globalBlacklist.exists { case (k, v) =>
        globalBlacklist.filterKeys(!_.equals(k)).exists { case (k2, v2) =>
          (v ++ v2).size == cityCount
        }
      }
    }

    def loop(subtrees: List[CityTree[String]], cityCount: Int, step: Int): Int = {
      val localBlacklist = mutable.Map.empty[String, Set[String]].withDefaultValue(Set.empty[String])
      val res = subtrees.map(run(_, globalBlacklist, localBlacklist))
      if (checkComplete(cityCount)) step + 1 else loop(res, cityCount, step + 1)
    }

    val cityCount = globalTree.size
    loop(globalTree.subTrees, cityCount, 1)

  }

  def generateTree(numberOfCities: Int, routes: List[String]): Option[(String, CityTree[String])] = {

    val map = scala.collection.mutable.Map.empty[String, CityTree[String]]

    val routedCities = routes.zip((1 to routes.length).map(_.toString)).groupBy(_._1).mapValues(list ⇒ list.map(_._2))
    require(routedCities.exists(x => !x._2.contains(x._1)), "a city can not have route to itself")
    //Map(0 -> List(1, 6), 1 -> List(2, 3, 4, 5), 2 -> List(7, 8))

    val nodeCityIds = routedCities.keySet
    val leafCityIds = (0 until numberOfCities).map(_.toString).filterNot(nodeCityIds)

    leafCityIds.foreach(id ⇒ map += id -> LeafCity(id))

    while (map.keySet.size != numberOfCities) {

      val nodesWithOnlyLeafs = routedCities.filter { case (id, connected) ⇒ connected.forall(map.keySet) }
      //nodesWithOnlyLeafs: Map(2 -> List(7, 8))

      nodesWithOnlyLeafs.foreach {
        case (id, connected) ⇒
          val leafs = connected.flatMap(i ⇒ map.get(i))
          map += id -> NodeCity(id, leafs)
      }
    }

    map.find { case (id, tree) ⇒ tree.size == numberOfCities }
  }

  def console(): Map[Int, (Int, List[String])] = {

    Console.println("--- Welcome to the Byteland Unifier Code ---")
    Console.println("Please enter the number of test cases:")

    val numberOfTestCases = scala.io.StdIn.readInt()
    require(numberOfTestCases < 1000, "number of test cases should be smaller than 1000")

    (0 until numberOfTestCases).map { i ⇒

      Console.println("Please enter the number of cities:")

      val numberOfCities = scala.io.StdIn.readInt()
      require(2 <= numberOfCities && numberOfCities <= 600, "number of cities should be between [2, 600]")

      Console.println(s"Please enter the route sequence with length ${numberOfCities - 1} e.g. ${"\n"} 0 1 2")

      val routes = scala.io.StdIn.readLine().split(' ').toList
      require(routes.size == numberOfCities - 1, "number of routes should be 1 less than number of cities")
      require(routes.forall((0 until numberOfCities).map(_.toString).contains), "routes should be between the cities")

      i -> (numberOfCities, routes)

    }.toMap
  }

}

