package com.byteland

//noinspection ScalaStyle
object Bootstrap extends App {

  val inputs = console()

  inputs.foreach {

    case (_, (numberOfCities, routes)) ⇒

      val tree = generateTree(numberOfCities, routes)

      println(tree)
      println(tree.get._2.toSeqPreOrder)
      println(tree.get._2.toSeqPostOrder)
      println(tree.get._2.toSeqLevelOrder)
      println(tree.get._2.height)
      println(tree.get._2.subTrees)

  }

  def generateTree(numberOfCities: Int, routes: List[Int]): Option[(Int, CityTree[Int])] = {

    val map = scala.collection.mutable.Map.empty[Int, CityTree[Int]]

    val routedCities = routes.zip(1 to routes.length).groupBy(_._1).mapValues(list ⇒ list.map(_._2))
    //Map(0 -> List(1, 6), 1 -> List(2, 3, 4, 5), 2 -> List(7, 8))

    val nodeCityIds = routedCities.keySet
    val leafCityIds = (0 until numberOfCities).filterNot(nodeCityIds)

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

  def console(): Map[Int, (Int, List[Int])] = {

    Console.println("--- Welcome to the Byteland Unifier Code ---")
    Console.println("Please enter the number of test cases:")

    val numberOfTestCases = scala.io.StdIn.readInt()
    require(numberOfTestCases < 1000)

    (0 until numberOfTestCases).map { i ⇒

      Console.println("Please enter the number of cities:")

      val numberOfCities = scala.io.StdIn.readInt()
      require(2 <= numberOfCities && numberOfCities <= 600)

      Console.println(s"Please enter the route sequence with length ${numberOfCities - 1} e.g. ${"\n"} 0 1 2")

      val routes = scala.io.StdIn.readLine().split(' ').map(_.toInt).toList
      require(routes.size == numberOfCities - 1)
      require(routes.forall((0 until numberOfCities).contains))

      i -> (numberOfCities, routes)

    }.toMap
  }

}

