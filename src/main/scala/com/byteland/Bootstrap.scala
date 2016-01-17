package com.byteland

//noinspection ScalaStyle
object Bootstrap extends App {

  val inputs = console()

  inputs.foreach {

    case (_, (numberOfCities, routes)) ⇒

      Byteland.generateTree(numberOfCities, routes) match {

        case None ⇒ println("Cannot generate the Byteland")
        case Some((rootCityId, tree)) ⇒

          println(s"Byteland: Root: $rootCityId, City: $tree")

          import scala.collection.mutable
          val globalBlacklist = mutable.Set.empty[mutable.Set[String]]

          val step = Byteland.findStepCount(tree, globalBlacklist)

          println(s"step count: $step")
      }
  }

  /**
   * Takes inputs from user
   * @return a map containing parameters - which are city count and routes - of each input.
   */
  def console(): Map[Int, (Int, List[String])] = {

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

