package com.byteland

object TestFixtures {

  val treeSize = 9

  val routes = List("0", "1", "1", "1", "1", "0", "2", "2")

  val treeStepCount = 5

  import scala.collection.mutable
  val globalBlacklist = mutable.Set.empty[mutable.Set[String]]

  val treeId = "0"
  val tree = NodeCity("0",
    List(
      NodeCity("1",
        List(
          NodeCity("2",
            List(LeafCity("7"), LeafCity("8"))
          ),
          LeafCity("3"), LeafCity("4"), LeafCity("5")
        )
      ),
      LeafCity("6")
    )
  )

  val levelOrder = Seq("0", "1", "6", "2", "3", "4", "5", "7", "8")
  val postOrder = Seq("7", "8", "2", "3", "4", "5", "1", "6", "0")
  val preOrder = Seq("0", "1", "2", "7", "8", "3", "4", "5", "6")

  val subtrees = List(
    NodeCity("2", List(LeafCity("7"), LeafCity("8"))),
    NodeCity("1", List(NodeCity("2", List(LeafCity("7"), LeafCity("8"))), LeafCity("3"), LeafCity("4"), LeafCity("5"))),
    NodeCity("0", List(NodeCity("1", List(NodeCity("2", List(LeafCity("7"), LeafCity("8"))),
      LeafCity("3"), LeafCity("4"), LeafCity("5"))), LeafCity("6")))
  )

  val rootConnected = List(NodeCity("1", List(NodeCity("2", List(LeafCity("7"), LeafCity("8"))),
    LeafCity("3"), LeafCity("4"), LeafCity("5"))), LeafCity("6"))

  val leafId = 0
  val leaf = LeafCity(0)

}
