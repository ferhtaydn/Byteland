package com.byteland

import org.scalatest.{ Matchers, WordSpecLike }
import TestFixtures._

class BytelandTest extends WordSpecLike with Matchers {

  "Byteland" when {

    "called generateTree on tree" should {
      val (rootId, generatedTree) = Byteland.generateTree(treeSize, routes).get
      "return the tree" in {
        generatedTree.size shouldBe treeSize
        rootId shouldBe treeId
      }
    }

    "called findStepCount on tree" should {
      val result = Byteland.findStepCount(tree, globalBlacklist)
      "return step count" in {
        result shouldBe treeStepCount
      }
    }

  }
}
