package com.byteland

import org.scalatest.{ Matchers, WordSpecLike }
import TestFixtures._

class CityTreeTest extends WordSpecLike with Matchers {

  "CityTree" when {

    "called toSeqPreOrder on LeafCity" should {
      val result = leaf.toSeqPreOrder
      "return LeafCity id" in {
        result shouldBe Seq(0)
        result.size shouldBe 1
      }
    }

    "called toSeqPreOrder on NodeCity" should {
      val result = tree.toSeqPreOrder
      "return " in {
        result shouldBe preOrder
        result.size shouldBe treeSize
      }
    }

    "called toSeqPostOrder on LeafCity" should {
      val result = leaf.toSeqPostOrder
      "return LeafCity id" in {
        result shouldBe Seq(0)
        result.size shouldBe 1
      }
    }

    "called toSeqPostOrder on NodeCity" should {
      val result = tree.toSeqPostOrder
      "return " in {
        result shouldBe postOrder
        result.size shouldBe treeSize
      }
    }

    "called toSeqLevelOrder on LeafCity" should {
      val result = leaf.toSeqLevelOrder
      "return LeafCity id" in {
        result shouldBe Seq(0)
        result.size shouldBe 1
      }
    }

    "called toSeqLevelOrder on NodeCity" should {
      val result = tree.toSeqLevelOrder
      "return " in {
        result shouldBe levelOrder
        result.size shouldBe treeSize
      }
    }

    "called isLeaf on LeafCity" should {
      val result = leaf.isLeaf
      "return true" in {
        result shouldBe true
      }
    }

    "called isLeaf on NodeCity" should {
      val result = tree.isLeaf
      "return false" in {
        result shouldBe false
      }
    }

    "called getId on NodeCity" should {
      val result = tree.getId
      "return id of city" in {
        result shouldBe treeId
      }
    }

    "called getId on LeafCity" should {
      val result = leaf.getId
      "return id of city" in {
        result shouldBe leafId
      }
    }

    "called size on LeafCity" should {
      val result = leaf.size
      "return the size as 1" in {
        result shouldBe 1
      }
    }

    "called size on NodeCity" should {
      val result = tree.size
      "return size of tree" in {
        result shouldBe treeSize
      }
    }

    "called subTrees on LeafCity" should {
      val result = leaf.subTrees
      "return empty list" in {
        result shouldBe Nil
      }
    }

    "called subTrees on NodeCity" should {
      val result = tree.subTrees
      "return sub-trees of the original tree" in {
        result shouldBe subtrees
      }
    }

    "called getConnected on LeafCity" should {
      val result = leaf.getConnected
      "return empty list" in {
        result shouldBe Nil
      }
    }

    "called getConnected on NodeCity" should {
      val result = tree.getConnected
      "return connected trees of the root" in {
        result shouldBe rootConnected
      }
    }
  }
}
