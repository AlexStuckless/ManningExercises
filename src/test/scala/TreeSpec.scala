import Tree._
import org.scalatest._
import Tree._

class TreeSpec extends FlatSpec with Matchers {

  "A tree that has size called on it" should "return the number of nodes in the tree" in {
    val testTree = Branch(Branch(Leaf(1), Leaf(4)), Branch(Branch(Leaf(12), Leaf(8)), Leaf(7)))
    Tree.size(testTree) should be(9)
  }

  "A tree that has maximum called on it" should "return the node with the highest value" in {
    val testTree = Branch(Branch(Leaf(1), Leaf(4)), Branch(Branch(Leaf(12), Leaf(8)), Leaf(7)))
    maximum(testTree) should be(12)
  }

  "A tree that has depth called on it" should "return the longest path from root to a leaf" in {
    val testTree = Branch(Branch(Leaf(1), Leaf(4)), Branch(Branch(Leaf(12), Leaf(8)), Leaf(7)))
    depth(testTree) should be(4)
  }

  "A tree that has map called on it" should "return a new tree witht each node having been transformed" in {
    val testTree = Branch(Branch(Leaf(1), Leaf(4)), Branch(Branch(Leaf(12), Leaf(8)), Leaf(7)))
    map(testTree)(x => x + 1) should be(Branch(Branch(Leaf(2), Leaf(5)), Branch(Branch(Leaf(13), Leaf(9)), Leaf(8))))
  }

  "A tree that has sizeFold called on it" should "return the number of nodes in the tree" in {
    val testTree = Branch(Branch(Leaf(1), Leaf(4)), Branch(Branch(Leaf(12), Leaf(8)), Leaf(7)))
    sizeFold(testTree) should be(9)
  }

  "A tree that has maximumFold called on it" should "return the node with the highest value" in {
    val testTree = Branch(Branch(Leaf(1), Leaf(4)), Branch(Branch(Leaf(12), Leaf(8)), Leaf(7)))
    maximumFold(testTree) should be(12)
  }

  "A tree that has depthFold called on it" should "return the longest path from root to a leaf" in {
    val testTree = Branch(Branch(Leaf(1), Leaf(4)), Branch(Branch(Leaf(12), Leaf(8)), Leaf(7)))
    depthFold(testTree) should be(4)
  }

  "A tree that has mapFold called on it" should "return a new tree witht each node having been transformed" in {
    val testTree = Branch(Branch(Leaf(1), Leaf(4)), Branch(Branch(Leaf(12), Leaf(8)), Leaf(7)))
    mapFold(testTree)(x => x + 1) should be(Branch(Branch(Leaf(2), Leaf(5)), Branch(Branch(Leaf(13), Leaf(9)), Leaf(8))))
  }
}
