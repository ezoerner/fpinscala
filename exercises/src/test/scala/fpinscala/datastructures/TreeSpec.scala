package fpinscala.datastructures

import org.scalacheck.Arbitrary._
import org.scalacheck.Gen._
import org.scalacheck._
import org.scalactic.TypeCheckedTripleEquals._
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{Matchers, PropSpec}

class TreeSpec extends PropSpec with GeneratorDrivenPropertyChecks with Matchers {

  val genLeafWithNumNodes: Gen[(Leaf[Int], Int)] = arbitrary[Int] map (v => (Leaf(v), 1))

  def genBranchWithNumNodes(level: Int, numNodes: Int): Gen[(Tree[Int], Int)] =
    for {
      left <- genTreeWithNumNodes(level, numNodes)
      right <- genTreeWithNumNodes(level, numNodes)
    } yield (Branch(left._1, right._1), left._2 + right._2 + 1)

  def genTreeWithNumNodes(level: Int, numNodes: Int): Gen[(Tree[Int], Int)] = {
    if (level >= 100)
      genLeafWithNumNodes
    else
      Gen.oneOf(genLeafWithNumNodes, lzy(genBranchWithNumNodes(level + 1, numNodes)))
  }

  val treesWithNumNodes: Gen[(Tree[Int], Int)] = genTreeWithNumNodes(0, 0)

  property("example trees have size equal to number of nodes") {
    forAll(treesWithNumNodes) {
      case (tree, numNodes) => Tree.size(tree) should ===(numNodes)
    }
  }
}
