package example

import org.scalacheck.Properties
import org.scalacheck.Arbitrary
import org.scalacheck.Gen
import org.scalacheck.Prop.forAll

import cats._
import cats.data._
import cats.implicits._

object DSpecification extends Properties("D.f") {

  def arbitraryLeafCell[A : Arbitrary] = for {
      value <- Arbitrary.arbitrary[A](implicitly[Arbitrary[A]])
    } yield LeafCell(value)

  def arbitraryDivisionCell[A : Arbitrary](direction : SubdivisionDirection, maxDepth : Int) =
      {
        implicit def subcellArbitrary[A : Arbitrary] : Arbitrary[Cell[A]] = cellArbitraryInst(maxDepth)
        for {
          subcells <- Arbitrary.arbitrary[List[Cell[A]]]
        } yield DivisionCell(subcells, direction)
      }

  def cellArbitraryInst[A : Arbitrary](maxDepth : Int) : Arbitrary[Cell[A]] = Arbitrary(
    if (maxDepth < 2) {
      arbitraryLeafCell(implicitly[Arbitrary[A]])
    } else {
      for {
        n <- Gen.posNum[Int]
        x <- n.abs % 3 match {
          case 0 => arbitraryDivisionCell(SubdivisionDirection.Vertical, maxDepth - 1)(implicitly[Arbitrary[A]])
          case 1 => arbitraryDivisionCell(SubdivisionDirection.Horizontal, maxDepth - 1)(implicitly[Arbitrary[A]])
          case 2 => arbitraryLeafCell(implicitly[Arbitrary[A]])
        }
      } yield x
    }
  )

  implicit val cellArbitrary: Arbitrary[Cell[Int]] = cellArbitraryInst(4)

  property("id") = forAll { (a: Cell[Int]) =>
    Cell.f[Id, Int, Int](identity)(a) == a
  }

  property("some") = forAll { (a: Cell[Int]) =>
    Cell.f[Option, Int, Int](Some(_))(a) == Some(a)
  }

}
