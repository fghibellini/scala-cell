package example

import org.scalacheck.Properties
import org.scalacheck.Arbitrary
import org.scalacheck.Gen
import org.scalacheck.Prop.forAll

import cats._
import cats.data._
import cats.implicits._

object DSpecification extends Properties("D.f") {

  def cellArbitrary[A : Arbitrary](depth : Int) : Arbitrary[Cell[A]] = Arbitrary(if (depth < 2) {
      for {
        value <- Arbitrary.arbitrary[A]
      } yield LeafCell(value)
   } else {
     for {
      n <- Gen.posNum[Int]
      x <- n.abs % 3 match {
        case 0 => {
          implicit def subcellArbitrary[A : Arbitrary] : Arbitrary[Cell[A]] = cellArbitrary(depth - 1)
          for {
            subcells <- Arbitrary.arbitrary[List[Cell[A]]]
          } yield DivisionCell(subcells, SubdivisionDirection.Vertical)
        }
        case 1 => {
          implicit def subcellArbitrary[A : Arbitrary] : Arbitrary[Cell[A]] = cellArbitrary(depth - 1)
          for {
            subcells <- Arbitrary.arbitrary[List[Cell[A]]]
          } yield DivisionCell(subcells, SubdivisionDirection.Horizontal)
        }
        case 2 => {
          for {
            value <- Arbitrary.arbitrary[A]
          } yield LeafCell(value)
        }
      }
    } yield x
  })

  implicit val cellArbitrary: Arbitrary[Cell[Int]] = cellArbitrary(4)

  property("id") = forAll { (a: Cell[Int]) =>
    implicit val cellShow : Show[Cell[Int]] = Cell.showCell
    println(a.show)
    a.f[Id, Int](identity) == a
  }

  property("some") = forAll { (a: Cell[Int]) =>
    a.f[Option, Int](Some(_)) == Some(a)
  }

}
