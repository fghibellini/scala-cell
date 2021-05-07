package example

import cats._
import cats.data._
import cats.implicits._

sealed abstract class SubdivisionDirection
object SubdivisionDirection {
  final case object Horizontal extends SubdivisionDirection
  final case object Vertical extends SubdivisionDirection
}

// From the assignment it would sound that the root should not directly be a value
// but that would just introduce a lot of boilerplate code (we would need a second class)
// so I'll have a single type to represent nodes of the tree.

sealed abstract class Cell[A]
final case class DivisionCell[A](xs: List[Cell[A]], direction: SubdivisionDirection) extends Cell[A]
final case class LeafCell[A](value: A) extends Cell[A]

object Cell {

  def f[M[_]: Monad, A, B](fn : A => M[B])(d: Cell[A]) : M[Cell[B]] = d match {
    case DivisionCell(xs, direction) =>
      xs.traverse(x => f(fn)(x)).map(xs_ => DivisionCell(xs_, direction))
    case LeafCell(value) =>
      fn(value).map(x => LeafCell(x))
  }

  implicit def showCell[A : Show]: Show[Cell[A]] = Show.show(cell => cell match {
    case DivisionCell(xs, direction) => {
      val contents = Foldable[List].intercalate(xs.map(x => x.show), ",")
      val cellDir = if (direction == SubdivisionDirection.Horizontal) "h" else "v"
      s"${cellDir}(${contents})"
    }
    case LeafCell(value) => value.toString()
  })

}

object Hello extends App {
  println("hey!")
}
