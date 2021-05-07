package example

import cats._
import cats.data._
import cats.implicits._

sealed abstract class SubdivisionDirection
object SubdivisionDirection {
  final case object Horizontal extends SubdivisionDirection
  final case object Vertical extends SubdivisionDirection
}

// TODO can the root be a LeafCell ?
sealed abstract class Cell[A] {
  def f[M[_]: Monad, B](fn : A => M[B]) : M[Cell[B]]
}

case class DivisionCell[A](xs: List[Cell[A]], direction: SubdivisionDirection) extends Cell[A] {
  def f[M[_]: Monad, B](fn : A => M[B]) : M[Cell[B]] =
    xs.traverse(x => x.f(fn)).map(xs_ => DivisionCell(xs_, direction))
}

case class LeafCell[A](value: A) extends Cell[A] {
  def f[M[_]: Monad, B](fn : A => M[B]) : M[Cell[B]] =
    fn(value).map(x => LeafCell(x))
}

object Hello extends App {
  println("hey!")
}
