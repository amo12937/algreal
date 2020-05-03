package amo.geometry.commands

import amo.algreal.field.ConstructibleTrait
import amo.geometry.figures.{ Circle, Point }
import amo.geometry.problems.{ Board, Cost }

class CircleCommand[T](p1: Point[T], p2: Point[T])(
    implicit constructible: ConstructibleTrait[T],
    ordering: Ordering[T]
) extends CircleCommandBase[T] {
    val fig = Circle(p1, p1.dist(p2))
    override def toString = s"Draw a circle passing through ${p2} with the center ${p1}"
}

case class GetCircleCommandProvider[T]()(
    implicit constructible: ConstructibleTrait[T],
    ordering: Ordering[T]
) extends CommandProvider[T] {
    val cost = Cost(1, 1)

    def provideCommands(board: Board[T]): Iterator[Command[T]] =
        board.points.toVector.combinations(2).flatMap {
            case Seq(p1, p2) => {
                val r = p1.dist(p2)
                Iterator(
                    new CircleCommand(p1, p2),
                    new CircleCommand(p2, p1),
                )
            }
        }
}
