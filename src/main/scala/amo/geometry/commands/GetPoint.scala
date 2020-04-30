package amo.geometry.commands

import amo.algreal.Field.ConstructibleTrait
import amo.geometry.figures.{ Circle, LineLike, Point }
import amo.geometry.problems.{ Cost, ProblemEnvironment }

import amo.implicits._

case class GetPointCommand[T](
    lines: Set[LineLike[T]],
    circles: Set[Circle[T]]
)(
    implicit constructible: ConstructibleTrait[T],
    ordering: Ordering[T]
) extends Command[T] {
    val cost = Cost(0, 0)

    def run(problemEnvironment: ProblemEnvironment[T]): ProblemEnvironment[T] = {
        val ps = lines.toVector.combinations(2).flatMap({
            case Seq(l1, l2) => l1.intersects(l2)
        }) ++ circles.toVector.combinations(2).flatMap({
            case Seq(c1, c2) => c1.intersects(c2)
        }) ++ (for {
            l <- lines
            c <- circles
            p <- l.intersects(c)
        } yield p)

        problemEnvironment.addPoints(ps.toSet)
    }
}

case class GetPointCommandProvider[T]()(
    implicit constructible: ConstructibleTrait[T],
    ordering: Ordering[T]
) extends CommandProvider[T] {
    def provideCommands(problemEnvironment: ProblemEnvironment[T]): Iterator[Command[T]] =
        Iterator(
            GetPointCommand(problemEnvironment.lines, problemEnvironment.circles)
        )
}
