package amo.geometry.problems

import amo.geometry.commands.Command
import amo.geometry.figures.{ Circle, LineLike, Point }

trait ProblemEnvironment[T] {
    val points: Set[Point[T]]
    val lines: Set[LineLike[T]]
    val circles: Set[Circle[T]]

    def addPoints(newPoints: Set[Point[T]]): ProblemEnvironment[T]
    def addLine(line: LineLike[T]): ProblemEnvironment[T]
    def addCircle(circle: Circle[T]): ProblemEnvironment[T]
}

case class SimpleProblemEnvironment[T](
    points: Set[Point[T]] = Set[Point[T]](),
    lines: Set[LineLike[T]] = Set[LineLike[T]](),
    circles: Set[Circle[T]] = Set[Circle[T]](),
) extends ProblemEnvironment[T] {
    def addPoints(newPoints: Set[Point[T]]): ProblemEnvironment[T] =
        copy(points = points ++ newPoints)

    def addLine(line: LineLike[T]): ProblemEnvironment[T] =
        copy(lines = lines + line)

    def addCircle(circle: Circle[T]): ProblemEnvironment[T] =
        copy(circles = circles + circle)
}
