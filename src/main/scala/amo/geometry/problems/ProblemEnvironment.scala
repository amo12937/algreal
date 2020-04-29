package amo.geometry.problems

import amo.geometry.commands.Command
import amo.geometry.figures.{ Circle, LineLike, Point }

trait ProblemEnvironment[T] {
    val costL: Int
    val costE: Int

    val points: Set[Point[T]]
    val lines: Set[LineLike[T]]
    val circles: Set[Circle[T]]
    val commands: Vector[Command[T]]

    def addCommand(command: Command[T]): ProblemEnvironment[T]
    def addPoints(newPoints: Set[Point[T]]): ProblemEnvironment[T]
    def addLine(line: LineLike[T]): ProblemEnvironment[T]
    def addCircle(circle: Circle[T]): ProblemEnvironment[T]
    def addCost(costL: Int, costE: Int): ProblemEnvironment[T]
}

case class SimpleProblemEnvironment[T](
    costL: Int = 0,
    costE: Int = 0,
    points: Set[Point[T]] = Set[Point[T]](),
    lines: Set[LineLike[T]] = Set[LineLike[T]](),
    circles: Set[Circle[T]] = Set[Circle[T]](),
    commands: Vector[Command[T]] = Vector()
) extends ProblemEnvironment[T] {
    def addCommand(command: Command[T]): ProblemEnvironment[T] =
        copy(commands = commands :+ command)

    def addPoints(newPoints: Set[Point[T]]): ProblemEnvironment[T] =
        copy(points = points ++ newPoints)

    def addLine(line: LineLike[T]): ProblemEnvironment[T] =
        copy(lines = lines + line)

    def addCircle(circle: Circle[T]): ProblemEnvironment[T] =
        copy(circles = circles + circle)

    def addCost(extraCostL: Int, extraCostE: Int): ProblemEnvironment[T] =
        copy(costL = costL + extraCostL, costE = costE + extraCostE)
}
