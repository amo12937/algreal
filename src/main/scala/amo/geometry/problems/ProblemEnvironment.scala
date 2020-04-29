package amo.geometry.problems

import amo.geometry.commands.Command
import amo.geometry.figures.{ Circle, LineLike, Point }

trait ProblemEnvironment[T] {
    val costL: Int
    val costE: Int

    val points: Set[Point[T]]
    val lines: Set[LineLike[T]]
    val circles: Set[Circle[T]]
    def commands: Vector[Command[T]]

    def addCommand(command: Command[T]): ProblemEnvironment[T]
    def addLine(_costL: Int, _costR: Int, line: LineLike[T]): ProblemEnvironment[T]

    // def addCircle
    // def addPoints
}
