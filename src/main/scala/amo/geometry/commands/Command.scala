package amo.geometry.commands

import amo.geometry.problems.{ Cost, ProblemEnvironment }

trait Command[T] {
    val cost: Cost
    def run(problemEnvironment: ProblemEnvironment[T]): ProblemEnvironment[T]
}
