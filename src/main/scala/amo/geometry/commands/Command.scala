package amo.geometry.commands

import amo.geometry.problems.ProblemEnvironment

trait Command[T] {
    def run(problemEnvironment: ProblemEnvironment[T]): ProblemEnvironment[T]
}
