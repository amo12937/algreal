package amo.geometry.problems

import amo.geometry.commands.CommandProvider

trait Problem[T] {
    val initialProblemEnvironment: ProblemEnvironment[T]

    def isSolved(problemEnvironment: ProblemEnvironment[T]): Boolean
    def isOverCost(problemEnvironment: ProblemEnvironment[T]): Boolean
    def availableCommandProviders: Iterator[CommandProvider[T]]
}
