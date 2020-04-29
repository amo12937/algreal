package amo.geometry.problems

import amo.geometry.commands.CommandProvider

trait Problem[T] {
    val initialProblemEnvironment: ProblemEnvironment[T]
    val commandProviders: Vector[CommandProvider[T]]
    val answer: ProblemAnswer[T]

    def isSolved(problemEnvironment: ProblemEnvironment[T]): Boolean =
        !isOverCost(problemEnvironment) && answer.fulfill(problemEnvironment)
    def isOverCost(problemEnvironment: ProblemEnvironment[T]): Boolean
    def availableCommandProviders: Iterator[CommandProvider[T]] =
        commandProviders.toIterator
}

case class ProblemE[T](
    initialProblemEnvironment: ProblemEnvironment[T],
    commandProviders: Vector[CommandProvider[T]],
    answer: ProblemAnswer[T]
) extends Problem[T] {
    def isOverCost(problemEnvironment: ProblemEnvironment[T]): Boolean =
        problemEnvironment.costE <= answer.costE
}

case class ProblemL[T](
    initialProblemEnvironment: ProblemEnvironment[T],
    commandProviders: Vector[CommandProvider[T]],
    answer: ProblemAnswer[T]
) extends Problem[T] {
    def isOverCost(problemEnvironment: ProblemEnvironment[T]): Boolean =
        problemEnvironment.costL <= answer.costL
}

