package amo.geometry.problems

import amo.geometry.commands.CommandProvider

trait Problem[T] {
    val initialProblemEnvironment: ProblemEnvironment[T]
    val commandProviders: Vector[CommandProvider[T]]
    val answer: ProblemAnswer[T]

    def isSolved(
        cost: Cost,
        problemEnvironment: ProblemEnvironment[T]
    ): Boolean =
        !isOverCost(cost) && answer.fulfill(problemEnvironment)
    def isOverCost(cost: Cost): Boolean
    def availableCommandProviders: Iterator[CommandProvider[T]] =
        commandProviders.toIterator
}

case class ProblemL[T](
    initialProblemEnvironment: ProblemEnvironment[T],
    commandProviders: Vector[CommandProvider[T]],
    answer: ProblemAnswer[T]
) extends Problem[T] {
    def isOverCost(cost: Cost): Boolean = answer.cost.costL < cost.costL
}

case class ProblemE[T](
    initialProblemEnvironment: ProblemEnvironment[T],
    commandProviders: Vector[CommandProvider[T]],
    answer: ProblemAnswer[T]
) extends Problem[T] {
    def isOverCost(cost: Cost): Boolean = answer.cost.costE < cost.costE
}
