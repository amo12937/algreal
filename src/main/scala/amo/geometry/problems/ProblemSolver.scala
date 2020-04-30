package amo.geometry.problems

import scala.annotation.tailrec

import amo.geometry.commands.Command

class ProblemSolver[T] {
    def solve(problem: Problem[T]): Vector[Command[T]] = {
        val env = problem.initialProblemEnvironment
        val queue = nextCommands(problem, env).map[
            (Command[T], ProblemEnvironment[T], Vector[Command[T]], Cost)
        ]((_, env, Vector(), Cost(0, 0)))
        recursiveSolve(problem, queue)
    }

    @tailrec
    final def recursiveSolve(
        problem: Problem[T],
        queue: Iterator[(Command[T], ProblemEnvironment[T], Vector[Command[T]], Cost)]
    ): Vector[Command[T]] = if (!queue.hasNext) Vector.empty else {
        val (command, prevProblemEnvironment, prevCommands, prevCost) = queue.next
        val problemEnvironment = command.run(prevProblemEnvironment)
        val commands = prevCommands :+ command
        val cost = prevCost + command.cost
        if (problem.isSolved(cost, problemEnvironment)) commands
        else {
            val nextQueue =
                if (problem.isOverCost(cost)) Iterator.empty
                else nextCommands(problem, problemEnvironment).map[
                    (Command[T], ProblemEnvironment[T], Vector[Command[T]], Cost)
                ]((_, problemEnvironment, commands, cost))
            recursiveSolve(problem, queue ++ nextQueue)
        }
    }

    def nextCommands(
        problem: Problem[T],
        problemEnvironment: ProblemEnvironment[T]
    ): Iterator[Command[T]] = for {
        commandProvider <- problem.availableCommandProviders
        command <- commandProvider.provideCommands(problemEnvironment)
    } yield command
}
