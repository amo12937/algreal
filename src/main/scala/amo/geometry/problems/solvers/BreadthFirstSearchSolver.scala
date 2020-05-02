package amo.geometry.problems.solvers

import scala.annotation.tailrec
import cats._

import amo.geometry.commands.Command
import amo.geometry.problems.{ Board, Cost, Problem, ProblemEnvironment }

class BreadthFirstSearchSolver[T] extends Solver[T, Id]{
    def solve(problem: Problem[T]): Vector[Command[T]] = {
        val env = problem.initialEnvironment
        val queue = nextCommands(problem, env)
        recursiveSolve(problem, queue, Set())
    }

    @tailrec
    final def recursiveSolve(
        problem: Problem[T],
        queue: Iterator[(Command[T], Cost, ProblemEnvironment[T])],
        prevBoardHistory: Set[Board[T]]
    ): Vector[Command[T]] = if (!queue.hasNext) Vector.empty else {
        val (command, cost, prevProblemEnvironment) = queue.next
        val problemEnvironment = prevProblemEnvironment.applyCommand(command, cost)
        if (problem.isSolved(problemEnvironment)) problemEnvironment.commands
        else {
            val board = problemEnvironment.board
            val (nextQueue, boardHistory) =
                if (prevBoardHistory.contains(board))
                    (Iterator.empty, prevBoardHistory)
                else if (problem.isOverCost(problemEnvironment.cost))
                    (Iterator.empty, prevBoardHistory + board)
                else (
                    nextCommands(problem, problemEnvironment),
                    prevBoardHistory + board
                )
            recursiveSolve(problem, queue ++ nextQueue, boardHistory)
        }
    }

    def nextCommands(
        problem: Problem[T],
        problemEnvironment: ProblemEnvironment[T]
    ): Iterator[(Command[T], Cost, ProblemEnvironment[T])] = {
        val x = (for {
            provider <- problem.availableCommandProviders
                if (!problem.isOverCost(problemEnvironment.cost + provider.cost))
            command <- provider.provideCommands(problemEnvironment.board)
        } yield (command, provider.cost, problemEnvironment))
            .toVector
            .groupBy(_._1)
            .mapValues(_.minBy(_._2.costE))
            .values
        println(x.size)
        x.toIterator
    }
}
