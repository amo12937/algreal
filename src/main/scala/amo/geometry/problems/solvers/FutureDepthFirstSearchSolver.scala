package amo.geometry.problems.solvers

import cats._
import scala.collection.concurrent.TrieMap
import scala.collection.immutable.Iterable
import scala.concurrent.{ ExecutionContext, Future }

import amo.geometry.commands.Command
import amo.geometry.problems.{ Board, Cost, Problem, ProblemEnvironment }

class FutureDepthFirstSearchSolver[T](
    implicit ec: ExecutionContext
) extends Solver[T, Future]{
    def solve(problem: Problem[T]): Future[Vector[Command[T]]] = {
        val env = problem.initialEnvironment
        val commands = nextCommands(problem, env)
        concurrentSearch(problem, commands, new TrieMap())
    }

    def concurrentSearch(
        problem: Problem[T],
        commands: Iterator[(Command[T], Cost, ProblemEnvironment[T])],
        boardHistory: TrieMap[Board[T], Unit]
    ): Future[Vector[Command[T]]] = {
        val itr = Iterable(commands.map(Future(_).flatMap({
            case (command, cost, prevEnv) => {
                val env = prevEnv.applyCommand(command, cost)
                if (problem.isSolved(env)) Future(env.commands)
                else {
                    val board = env.board
                    val nextTasks =
                        if (problem.isOverCost(env.cost)) Iterator.empty
                        else if (boardHistory.isDefinedAt(board)) Iterator.empty
                        else if (!problem.canReachAnswer(env)) Iterator.empty
                        else nextCommands(problem, env)
                    boardHistory += ((board, ()))
                    concurrentSearch(problem, nextTasks, boardHistory)
                }
            }
        })).toVector: _*)

        Future.find(itr)(commands => commands.length > 0)
            .map(_.getOrElse(Vector.empty))
    }

    def nextCommands(
        problem: Problem[T],
        problemEnvironment: ProblemEnvironment[T]
    ): Iterator[(Command[T], Cost, ProblemEnvironment[T])] =
        for {
            provider <- problem.availableCommandProviders
                if (!problem.isOverCost(problemEnvironment.cost + provider.cost))
            command <- provider.provideCommands(problemEnvironment.board)
        } yield (command, provider.cost, problemEnvironment)
}
