package amo.geometry.commands

import amo.algreal.Field.ConstructibleTrait
import amo.geometry.figures.{ Point, Line }
import amo.geometry.problems.ProblemEnvironment

case class GetLineCommand[T](
    val p1: Point[T],
    val p2: Point[T]
)(
    implicit constructible: ConstructibleTrait[T],
    ordering: Ordering[T]
) extends Command[T] {
    val costL = 1
    val costE = 1

    def run(problemEnvironment: ProblemEnvironment[T]): ProblemEnvironment[T] =
        problemEnvironment
            .addCommand(this)
            .addLine(Line(p1, p2))
            .addCost(costL, costE)
}

case class GetLineCommandProvider[T]()(
    implicit constructible: ConstructibleTrait[T],
    ordering: Ordering[T]
) extends CommandProvider[T] {
    def provideCommands(problemEnvironment: ProblemEnvironment[T]): Iterator[Command[T]] =
        problemEnvironment.points.toVector.combinations(2).map {
            case Seq(p1, p2) => GetLineCommand(p1, p2)
        }
}

/*
 * 最終的な出力は？
 *
 * val problem = ...                                     // 初期値、終了条件、利用可能なコマンドリスト等を保持
 * val problemEnvironment = problem.initialEnvironment   // 問題の現在のステータス（保持する Line の数やコスト等）
 * val solver = ...                                      // ソルバー
 *
 * val processesE = solver.solveE(problem, problemEnvironment) // 手順 (E)
 * val processesL = solver.solveL(problem, problemEnvironment) // 手順 (L)
 *
 *
 * solve の中身
 *
 * def solve(problem, queue): Vector[Command] = if (queue.hasNext) Vector.empty else {
 *     val (prevProblemEnvironment, command) = queue.next
 *     val problemEnvironment = command.run(prevProblemEnvironment)
 *     if (problem.isSolved(problemEnvironment)) problemEnvironment.commands
 *     else if (queue.hasNext)
 *     else {
 *         val nextQueue = queue ++ if (
 *             problem.isOverCost(problemEnvironment)
 *         ) Iterator.empty else for {
 *             commandProvider <- problem.commandProviders
 *             command <- commandProvide.provide(problemEnvironment)
 *         } yield (problemEnvironment, command)
 *         solve(problem, queue)
 *     }
 * }
 *
 */






