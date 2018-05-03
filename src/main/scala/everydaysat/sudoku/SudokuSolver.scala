package everydaysat.sudoku

import cafesat.api.FormulaBuilder.propVar
import cafesat.api.Formulas.{Formula, PropVar}
import cafesat.api.Solver.solveForSatisfiability

import SudokuExamples._

object SudokuSolver extends App {

  type VariableVector = IndexedSeq[PropVar]

  case class SudokuPuzzle(grid: IndexedSeq[IndexedSeq[Option[Int]]]) {
    val size: Int = grid.size
    require(size > 0)
    require(grid.forall(_.size == size))
    val sqrt: Int = Math.sqrt(size).toInt // No precision loss
    require(sqrt * sqrt == size)
    val values: IndexedSeq[Option[Int]] = grid.flatten
    require(values.flatten.forall(v => v >= 1 && v <= size))
  }

  object SudokuPuzzle {
    def apply(input: String): SudokuPuzzle = {
      val split = input.split("\n")
      SudokuPuzzle(split.map(_.map({
        case c if c.isDigit => Some(c.asDigit)
        case '?' => None
      })))
    }
  }

  def solve(puzzle: SudokuPuzzle): Option[IndexedSeq[IndexedSeq[Int]]] = {

    val variables: IndexedSeq[IndexedSeq[VariableVector]] =
      (0 until puzzle.size).map(y => (0 until puzzle.size).map(x => (0 until puzzle.size).map(i => propVar(s"($x,$y,$i)"))))

    def and(seq: Seq[Formula]): Formula = seq.reduce(_ && _)
    def or(seq: Seq[Formula]): Formula = seq.reduce(_ || _)

    val constraintVectorOneHot = for (vector <- variables.flatten)
      yield {
        val atLeast = for (variable <- vector) yield variable && and(vector.filter(_ != variable).map(!_))
        or(atLeast)
      }

    val constraintClues = for {
      y <- 0 until puzzle.size
      x <- 0 until puzzle.size
      value <- puzzle.grid(y)(x).toIterable
      i <- 0 until puzzle.size
      variable = variables(y)(x)(i)
    } yield if (i == value - 1) variable else !variable

    val constraintUniqueHorizontally = for {
      y <- 0 until puzzle.size
      x1 <- 0 until puzzle.size
      x2 <- 0 until puzzle.size
      if x1 != x2
      i <- 0 until puzzle.size
    } yield !variables(y)(x1)(i) || !variables(y)(x2)(i)

    val constraintUniqueVertically = for {
      x <- 0 until puzzle.size
      y1 <- 0 until puzzle.size
      y2 <- 0 until puzzle.size
      if y1 != y2
      i <- 0 until puzzle.size
    } yield !variables(y1)(x)(i) || !variables(y2)(x)(i)

    val constraintUniqueSector = for {
      y <- 0 until puzzle.size by puzzle.sqrt
      x <- 0 until puzzle.size by puzzle.sqrt
      y1 <- 0 until puzzle.sqrt
      x1 <- 0 until puzzle.sqrt
      y2 <- 0 until puzzle.sqrt
      x2 <- 0 until puzzle.sqrt
      if x1 != x2 || y1 != y2
      i <- 0 until puzzle.size
    } yield !variables(y + y1)(x + x1)(i) || !variables(y + y2)(x + x2)(i)

    val allConstraints = constraintVectorOneHot ++ constraintClues ++ constraintUniqueHorizontally ++ constraintUniqueVertically ++ constraintUniqueSector

    solveForSatisfiability(and(allConstraints)).map(solution => variables.map(_.map(_.map(v => solution(v)).indexWhere(identity) + 1)))
  }

  // Actual solving
  // You may want to increase the stack size of your JVM

  val puzzle = SudokuPuzzle(example1)

  val t0 = System.currentTimeMillis()
  solve(puzzle) match {
    case Some(solution) =>
      val t1 = System.currentTimeMillis()
      println(s"Found solution in ${t1 - t0} ms")
      println(solution.map(_.mkString).mkString("\n"))

    case None => println("Unsatisfiable problem!")
  }

}
