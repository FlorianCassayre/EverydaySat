package everydaysat.kemaru

import cafesat.api.Formulas._
import cafesat.api.FormulaBuilder._
import cafesat.api.Solver.solveForSatisfiability

import KemaruExamples._

object KemaruSolver extends App {

  type KemaruValue = Int
  type VariableVector = Seq[PropVar]

  case class KemaruSquare(x: Int, y: Int) {
    require(x >= 0 && y >= 0)
  }

  case class KemaruPuzzle(width: Int, height: Int, areas: Set[Set[KemaruSquare]], known: Map[KemaruSquare, KemaruValue]) {
    require(width > 0 && height > 0)

    val squares: Set[KemaruSquare] = areas.flatten

    require(squares.size == width * height)
    require(squares.forall(sq => sq.x < width && sq.y < height))
    require(known.keys.forall(sq => sq.x < width && sq.y < height))
  }

  object KemaruPuzzle {
    def apply(input: (String, String)): KemaruPuzzle = {
      val (mask, known) = input
      val (splitMask, splitKnown) = (mask.split("\n"), known.split("\n"))
      val (width, height) = (splitMask(0).length, splitMask.length)
      val squares: Seq[(Char, KemaruSquare)] = for {
        y <- 0 until height
        x <- 0 until width
        c = splitMask(y)(x)
      } yield (c, KemaruSquare(x, y))
      val areas = squares.groupBy(_._1).values.map(_.map(_._2).toSet)
      val knownValues = for {
        y <- 0 until height
        x <- 0 until width
        c = splitKnown(y)(x)
        if c.isDigit
      } yield KemaruSquare(x, y) -> c.asDigit

      KemaruPuzzle(width, height, areas.toSet, knownValues.toMap)
    }
  }


  def solve(puzzle: KemaruPuzzle): Option[Map[KemaruSquare, KemaruValue]] = {

    val variablesCount = puzzle.areas.map(_.size).max

    val variables: Map[KemaruSquare, VariableVector] =
      puzzle.squares.map(sq => sq -> (0 until variablesCount).map(i => propVar(s"(${sq.x},${sq.y},$i)"))).toMap

    def vectorEqual(a: VariableVector, b: VariableVector): Formula =
      (a zip b).foldLeft(True: Formula) { case (acc, (x, y)) => acc && (x iff y) }
    def and(seq: Seq[Formula]): Formula = seq.reduce(_ && _)
    def or(seq: Seq[Formula]): Formula = seq.reduce(_ || _)

    val constraintVectorOneHot = for (square <- puzzle.squares.toSeq)
      yield {
        val vector = variables(square)
        val atLeast = for (variable <- vector) yield variable && vector.filter(_ != variable).map(!_).reduce(_ && _)
        or(atLeast)
      }

    val constraintSquareUniqueness = for {
      area <- puzzle.areas.toSeq
      square <- area.toSeq
      squareVar = variables(square)
    } yield and((area - square).toSeq.map(e => !vectorEqual(squareVar, variables(e))))

    val constraintKnown = for {
      (square, value) <- puzzle.known.toSeq
      vector = variables(square)
      one = vector(value - 1)
    } yield one && and(vector.filter(_ != one).map(!_))

    val constraintRange = for {
      area <- puzzle.areas.toSeq
      square <- area.toSeq
      vector = variables(square)
      i <- area.size until variablesCount
    } yield !vector(i)

    val constraintAdjacency = for {
      area <- puzzle.areas.toSeq
      square <- area
      squareVector = variables(square)
      y <- -1 to 1
      x <- -1 to 1
      rx = square.x + x
      ry = square.y + y
      if x != 0 || y != 0
      if rx >= 0 && rx < puzzle.width && ry >= 0 && ry < puzzle.height
      adjSquare = KemaruSquare(rx, ry)
      if !area.contains(adjSquare)
      adjVector = variables(adjSquare)
    } yield !vectorEqual(squareVector, adjVector)

    val allConstraints = constraintVectorOneHot ++ constraintSquareUniqueness ++ constraintKnown ++ constraintRange ++ constraintAdjacency

    solveForSatisfiability(and(allConstraints)).map(solution =>
      variables.map { case (square, vector) => square -> (vector.indexWhere(v => solution(v)) + 1) })
  }

  // Actual solving

  val puzzle = KemaruPuzzle(example1)

  val t0 = System.currentTimeMillis()
  solve(puzzle) match {
    case Some(solution) =>
      val t1 = System.currentTimeMillis()
      println(s"Found solution in ${t1 - t0} ms")
      for {
        y <- 0 until puzzle.height
        x <- 0 until puzzle.width
      } yield {
        val square = solution.keys.find(_ == KemaruSquare(x, y))
        print(solution(square.get))

        if (x == puzzle.width - 1)
          println()
      }

    case None => println("Unsatisfiable problem!")
  }
}
