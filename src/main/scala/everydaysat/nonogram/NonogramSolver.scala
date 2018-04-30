package everydaysat.nonogram

import cafesat.api.Formulas._
import cafesat.api.FormulaBuilder._
import cafesat.api.Solver.solveForSatisfiability

import NonogramExamples._

object NonogramSolver extends App {

  case class NonogramPuzzle(horizontal: Seq[Seq[Int]], vertical: Seq[Seq[Int]]) {
    val (width: Int, height: Int) = (vertical.size, horizontal.size)
    require(horizontal.forall(_.forall(_ > 0)) && vertical.forall(_.forall(_ > 0)))
  }

  object NonogramPuzzle {
    def apply(input: (String, String)): NonogramPuzzle = {
      val (horizontal, vertical) = input
      def parse(string: String): Seq[Seq[Int]] =
        string.split("\n").map(_.split(",")).map(s => if(s.nonEmpty) s.map(_.toInt).toSeq else Seq())

      NonogramPuzzle(parse(horizontal), parse(vertical))
    }
  }

  def solve(puzzle: NonogramPuzzle): Option[IndexedSeq[IndexedSeq[Boolean]]] = {
    val variables: IndexedSeq[IndexedSeq[PropVar]] =
      (0 until puzzle.height).map(y => (0 until puzzle.width).map(x => propVar(s"($x,$y)")).toVector).toVector

    def and(seq: Seq[Formula]): Formula = if(seq.nonEmpty) seq.reduce(_ && _) else True
    def or(seq: Seq[Formula]): Formula = if(seq.nonEmpty) seq.reduce(_ || _) else True

    def combinations(vector: IndexedSeq[PropVar], clues: Seq[Int]): Formula = {
      def combinations(vectorToComplete: IndexedSeq[Boolean], index: Int, clues: Seq[Int]): Seq[IndexedSeq[Boolean]] = {
        if (clues.isEmpty)
          Seq(vectorToComplete)
        else {
          val (head, tail) = (clues.head, clues.tail)
          val maxIndex = vector.size - (clues.sum + clues.size - 1)

          val combs = for (i <- index to maxIndex) yield {
            val zipped: IndexedSeq[(Boolean, Int)] = vectorToComplete.zipWithIndex
            val filled: IndexedSeq[Boolean] = zipped.map{ case (b, j) => if (j >= i && j < i + head) true else b }

            combinations(filled, i + head + 1, tail)
          }
          combs.flatten
        }
      }
      or(combinations(Vector.fill(vector.size)(false), 0, clues).map(v => and(v.zipWithIndex.map{ case(b, i) => if (b) vector(i) else !vector(i) })))
    }

    val constraintHorizontal = for (y <- 0 until puzzle.height)
      yield combinations(variables(y), puzzle.horizontal(y))

    val constraintVertical = for (x <- 0 until puzzle.width)
      yield combinations(variables.map(v => v(x)), puzzle.vertical(x))

    val allConstraints = constraintHorizontal ++ constraintVertical

    solveForSatisfiability(and(allConstraints)).map(solution => variables.map(_.map(v => solution.getOrElse(v, false))))
  }


  // Actual solving

  val puzzle = NonogramPuzzle(example1)

  val t0 = System.currentTimeMillis()
  solve(puzzle) match {
    case Some(solution) =>
      val t1 = System.currentTimeMillis()
      println(s"Found solution in ${t1 - t0} ms")
      println(solution.map(_.map(b => if(b) "#" else ".").mkString).mkString("\n"))

    case None => println("Unsatisfiable problem!")
  }

}
