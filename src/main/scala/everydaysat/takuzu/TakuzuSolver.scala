package everydaysat.takuzu

import cafesat.api.Formulas._
import cafesat.api.FormulaBuilder._
import cafesat.api.Solver.solveForSatisfiability

import TakuzuExamples._

object TakuzuSolver extends App {

  case class TakuzuPuzzle(grid: IndexedSeq[IndexedSeq[Option[Boolean]]]) {
    val size: Int = grid.size
    require(size % 2 == 0)
  }

  object TakuzuPuzzle {
    def apply(input: String): TakuzuPuzzle =
      TakuzuPuzzle(input.split("\n").map(_.map({
        case '0' => Some(false)
        case '1' => Some(true)
        case '?' => None
      })))
  }

  def solve(puzzle: TakuzuPuzzle): Option[IndexedSeq[IndexedSeq[Boolean]]] = {
    val variables: IndexedSeq[IndexedSeq[PropVar]] =
      (0 until puzzle.size).map(y => (0 until puzzle.size).map(x => propVar(s"($x,$y)")))

    def intToBinary(n: Int): Seq[Boolean] = {
      def intToBinary(n: Int, acc: Seq[Boolean]): Seq[Boolean] = {
        if(n > 0)
          intToBinary(n / 2, (n % 2 == 1) +: acc)
        else
          acc
      }
      intToBinary(n, Seq())
    }
    def binaryToInt(binary: Seq[Boolean]): Int =
      binary.foldLeft(0)((acc, b) => (acc << 1) | (if (b) 1 else 0))

    val vectorsSize = intToBinary(puzzle.size).size // Technically, this size can be lower under certain conditions
    val vectorCorrectCount = false +: intToBinary(puzzle.size).dropRight(1)

    def and(seq: Seq[Formula]): Formula = if(seq.nonEmpty) seq.reduce(_ && _) else True
    def or(seq: Seq[Formula]): Formula = if(seq.nonEmpty) seq.reduce(_ || _) else True
    def fullAdder(a: Formula, b: Formula, cIn: Formula): (Formula, Formula) =
      ((a && b) || (a && cIn) || (b && cIn), a xor b xor cIn)
    def adder(n1: Seq[Formula], n2: Seq[Formula]): (Seq[Formula], Set[Formula]) = {
      val (l1, l2) = n1.reverse.zipAll(n2.reverse, False, False).unzip
      def adder(n1: Seq[Formula], n2: Seq[Formula], carry: Formula, acc: Seq[Formula], set: Set[Formula]): (Seq[Formula], Set[Formula]) =
        (n1, n2) match {
          case (Nil, Nil) => (carry +: acc, set)
          case (x :: xs, y :: ys) =>
            val p, vx, vy = propVar()
            val (c, s) = fullAdder(vx, vy, p)
            adder(xs, ys, c, s +: acc, set + !(p xor carry) + !(x xor vx) + !(y xor vy))
        }
      adder(l1, l2, False, Seq(), Set())
    }

    val constraintClues = for {
        y <- 0 until puzzle.size
        x <- 0 until puzzle.size
        value <- puzzle.grid(y)(x)
        variable = variables(y)(x)
      } yield if (value) variable else !variable

    def countOccurences(seq: Seq[PropVar]): (Seq[Formula], Set[Formula]) = {
      def bitCounter(bits: Seq[PropVar]): (Seq[Formula], Set[Formula]) = {
        if (bits.size >= 2) {
          val (a, b) = bits.splitAt(bits.size / 2)
          val (seqA, setA) = bitCounter(a)
          val (seqB, setB) = bitCounter(b)
          val (seqC, setC) = adder(seqA, seqB)
          (seqC, setA ++ setB ++ setC)
        } else if (bits.size == 1) {
          (Seq(bits.head), Set())
        } else {
          (Seq(), Set())
        }
      }
      bitCounter(seq)
    }

    def correctCounts(seq: Seq[PropVar]): Formula = {
      val (occurences, constrainedVars) = countOccurences(seq)
      and((for ((v, b) <- occurences zip vectorCorrectCount) yield if (b) v else !v) ++ constrainedVars)
    }

    val constraintSameCountHorizontally = for {
        y <- 0 until puzzle.size
      } yield correctCounts(variables(y))

    val constraintSameCountVertically = for {
      x <- 0 until puzzle.size
    } yield correctCounts((0 until puzzle.size).map(y => variables(y)(x)))

    val constraintTwoAdjacentHorizontally = for {
      y <- 0 until puzzle.size
      x <- 0 until puzzle.size - 2
    } yield !((variables(y)(x) iff variables(y)(x + 1)) && (variables(y)(x + 1) iff variables(y)(x + 2)))

    val constraintTwoAdjacentVertically = for {
      y <- 0 until puzzle.size - 2
      x <- 0 until puzzle.size
    } yield !((variables(y)(x) iff variables(y + 1)(x)) && (variables(y + 1)(x) iff variables(y + 2)(x)))

    val constraintDifferentRows =
      for {
        row1 <- 0 until puzzle.size
        row2 <- row1 + 1 until puzzle.size
      } yield or(for {
        x <- 0 until puzzle.size
      } yield !(variables(row1)(x) iff variables(row2)(x)))

    val constraintDifferentColumns =
      for {
        col1 <- 0 until puzzle.size
        col2 <- col1 + 1 until puzzle.size
      } yield or(for {
        y <- 0 until puzzle.size
      } yield !(variables(y)(col1) iff variables(y)(col2)))

    val allConstraints = constraintClues ++ constraintSameCountHorizontally ++ constraintSameCountVertically ++ constraintTwoAdjacentHorizontally ++ constraintTwoAdjacentVertically ++ constraintDifferentRows ++ constraintDifferentColumns

    solveForSatisfiability(and(allConstraints)).map(solution => variables.map(_.map(v => solution(v))))
  }


  // Actual solving
  // Only works for powers of 2 (?)

  val puzzle = TakuzuPuzzle(example1)

  val t0 = System.currentTimeMillis()
  solve(puzzle) match {
    case Some(solution) =>
      val t1 = System.currentTimeMillis()
      println(s"Found solution in ${t1 - t0} ms")
      println(solution.map(_.map(b => if(b) "1" else "0").mkString).mkString("\n"))

    case None => println("Unsatisfiable problem!")
  }

}
