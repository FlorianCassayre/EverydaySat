package everydaysat.nonogram

object NonogramExamples {

  val example1: (String, String) =
    ("""3
      |4,2
      |6,6
      |6,2,1
      |1,4,2,1
      |6,3,2
      |6,7
      |6,8
      |1,10
      |1,10
      |1,10
      |1,1,4,4
      |3,4,4
      |4,4
      |4,4""".stripMargin,
    """1
      |11
      |3,3,1
      |7,2
      |7
      |15
      |1,5,7
      |2,8
      |14
      |9
      |1,6
      |1,9
      |1,9
      |1,10
      |12""".stripMargin)

  val example2: (String, String) =
    ("""4,1,2,1,3
       |2,1,1,1,4,3
       |2,3,11
       |3,1,7,5
       |3,15
       |1,10,4
       |2,1,1,1,8
       |6,9
       |4,1,5
       |3,3,1
       |3,3,1
       |2,1,3,4,2
       |1,1,10,1,4
       |13,5
       |7,6,6
       |8,1,4,3,2
       |9,1,3,3,1
       |15,3
       |3,1,1,3,2
       |1,1,1,4
       |2,6
       |3,6,1
       |6
       |1,1,4
       |1,2,4""".stripMargin,
      """1,1,1,3,4
        |3,1,2,4
        |2,1,5,2
        |1,2,6
        |3,7
        |3,5
        |4,7
        |1,2,3,1
        |2,3,3,3,2
        |1,1,2,2,1,3
        |7,12
        |2,3,4,2,1,1
        |2,7,8,1,1
        |3,7
        |8,6,1
        |7,5
        |11,3
        |12,2
        |3,6,7
        |1,5,8
        |9,6,3
        |6,4,3,2
        |6,1,4,2
        |5,4,2
        |5,4,1,2""".stripMargin)

}
