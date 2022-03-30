
import scala.io.StdIn.readLine

  object lab1 {

    def main(args: Array[String]): Unit = {
      val a = BuildMatrix(2, 2)
      val b = BuildMatrix(2, 2)

      println(Addition(a, b))
      println(Multiplication(a, 2))
      println(Division(a, 2.5))
      println(Transposition(a))

      DisplayMatrix(a)
    }

    def BuildMatrix(rows: Int, column: Int): List[List[Double]] = {
      List.fill(column)(readLine().split(' ').map(_.toDouble).toList.take(rows))
    }

    def Addition(a: List[List[Double]], b: List[List[Double]]): List[List[Double]] = {
      var result = List[List[Double]]()
      for (i <- 0 to 1) {
        result = result :+
          a(i).zip(b(i)).map {
            case (a, b) => a + b
          }
      }
      result
    }

    def Subtraction(a: List[List[Double]], b: List[List[Double]]): List[List[Double]] = {
      var result = List[List[Double]]()
      for (i <- 0 to 1) {
        result = result :+
          a(i).zip(b(i)).map {
            case (a, b) => a - b
          }
      }
      result
    }

    def Multiplication(a: List[List[Double]], b: Double): List[List[Double]] = {
      a.map(_.map(x => x * b))
    }

    def Division(a: List[List[Double]], b: Double): List[List[Double]] = {
      a.map(_.map(x => x / b))
    }

    def Transposition(a: List[List[Double]]): List[List[Double]] = {
      a.transpose
    }

    def DisplayMatrix(a: List[List[Double]]): Unit = {
      println(a.map(_.mkString(" ")).mkString("\n"))
    }
  }

