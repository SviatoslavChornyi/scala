
import scala.io.StdIn.readLine

object lab1 {

  def main(args : Array[String]) : Unit = {

    println("Chose the operation:\n1.Addition\n2.Subtraction\n3.Multiplication by a number\n4.Division by a number\n5.Transposition\n6.End the program")
    val a = readLine().toInt
    a match{
      case 1 => println("Enter the size of matrices which u want to addition")
        println("Rows :")
        val rows = readLine().toInt
        println("Column :")
        val columns = readLine().toInt
        println("Enter first matrix")
        val firstMatrix = BuildMatrix(rows, columns)
        println("Enter second matrix")
        val secondMatrix = BuildMatrix(rows, columns)
        println("The result of adding matrices:")
        DisplayMatrix(Addition(firstMatrix, secondMatrix, rows - 1))

      case 2 => println("Enter the size of matrices which u want to Subtraction")
        println("Rows :")
        val rows = readLine().toInt
        println("Column :")
        val columns = readLine().toInt
        println("Enter first matrix")
        val firstMatrix = BuildMatrix(rows, columns)
        println("Enter second matrix")
        val secondMatrix = BuildMatrix(rows, columns)
        println("The result of subtraction of matrices:")
        DisplayMatrix(Subtraction(firstMatrix, secondMatrix, rows - 1))

      case 3 => println("Enter the size of matrix which u want to Multiplication")
        println("Rows :")
        val rows = readLine().toInt
        println("Column :")
        val columns = readLine().toInt
        println("Enter first matrix")
        val firstMatrix = BuildMatrix(rows, columns)
        println("Enter the number")
        val number = readLine().toInt
        println("The result of multiplication by number:")
        DisplayMatrix(Multiplication(firstMatrix, number))

      case 4 => println("Enter the size of matrix which u want to Division")
        println("Rows :")
        val rows = readLine().toInt
        println("Column :")
        val columns = readLine().toInt
        println("Enter first matrix")
        val firstMatrix = BuildMatrix(rows, columns)
        println("Enter the number")
        val number = readLine().toInt
        println("The result of division by number:")
        DisplayMatrix(Division(firstMatrix, number))

      case 5 => println("Enter the size of matrix which u want to Transposition")
        println("Rows :")
        val rows = readLine().toInt
        println("Column :")
        val columns = readLine().toInt
        println("Enter the matrix")
        val firstMatrix = BuildMatrix(rows, columns)
        println("The result of transposition by number:")
        DisplayMatrix(Transposition(firstMatrix))
    }

  }

  def BuildMatrix(rows : Int, column : Int): List[List[Double]] = {
    List.fill(rows)(readLine().split(' ').map(_.toDouble).toList.take(column))
  }

  def Addition(a : List[List[Double]], b : List[List[Double]], rows : Int) : List[List[Double]] = {
    var result = List[List[Double]]()
    for (i <- 0 to rows) {
      result = result :+
        a(i).zip(b(i)).map {
        case (a, b) => a + b
      }
    }
    result
  }

  def Subtraction(a : List[List[Double]], b : List[List[Double]], rows : Int) : List[List[Double]] = {
    var result = List[List[Double]]()
    for (i <- 0 to rows) {
      result = result :+
        a(i).zip(b(i)).map {
          case (a, b) => a - b
        }
    }
    result
  }

  def Multiplication(a : List[List[Double]], b : Double) : List[List[Double]] = {
    a.map(_.map(x => x * b))
  }

  def Division(a : List[List[Double]], b : Double) : List[List[Double]] = {
    a.map(_.map(x => x / b))
  }

  def Transposition(a : List[List[Double]]) : List[List[Double]] = {
    a.transpose
  }

  def DisplayMatrix(a : List[List[Double]]) : Unit = {
    println(a.map(_.mkString(" ")).mkString("\n"))
  }
}
