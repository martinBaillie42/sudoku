/**
 *
 */
package sudoku

import scala.io.Source._
import javax.swing._

/**
 * @author Martin Martin
 *
 */
object sudoku2 {

  def main(args: Array[String]): Unit = {
    val fileList = formatFile(loadFile)
    val sudokuGrid = genSudokuGrid(fileList)

//    println(sudokuGrid foreach (x => x foreach (y => println(y)) ))
//    println(sudokuGrid foreach (x => println(x(0))))
    val completeSudokuGrid = solveSudoku(sudokuGrid)

//    completeSudokuGrid foreach (println(_))
//    println(completeSudokuGrid foreach (x => println(x(0))))
//      println(completeSudokuGrid(0) foreach (x => println(x)))
//
//    println(completeSudokuGrid foreach (x => x foreach (y => println(y)) ))
    for (y <- 0 until 9) {
      for (x <- 0 until 9) {
        if (completeSudokuGrid(x)(y).size == 1) {
          print(completeSudokuGrid(x)(y)(0) + " ")
        } else {
          print("_ ")
        }
      }
      println()
    }
  }
  
  def solveSudoku(sudokuArray: Array[Array[List[Int]]]) = {
    def arrayFiltersList (allSingle: Array[Int], i: Int) = {
      if (allSingle.exists(_ == i)) false else true
    }
//    first thing to do is to make a list of all the single lists
//    in each x position (columns) and then remove this numbers from the lists with
//    more than one int.  
    def solveVertical(y: Int ): Boolean = {
      if (y < 9) {
        val solvedCellsArray = for (x <- sudokuArray if x(y).length == 1) yield x(y)(0)
        for (x <- sudokuArray) if (x(y).length > 1) {
          x(y) = x(y).toList filter (i => arrayFiltersList(solvedCellsArray, i))
        } 
        solveVertical(y + 1)
      }
      true
    }
//    now do (nearly) the same thing horizontally 
    def solveHorizontal(x: Int ): Boolean = {
      if (x < 9) {
        val solvedCellsArray = for (y <- (0 to 8) if sudokuArray(x)(y).length == 1) yield sudokuArray(x)(y)(0)
        for (y <- (0 to 8) if sudokuArray(x)(y).length > 1) {
          sudokuArray(x)(y) = sudokuArray(x)(y).toList filter (i => arrayFiltersList(solvedCellsArray.toArray, i))
        } 
        solveHorizontal(x + 1)
      }    	
      
      true
    }

    solveVertical(0)
    solveHorizontal(0)
    
//    def pos (y: Int, x: Int) = {
//      sudokuArray(y)(x)
//    }
//
//    def horizontalAnswers (y: Int) = {
//      (for (x <- (0 to 8) if pos(y,x).length == 1) yield pos(y,x)(0)).toArray
//    }
//
//    def verticalAnswers (x: Int) = {
//      (for (y <- (0 to 8) if pos(y,x).length == 1) yield pos(y,x)(0)).toArray
//    }
    
//    def calcH(y: Int) = {
//      sudokuArray(y).indexWhere()
//      sudokuArray(y)
//    }
    
//    def diagonal(yx: Int): Array[Array[List[Int]]] = {
//      if (yx < sudokuArray.length) {
//        sudokuArray(yx) = sudokuArray(yx) foreach (l => l filter (i => case ) )
//        diagonal(yx + 1)
//      }
//      sudokuArray
//
//    }
    
    def test(x: Array[List[Int]]) = {
      println(x)
      true
    }
    
//    println(sudokuArray.indexWhere { (x) => test(x) })
//    println(sudokuArray.view.zipWithIndex foreach {case (value, index) => })
//    diagonal(0)
    sudokuArray
  }
  
  def genSudokuGrid (list: List[Char]) = {
    def formattedGrid = {
      list.map(ch => ch match {
        case '_' => (1 to 9).toList 
        case _ => List(ch.toInt - 48)
      }) 
    }
    formattedGrid.toArray grouped 9 toArray
  }

  def loadFile = {
    val chooser = new JFileChooser()
    val view = chooser.getFileSystemView()
//    TODO: re-enable file selection
//    val in = Console.readLine("Place the Sudoku text file in the '" + view.getHomeDirectory + "' directory. \nEnter its filename here (minus the 'txt' suffix): ")
//    use file.exist to check if it exists
    
    fromFile(view.getHomeDirectory + "/" + "sudoku" + ".txt")
  }
  
  def formatFile (file: scala.io.BufferedSource) = {
   
    def fileAsString = {
      file.getLines mkString "\n"
    }
    
    def filterString = {
      fileAsString filter ("_123456789" contains _ )
    } 
    
    def fileListToArray = {
      filterString.toList
    }   
        
    fileListToArray 
  }
  

}