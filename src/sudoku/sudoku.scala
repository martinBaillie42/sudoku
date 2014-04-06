package sudoku

import scala.io.Source._
import javax.swing._

/** Object to load and 'play' a text file containing a Sudoku problem */
object sudoku {

    def main(args: Array[String]): Unit = {
        val fileList = formatFile(loadFile)
        val sudokuGrid = genSudokuGrid(fileList)

        val completeSudokuGrid = solveSudoku(sudokuGrid)

        displayGrid(completeSudokuGrid)

    }

    /** Asks the user to enter the text file to load.
    *
    * @return a BufferedSource object containing the text file selected by the user
    */
    def loadFile = {
        val chooser = new JFileChooser()
        val view = chooser.getFileSystemView
        val in = Console.readLine("Place the Sudoku text file in the '" + view.getHomeDirectory + "' directory. \nEnter its filename here (minus the 'txt' suffix): ")
        fromFile(view.getHomeDirectory + "/" + in + ".txt")
    }

    //
    /** Alters the file so it can be more easily processed by the program.
    *
    * @param file BufferedSource object
    * @return a list containing the reformatted data
    */
    def formatFile(file: scala.io.BufferedSource) = {

        def fileAsString = {
            file.getLines mkString "\n"
        }

        def filterString = {
            fileAsString filter ("_123456789" contains _)
        }

        def fileListToArray = {
            filterString.toList
        }

        fileListToArray
    }

    /** Creates a 9 by 9 array from the formatted sudoku file entered by the user
    *
    * @param list a list of Sudoku data
    * @return an Array containing the unsolved Sudoku puzzle
    */
    def genSudokuGrid(list: List[Char]) = {
        def formattedGrid = {
            list.map {
                case '_' => (1 to 9).toList
                case ch => List(ch.toInt - 48)
            }
        }
        (formattedGrid.toArray grouped 9).toArray
    }

    /** Solves the Sudoku puzzle.
    *
    * @param sudokuArray an Array containing the unsolved Sudoku puzzle
    * @return an array containing the solved Sudoku puzzle
    */
    def solveSudoku(sudokuArray: Array[Array[List[Int]]]) = {

        /** Used by the solve* functions to determine if the current location has only one value
        *
        * @param allSingle a list of the single locations
        * @param i an integer
        * @return boolean
        */
        def arrayFiltersList(allSingle: Array[Int], i: Int) = {
            if (allSingle.exists(_ == i)) false else true
        }

        /** Recursive function that removes known resolved numbers on the y axis
        *
        * @param y location along the y axis
        * @return boolean
        */
        def solveVertical(y: Int): Boolean = {
            if (y < 9) {
                val solvedCellsArray = for (x <- sudokuArray if x(y).length == 1) yield x(y)(0)
                for (x <- sudokuArray) if (x(y).length > 1) {
                    x(y) = x(y).toList filter (i => arrayFiltersList(solvedCellsArray, i))
                }
                solveVertical(y + 1)
            }
            true
        }

        /** Recursive function that removes known resolved numbers on the x axis
        *
        * @param x location along the x axis
        * @return boolean
        */
        def solveHorizontal(x: Int): Boolean = {
            if (x < 9) {
                val solvedCellsArray = for (y <- 0 to 8 if sudokuArray(x)(y).length == 1) yield sudokuArray(x)(y)(0)
                for (y <- 0 to 8 if sudokuArray(x)(y).length > 1) {
                    sudokuArray(x)(y) = sudokuArray(x)(y).toList filter (i => arrayFiltersList(solvedCellsArray.toArray, i))
                }
                solveHorizontal(x + 1)
            }
            true
        }

        /** Recursive function that removes known resolved numbers in each 3 x 3 box
        *
        * @param x location along the x axis
        * @param y location along the y axis
        * @return boolean
        */
        def solveBoxes(x: Int, y:Int): Boolean = {
            if (x < 9 && y < 9) {
                val solvedCellsArray = for (yy <- y-(y%3) to y+2-(y%3); xx <- x-(x%3) to x+2-(x%3) if sudokuArray(xx)(yy).length == 1) yield sudokuArray(xx)(yy)(0)
                for (yy <- y-(y%3) to y+2-(y%3) ; xx <- x-(x%3) to x+2-(x%3) if sudokuArray(xx)(yy).length > 1) {
                    sudokuArray(xx)(yy) = sudokuArray(xx)(yy).toList filter (i => arrayFiltersList(solvedCellsArray.toArray, i))
                }
                solveBoxes(if(x==8) 0 else x + 1, if(x==8) y+1 else y)
            }
            true
        }

        solveVertical(0)
        solveHorizontal(0)
        solveBoxes(7,7)

        sudokuArray
    }

    /** prints the finished puzzle
    *
    * @param completeSudokuGrid the completed puzzle
    */
    def displayGrid(completeSudokuGrid: Array[Array[List[Int]]]) = {
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
}