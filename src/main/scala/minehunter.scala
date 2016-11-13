
import scala.io.Source
import scalaz._
import Scalaz._

// This code is shared under the terms of the
// Creative Commons Attribution-ShareAlike 4.0 International license.
// See https://creativecommons.org/licenses/by-sa/4.0/ for more information.

case class Result(pass: Boolean, score: Int)

object MineHunter {

	type Script	= List[String]
	type Mine	= (Int, Int, Int)
	type Position	= (Int, Int)
	type Offset	= (Int, Int)

	val north	= (0, -1)
	val south	= (0, 1)
	val east	= (1, 0)
	val west	= (-1, 0)
	val alpha	= List ( (-1, -1), (-1, 1), (1, -1), (1, 1) )
	val beta	= List ( (-1, 0), (0, -1), (0, 1), (1, 0) )
	val gamma	= List ( (-1, 0), (0, 0), (1, 0) )
	val delta	= List ( (0, -1), (0, 0), (0, 1) )

	val zMap	= (((('a' to 'z') ++ ('A' to 'Z')) zip (1 to 52)) :+ ('*', 0)).toMap
	val zMapR	= zMap.map(_.swap)

	val usage	= "usage: minehunter <minefield> <commands>"

	def main(args: Array[String]) = {
		if (checkArgs(args)) {
			val fields	= getFields(args(0))
			val script	= getScript(args(1))

			if (checkFields(fields) && checkScript(script)) {
				val (mineField, startPosition) = initMinefield(fields)
				val nMines	= mineField.length
				val nMoves	= countMoves(script)
				val nVolleys	= countVolleys(script)
				val result	= simulate(startPosition, mineField, script, nMines, nMoves, nVolleys)
				printResult(result)
			}
			else {
				println(s"error: input file format")
				println(s"$usage")
			}
		}
		else {
			println(s"$usage")
		}
	}

	// count the total number of move commands in the script file

	def countMoves(commands: Script) : Int = {
		val moves = commands.foldLeft (0) ( (b, a) =>
			if (a.containsSlice("north")            ||
				a.containsSlice("south")	||
				a.containsSlice("east")		||
				a.containsSlice("west")) b + 1 else b
		)
		moves
	}

	// count the total number of volleys in the script file

	def countVolleys(commands: Script) : Int = {
		val volleys = commands.foldLeft (0) ( (b, a) =>
			if (a.containsSlice("alpha")            ||
				a.containsSlice("beta")		||
				a.containsSlice("gamma")	||
				a.containsSlice("delta")) b + 1 else b
		)
		volleys
	}

	// check that the args are reasonable

	def checkArgs( args: Array[String]) : Boolean = {
		if (args.length != 2) return false
		if (! checkFile(args(0))) return false
		if (! checkFile(args(1))) return false
		true
	}

	// check that a file exists and is readable

	def checkFile(path: String) : Boolean = {
		if (! new java.io.File(path).exists) {
			println(s"file <${path}> doesn't exist\n")
			return false
		}
		if (! new java.io.File(path).canRead) {
			println(s"file <${path}> isn't readable\n")
			return false
		}
		true
	}

	// read a mine field description

	def getFields( path: String ) : List[String] = {
		Source.fromFile(path).getLines().toList
	}

	// initialize mine field from description

	def initMinefield( lines: List[String] ) : (List[Mine], Position) = {
		var x = 0
		var y = 0;
		var mineField: List[Mine] = Nil

		lines.foreach { line =>
			x = 0
			line.filterNot(c => (c == '\r') || (c == '\n')).foreach { c => zMap.get(c) match {
					case Some(z)    => mineField = mineField :+ (x, y, z)
					case None	=> {}
				}
				x += 1
			}
			y += 1
		}
		val startPosition: Position = (x / 2, y / 2)
		( mineField, startPosition )


	}

	//
	// check a script file
	//
	// the requirements imply but do not state that:
	//		the file must be non-empty
	//		all non-blank lines must be the same length
	//		that the line length and the number of lines must be odd
	//		legal characters are '.', a-zA-Z \n

	def checkFields( fields: List[String] ) : Boolean = {
		val re1 = "[^a-zA-Z\\.\\n]".r
		val oddLinesP = if (fields.length %2 == 0) false else true
		val rval = fields.foldLeft (0, true) ( (b, a) => {
			if (b._1 == 0) (a.length, true)
			else {
				if (a.length == b._1) (b._1, b._2 && true) else (b._1, b._2 && false)
			}
		})
		val sameLengthP = rval._2
		val goodContentP = fields.foldLeft (true) ( (b, a) => {
			re1.findFirstIn(a) match {
				case Some(c)	=> b && false
				case None	=> b && true
			}
		})
		if (oddLinesP && sameLengthP && goodContentP) true else false
	}

	// reads a script file and returns a list of commands

	def getScript(path: String) : Script = {
		Source.fromFile(path).getLines().toList
	}

	// do some simple syntax checking of the script

	def checkScript(script: Script) : Boolean = {
		val s	= script.foldLeft ("") ( (b, a) => b ++ a )
		val re1	= "(north|south|east|west|alpha|beta|gamma|delta)".r
		val re2	= "\\S".r

		re2.findFirstIn(re1.replaceAllIn(s, "")) match {
			case Some(w)	=> false
			case None	=> true
		}
	}

	// print a mine field map, ensuring the the current position is in the middle of the map

	def printField(position: Position, mineField: List[Mine]) = {
		val max		= mineField.foldLeft (0,0) (( b, a ) => (a._1.max(b._1), a._2.max(b._2)))
		val size	= if ((max._1 == 0) && (max._2 == 0)) (1,1)
				        else (((max._1 - position._1).abs * 2) + 1, ((max._2 - position._2).abs * 2) + 1)
		val center	= (size._1 / 2, size._2 / 2)
		val tField      = mineField.map( m => (m._1 - position._1 + center._1, m._2 - position._2 + center._2, m._3))

		val fieldRep    = for ( y <- 0 until size._2; x <- 0 until size._1 ) yield {
			                val z = tField.filter( m => m._1 == x && m._2 == y ).headOption match {
				                case Some(t)	=> t._3
				                case None	=> -1
			}
			(x, y, z)
		}

 		val printRep = fieldRep.foldLeft ("") ( (b, a) =>
 			 { b ++ (if ((a._1 % size._1) == 0) "\n" else "") ++ (if (a._3 == -1) "." else zMapR(a._3).toString) }
 		)
		println(s"${printRep.drop(1)}\n")
	}

	// print the current command

	def printCommand(script: Script) = {
		val command = script.take(1).headOption.getOrElse("")
		println(s"${command}\n")
	}

	// print the result of the simulation

	def printResult(r: Result) = {
		val pass = if (r.pass) "pass" else "fail"
		println(s"$pass (${r.score})")
	}

	// as it says

	def simulate(startPosition: Position, mineField: List[Mine], script: Script,
					nMines: Int, nMoves: Int, nVolleys: Int) : Result = {
		process(startPosition, mineField, script, 1, nMines, nMoves, nVolleys)
	}

	// recursively execute each script line until the game is over

	def process(startPosition: Position, mineField: List[Mine], script: Script,
				stepCounter: Int, nMines: Int, nMoves: Int, nVolleys: Int) : Result = {
		println(s"Step $stepCounter\n")
		printField(startPosition, mineField)
		printCommand(script)
		val (newPosition: Position, newMineField: List[Mine], newScript: Script) = fireAndMove(startPosition, mineField, script)
		printField(newPosition, newMineField)
		gameOverP(newMineField, newScript, nMines, nMoves, nVolleys) match {
			case Some(result) => {
				return result
			}
			case None => {
				process(newPosition, newMineField, newScript, stepCounter + 1,
					nMines, nMoves, nVolleys)
			}
		}
	}

	// is the simulation over ?

	def gameOverP(mineField: List[Mine], script: Script,
				nMines: Int, nMoves: Int, nVolleys: Int) : Option[Result] = {
		val kaboom		= mineField.foldLeft (false) ( (b, a) => if (a._3 != 0) b || false else b || true )
		val minesRemaining	= mineField.length
		val commandsRemaining	= script.length

		if (kaboom) {
			Some(Result(false, 0))
		}
		else if (minesRemaining > 0) {
			if (commandsRemaining == 0) Some(Result(false, 0))
			else None
		}
		else {
			if (commandsRemaining > 0) Some(Result(true, 1))
			else {
				val score = (10 * nMines) - (5 * nVolleys).min(5 * nMines) - (2 * nMoves).min(3 * nMines)
				Some(Result(true, score))
			}
		}
	}

	// fire an optional volley and make an optional x,y move in the minefield then drop through a level

	def fireAndMove(position: Position, mineField: List[Mine], script: Script) : (Position, List[Mine], Script) = {
		val commands	=	script.head
		val pattern	=	if (commands.containsSlice("alpha")) alpha
						else if (commands.containsSlice("beta")) beta
						else if (commands.containsSlice("gamma")) gamma
						else if (commands.containsSlice("delta")) delta
						else Nil
		val offset	=	if (commands.containsSlice("north")) north
						else if (commands.containsSlice("south")) south
						else if (commands.containsSlice("east")) east
						else if (commands.containsSlice("west")) west
						else (0,0)

		val newMinefield	= if (! pattern.isEmpty) sweep(position, mineField, pattern) else mineField
		val newPosition		= move(position, offset, newMinefield)
		(newPosition, dropThrough(newMinefield), script.drop(1))
	}

	// clear the mineField of all mines whose x,y coordinates coincide with any element in a list of targets

	def sweep(position: Position, mineField: List[Mine], pattern: List[Offset]) : List[Mine] = {
		val targets = pattern.map( p => p |+| position)
		mineField.filter( m =>
			targets.foldLeft (true) ( (b, a) =>
				if ((m._1 == a._1) && (m._2 == a._2)) b && false else b && true
			)
		)
	}

	// move to a new x,y position in the mine field
	// the specification is ambiguous with respect to what happens if you try to move off map
	// we adopt the convention that if you attempt to move off map, no move is performed

	def move(startPosition: Position, offset: Offset, mineField: List[Mine]) : Position = {
		// get the map limits
		val fieldSize: (Int, Int) = mineField.foldLeft ((0,0)) ( (b, a) => (a._1.max(b._1), a._2.max(b._2)) )
		val newPosition = startPosition |+| offset
		if ((newPosition._1 < 0)	        ||
		        (newPosition._2 < 0)	        ||
			(newPosition._1 > fieldSize._1)	||
			(newPosition._2 > fieldSize._2)) startPosition else newPosition
	}

	// drop through the mine field on level

	def dropThrough(mineField: List[Mine]) : List[Mine] = {
		mineField.map( m => (m._1, m._2, m._3 - 1))
	}
}

