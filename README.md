A Solution To The Minehunter Exercise In Scala
==============================================

This is a sbt project containing a scala solution to the well known minehunter exercise.

* It uses a standard SBT directory structure. You must have sbt installed to run this simulation.
* The problem statement can be found in ./doc/minehunter-exercise.pdf.
* Minefield, script and expected output files corresponding to the examples in the problem definition are provided in ./src/test/resources.
* To run the simulator, checkout a copy and cd into the project directory. Compile the simulator with the command 'sbt compile' and run it with the command 'sbt run <path-to-minefield-file> <path-to-script-file>'.
* Rudimentary error checking is performed on the input files.

Checkout a copy
---------------

You can checkout a copy of this project with this command:

<pre>
git clone git://github.com/n0n3such/minehunter.git
</pre>

Notes
-----

The problem statement is incomplete with respect to the format of the input files and certain expected behaviors of the simulator. This simulator treats each of these conditions as an error and returns without running the simulation.

* The behavior is undefined when the input files do not conform to the description, for example if the minefield description contains invalid characters or if the script file contains unknown commands.
* The behavior is undefined when one or both of the x and y dimensions of the minefield is even 
* The behavior is undefined when the minefield is not a cuboid.
* The spec is silent on the expected behavior if one or both of the minefield and script files is empty.
* The spec is silent on the expected behavior if one or both of the minefield and script files do not exist.

License
-------

Please read the LICENSE file for information about applicable license conditions.
