In this assignment you will implement the core ideas (not the actual layout for any given architecture 
or language/compiler, and not the actual code generation) for understanding implementations of the 
(static/lexical) scoping discipline in Algol-like languages, particularly the visibility rules and the 
set-up/tear-down involved in procedure call and return. 

At the end of this page is the skeletal structure of a sample program with nested procedures.  
Your assignment is to be done with respect to this particular program structure. 
All arguments and local variables are specified.  For simplicity these are all assumed to be integers.  

What you have to do:

For each of these 9 procedures (including main) provided below, 
you will need to hard-code a mock-up data structure to simulate its stack frame -- 
that is, a record consisting of the name of the procedure, its parameters and local variables.  
Further, you will have to maintain the dynamic links (for call and return from procedure calls). 
The assignment is testing whether you can also correctly implement the static scoping discipline --  
either by building the static link chain, or by maintaining "display registers". 

The bodies of the main program and all the procedures are not specified.  
Instead your simulator will take a command line argument and perform the necessary operation, 
as if you were providing the program/procedure body "on the fly".

At any point in the simulation, you should be able to show 

the call stack (what procedures have been called so far, in the calling sequence
What procedures can be called from the current procedure frame at the top of the stack
All the named variables accessible from the current procedure frame, and their current values.
The static link chain or the display registers.
Instead of generating code for the procedures, you will have to develop an interactive environment -- 
a read-execute-display loop.  From the user, you should be able to take the following commands, and implement them correctly.

Set the value of a variable to a given value/expression. (e.g.,   x := y  or x := 3 or...). 
You should show that the correct instance of x is updated.  
If x or y is not accessible, an informative error message should be displayed. 
Call a procedure with specified arguments (e.g.,  call P(c,7) or  call S(a,5) or ...) 
-- If the procedure call is legal, then you should stack up the called procedure's frame 
with the parameters bound to the actual arguments and the static links/display registers, 
and the dynamic links correctly updated.  
If the call is not legal (procedure is not callable, or the arguments do not match the number of parameters), 
then an informative error message should be displayed.
Return from the current procedure to caller. 
The current call stack should be popped, and the static links/display registers correctly updated.
If you have a nice way of displaying the point of execution, 
you may want to show the state (points 1,2,3,4 above) as the result of executing the user command.

* Finally, your submission should include test input sequences of user commands that you have 
designed to show that you have correctly implemented the call and return of procedures correctly.



-- THE SAMPLE PROGRAM FOR THE ASSIGNMENT --

program main;

  var a: Tint; b: Tint; c: Tint;

  procedure P(x: Tint; y: Tint);

        var z: Tint; a: Tint;

        procedure R(w: Tint; i: Tint);

                var j: Tint; b: Tint;

                procedure V(m: Tint; n: Tint);

                        var c: Tint;

                        { body of V elided }

                { body of R elided }

        procedure S(c: Tint; k: Tint);

                var m: Tint; n: Tint;

                { body of S elided }

  procedure Q(z: Tint; w: Tint);

        var x: Tint; b: Tint;

        procedure T(a: Tint; y: Tint);

                var i: Tint; f: Tint;

                procedure W(m: Tint; p: Tint);

                       var j: Tint; h: Tint;

                       { body of W elided }

                { body of T elided }

        procedure U(c: Tint; z: Tint);

                var p: Tint; g: Tint;

                { body of U elided }

  { body of main elided }
