Goal: The project involves creating an interpreter for a minimal functional programming language called Nano, implemented in Haskell. This interpreter allows evaluation of Nano programs through abstract syntax trees and environments, offering hands-on experience with concepts like scoping, binding, and closures.

Features of the Nano Interpreter:
Data Types:

Supports integers, Booleans, and lists.
Represents lists as nested pairs (VPair) and includes operations like head and tail.
Expressions:

Arithmetic operations: Handles addition, subtraction, and multiplication with integer values.
Boolean operations: Supports logical (And, Or) and comparison operators (Eq, Ne, Lt, Le).
Conditionals: Allows evaluation of if expressions based on Boolean conditions.
Let-expressions: Enables variable binding and local scoping.
Functions:

Anonymous functions: Implements lambda expressions with closures.
Named and recursive functions: Supports recursion using let expressions to define functions referring to themselves.
Lists:

Includes list creation with the Cons operator and handles empty lists (ENil).
Adds support for primitive functions like head and tail.
Execution Tools:

Provides functions like execExpr for evaluating expressions, parse for converting strings to abstract syntax trees, and execString/execFile for executing Nano programs from strings or files.
