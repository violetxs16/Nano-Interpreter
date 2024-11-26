Goal: This project involves implementing a functional programming language interpreter for Nano in Haskell, leveraging abstract syntax trees and environments to evaluate Nano programs. The interpreter supports data types such as integers, Booleans, and lists, along with essential language constructs like arithmetic and logical operations, conditionals, and variable binding through let-expressions. Additionally, it includes features for defining and invoking both anonymous and named functions, including recursive functions. The system also manages list operations, such as creating and manipulating lists with head and tail functions. Through this implementation, the interpreter efficiently evaluates Nano programs while ensuring proper scoping, binding, and error handling for unsupported operations.


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
