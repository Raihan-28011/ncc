# NoobC - C with slight touch of python

NoobC is a very minimal language with very limited capabilities. It has C like syntax, with slight
touch of python. Very easy to learn and use. And also it is fast.

# Syntax

Very C like syntax, with some strict syntax rules.

## Data types
```go
    var character = 'a';
    var interger = 100;
    var float = 100.0; // Floating points have to have a radix point an a single digit after it
    var string = "string";
    var boolean = true;
    var null = nil;

```

## Supported Operators (for now)
    - Arithmetic operators
        - + (expectes integer or floats)
        - - (same as '+')
        - * (same as '+')
        - / (same as '+')
        - % (same as '+')
    - Relational operators
        - <  (expects integer or floats or characters)
        - >  (same as '<')
        - <= (same as '<')
        - >= (same as '<')
    - Equality operators
        - == (defined for all type of data)
        - != (same as '==')
    - Logical Operators
        - || (expects boolean operands)
        - && (same as '||')
    - Unary Operators
        - - (expects integer or double)
        - + (same as unary '-')
        - ! (defined for all type od data) 

(``NOTE``: All binary operators expectes both operand to be of same type. Otherwise it is a
runtime error)


## Variable declaration
```go
    // Global scpope
    var variable;// By default it will be initialised to nil

    func examples() {
        // Local scope
        var variable; // Variable shadowing is supported. global 
        var variable = 100; // Error. cannot declare variables with same name in same scope
        variable = "variable"; // Variables can be initialised with any type of data    
    }
``` 

## Conditional statements
```go
    var a = 12;
    if (a > 0) {     // Braces are must. If braces are omitted, it will be a syntax error
        print("positive number\n");
    } elif (a < 0) {
        print("negative number\n");
    } else {
        print("zero\n");
    }

```

## Control Statements
```go
    for (var i = 0; i < 100; i = i + 1) {   // As like if conditionals, braces are must
        ;   // This semicolon can be omitted. Body of the for loop can be empty.
    }
    
    var i = 0;
    while (i < 100) {
        i = i + 1;
    }

```
(``NOTE``: break and continue statements are not yet supported)

## Functions

```go
   // Functions are always defined with 'func' keyword
   // Argument list can be empty. If contains any argument, only the name of the argument is required.
    func fib(n) {
        if (n < 2) {
            return n;
        }

        return fib(n - 1) + fib(n + 2);  // recursion is also supported
    }

```

## Builtin Functions

```go
    func main() {       // execution always starts from main function
        var a;

        // To print expressions, expression has to be inside curly braces.
        print("a: {a}\n");  // Output: `a: nil`
        print({a});     // Error. Print arguments have to be inside a string

        geti(a);    // Gets an integer from stdin
        print("integer: {a}\n");

        getc(a);    // Gets a character from stdin
        print("character: {a}\n");

        getd(a);    // Gets a floating point number from stdin
        print("double: {a}\n");
    }

```
(``NOTE``: String inputs are not supported yet)


## How Fast is it?

Here is a fibonacci function implemented in NoobC
```go
    func fib(n) {
        if (n < 2) {
            return n;
        }

        return fib(n - 1) + fib(n - 2);
    }

    func main() {
        print("30th fibonacci is: {fib(30)}\n");
    }

```
If ncc is compiled with all g++/clang++ optimization on. This code execution finishes within 0.45s


## Where to start?

This is a total mess. I haven't yet organised the code base. But here is a rough sketch of
how to inspect the code
    - main()
    - interpret()
        - compile()
            - parse_functions()
                - parse_function_declaration()
                - parse_declaration()
                    - parse_variable_declaration()
                    - parse_statement()
        - run_vm()

All compilation happens in compile command and runtime starts at run_vm() function.
