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
    var str_var = "string";
    var boolean = true;
    var null = nil;
    string str[15] = "new string type"; // New string type has been added
                                        // Size of the string has to be known at compile time.

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
        - ++ (expects integer or double) (new)
        - -- (same as prefix increment operator) (new)
        - -  (same as prefix increment operator)
        - +  (same as prefix increment operator)
        - !  (defined for all type od data) 

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
## Array (new)

In NoobC, array has been added.

```go
    var a[100] = { 0, 1, 2, 3, 4, 5 };  // arrays can have initializers
    for (var i = 0; i < 100; i = i + 1) {
        print("a[{i]]: {a[i]}\n");      // arrays are indexable
    }

    a[12] = "NoobC";    // arrays are indexable. And yes, the can store any value

```
(``NOTE``: arrays can only be of size 2 to 255. I will try to increase that later.)


## Strings (new)

In NoobC, ``string`` is a new type. It is assignable, printable and indexable.

```go
    string demo[5];
    string demo2[21] = "NoobC has Strings Now";
    demo = demo2;   // this will copy only the 1st 5 characters, as demo is of size 5

    print("demo: {demo}\tdemo2: {demo2}\n"); // output: `demo: NoobC    demo2: NoobC has Strings Now`

    demo[1] = 'M';  // strings are indexable
    print("demo: {demo}\n"); // output: `demo: MoobC`
```
(``NOTE``: As like array, ``strings`` can also be of size 2 to 255. Only local scope arrays are supported fow now)

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

        string str[100];
        gets(str);  // (New)
                    // Gets a string from stdin until it founds any one of the following constraints:
                    // 1. 100 characters were read
                    // 2. space or tab or newline or eof was found

        gets(a);    // Error! gets only accepts data of type string
    }

```
(``NOTE``: String inputs are not supported yet. Only local scope strings are supported for now)

## Refenrences (new)

```go
    func get_integer_with_ref(&n) {     // to indicate that the argument is a reference, & is prefixed to the argument name
        print("Enter a number: ");
        geti(n);
    }

    func get_integer_without_ref(n) {
        print("Enter a number: ");
        geti(n);
    }

    func main() {
        var a;
        get_integer_without_ref(a);     // 100 will be the input
        print("Without reference: {a}\n\n");    // output: `Without reference: nil`

        get_integer_with_ref(&a);   // to pass by reference, & have to be prefixed
        print("With reference: {a}\n");     // output: `With reference: 100`
    }

```

(``NOTE``: Functions cannot take arrays as argument yet. Only local variables support reference.
global variable reference is not yet supported)

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
```
    - main()
    - interpret()
        - compile()
            - parse_functions()
                - parse_function_declaration()
                - parse_declaration()
                    - parse_variable_declaration()
                    - parse_statement()
        - run_vm()
```
All compilation happens in compile command and runtime starts at run_vm() function.
