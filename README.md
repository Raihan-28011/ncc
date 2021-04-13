# NoobC - C with slight touch of python

NoobC is a very minimal language with very limited capabilities. It has C like syntax, with slight
touch of python. Very easy to learn and use. And also it is fast.

## New Changes
Date: 12.04.2021
- Support for windows newline encoding
- Support for windows console colors
    - If now any error occurs, in windows, errors will be printind in colors as like linux
- Removed all scanfs, so that msvc doesn't give any warnings

## New Features
Date: 13.04.2021
- Support for arrays as function arguments
- Type casting

Date: 12.04.2021
- Support for prefix increment operator and prefix decrement operator
- New ``string`` type
- Support for string inputs
- Support for local arrays
- Runtime input error checking for integers and doubles

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
                                        // NOTE: no need for 1 extra space for null character,
                                        // ncc, handles that internally

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
        - !  (defined for all type of data) 

(``NOTE``: All binary operators expectes both operand to be of same type. Otherwise it is a
runtime error)


## Variable declaration
```go
    // Global scpope
    var variable;// By default it will be initialised to nil

    func examples() {
        // Local scope
        var variable; // Variable shadowing is supported. global scope 'variable' has been shadowed by local
                      // scope 'variable'
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
    for (var i = 0; i < 100; ++i) {   // As like if conditionals, braces are must
        ;   // This semicolon can be omitted. Body of the for loop can be empty.
    }
    
    var i = 0;
    while (i < 100) {
        ++i;
    }

```
(``NOTE``: break and continue statements are not yet supported)

## Functions

```go
   // Functions are always defined with 'func' keyword
   // Argument list can be empty. If contains any argument, only the name of the argument is required.
   // function body can not be empty
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

    a[12] = "NoobC";    // And yes, arrays can store any value

```
(``NOTE``: arrays can only be of size 2 to 255. I will try to increase that later. Only local scope arrays are supported for now)


## Strings (new)

In NoobC, ``string`` is a new type. It is assignable, printable and indexable.

```go
    string demo[5];
    string demo2[21] = "NoobC has Strings Now";
    demo = demo2;   // this will copy only the 1st 5 characters, as demo is of size 5

    print("demo: {demo}\tdemo2: {demo2}\n"); // output: `demo: NoobC    demo2: NoobC has Strings Now`

    demo[0] = 'M';  // strings are indexable
    print("demo: {demo}\n"); // output: `demo: MoobC`
```
(``NOTE``: As like array, ``strings`` can also be of size 2 to 255. Only local scope strings are supported fow now)

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
                    // NOTE: ncc automatically adds a null character at the end of the string,
                    // no need to allocte an extra space for null character

        gets(a);    // Error! gets only accepts data of type string
    }

```
(``NOTE``: Only local scope strings are supported for now)

## Refenrence (new)

### Reference to local variables
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
        get_integer_without_ref(a);     // If 100 is the input
        print("Without reference: {a}\n\n");    // output: `Without reference: nil`

        get_integer_with_ref(&a);   // to pass by reference, & have to be prefixed
        print("With reference: {a}\n");     // output: `With reference: 100`
    }

```
### Reference to local arrays (new)
Now functions support, arrays as arguments. But, they are only reference. Because, arrays in NoobC, are not copyable or assignable.
And also, it is memory efficient and also removes the complexity of copying datas into new array.
```go

    func get_integers(&array[10]) {     // arrays can be passed to a function, but as a reference
                                        // and the size of the array has to be known at compile time (for now)
        for (var i = 0; i < 10; ++i) {
            var n;
            geti(n);
            array[i] = n;
        }
    }

    func print_array(array[10]) {   // this is an error, array arguments must have to be reference
        for (var i = 0; i < 10; ++i) {
            print("array[{i}]: {array[i]}\n");
        }
    }

    func main() {
        var array[10];
        get_integers(&array);   // to pass an array, & has to be prefixed
        get_integers(array);    // this will be an error
        print_array(&array);
    }

```
(``NOTE``: global arrays are still not supported)


(``NOTE``: Functions cannot take arrays as argument yet. Only local variables support reference.
Global variable reference is not yet supported)

## Type casting (new)

Very C similar syntax. The type in which you want to convert the resulting value, you have to specify that type in
parenthesis. 4 types are supported.
- int_t
- double_t
- char_t
- bool_t 

```go
    func main() {
        print("converting 100 to double: {(double_t)(100)}\n"); // 100.000000
        print("converting 98.12 to char: {(char_t)(98.12)}\n"); // b
        print("converting true to int: {(int_t)(true)}\n");     // 1
        print("converting nil to bool: {(bool_t)(nil)}\n");     // false
        print("error: {(int_t)("raihan")}\n");  // this will cause an error
    }

```

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

All compilation happens in compile() function and runtime starts at run_vm() function.
