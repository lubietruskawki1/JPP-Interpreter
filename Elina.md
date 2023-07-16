# Elina

Imperative statically typed language inspired by Rust.

## Variables

The available types of variables are `int`, `bool`, and `string`. Declaring new variables has the following syntax:

```
var variable_name: variable_type = variable_value;

var intValue: int = 13;
var boolValue: bool = true;
var stringValue: string = "Hello, world!";
```

And also:

```
var variable_name: variable_type;

var intValue: int;
var boolValue: bool;
var stringValue: string;
```

To assign a value to a variable, use:

```
variable_name = variable_value;

intValue = 17;
```

Global variables are available, but they must have an assigned value at declaration.

## Print

The following functions are available: `printInt`, `printBool`, `printString`, and `printTuple`. They all take exactly one argument of the appropriate type.

## Control statements

`while` loops are available:

```
var x: int = 5;
while (x > 0) {
  printInt(x);
  x--;
}
```

Elina also provides `if` and `if-else` statements:

```
var x: int = 11;
if (x % 2 == 0) {
  printString("Given number is even");
} else {
  printString("Given number is odd");
}
```

## Functions

Functions have the following syntax:

```
fn function_name(ref? argument_name1: argument_type1, ...): return_type {}

fn function(argument1: int, ref argument2: string): void {
  return;
}
```

Where:

- `argument1: int` is passed by value
- `ref argument2: string` is passed by reference

Functions of type `void` end with the `return;` statement.

#### Recursion

Elina also allows recursive function calls:

```
fn factorial(n: int): int {
  if (n <= 1) {
    return 1;
  }
  return n * factorial(n - 1);
}
```

#### Nested functions

Functions can also be defined inside other functions:

```
fn f(x: int): int {

  fn g(y: int): int {
    return x + y;
  }

  return g(17);
}

fn main(): void {
  printInt(f(10));
}
```

## Tuples

Tuples are available with nested definitions and assignments:

```
var tuple_name: tuple<variable_types> = [contents];

var tuple: tuple<int, tuple<int, string>> = [17, [13, "Hello"]];
```

Values can be packed into tuples and unpacked.

To assign a tuple to multiple variables, they must be declared earlier:

```
var x: int;
var y: int;
var z: string;
<x, <y, z>> = [17, [13, "Hello"]];
```

Or:

```
var a: int;
var b: tuple<int, string>;
<a, b> = [17, [13, "Hello"]];
```

## `break` and `continue` statements

Loop manipulation statements are available:

```
var x: int = 5;
while (x >= 0) {
  if (x == 2) {
    break;
  } else {
    x--;
    continue;
  }
}
```

The `break` and `continue` statements must appear within a `while` block, but this does not apply to nested function declarations. Therefore, the following program is not valid:

```
fn main(): void {
  while (true) {
    fn f(): void {
      break;
      return;
    }
  }
  
  return;
}
```

## Static typing

Elina is a statically typed language, which means that before a program is executed, the type-checker is run to catch any typing errors.

## Default variable values

Variables that are declared without an assigned value will be assigned the default values:

- `int` = `0`
- `bool` = `false`
- `string` = `""`

Variables of type `tuple` will be assigned a tuple with default values of the corresponding types, i.e.,

```
var tupleWithoutValue: tuple<int, tuple<bool, string>>;
```

will have the value:

```
[0, [false, ""]]
```

## Presence of `main` function

To be a valid program (according to the type-checker), it must include a `main` function of type `void` that does not take any arguments.

This means that the shortest valid program in the Elina language is:

```
fn main(): void {
  return;
}
```

## `return` statement

All functions must end with a `return [value of the return type]` statement. Functions of type `void` can end with just the `return` statement.

Every `return` statement in a function must return the correct type.

The `return` statement **can** only appear in both blocks of an `if else` statement if both blocks return the correct type:

```
fn f(): int {
  if (true) {
    return 13;
  } else {
    return 17;
  }
}
```

The `return` statement **cannot** only appear in one block of an `if else` statement:

```
fn f(): int {
  if (true) {
    return 13;
  } else {
  
  }
}
```

The `return` statement **cannot** only appear in the block of an `if` statement:

```
fn f(): int {
  if (true) {
    return 13;
  }
}
```

The `return` statement **cannot** only appear in the block of a `while` statement:

```
fn f(): int {
  while (true) {
    return 13;
  }
}
```

The above invalid examples are allowed if the function also contains another "publicly accessible" `return`:

```
fn f(): int {
  while (true) {
    return 13;
  }
  return 17;
}
```

## Allowed variable types

Variables of type `void` are illegal. As for tuples, empty tuples are not allowed:

```
emptyTuple: tuple<>
[]
```

Tuples containing values of type `void` are also not allowed:

```
var tupleWithVoid: tuple<void>
[main()]
```

Recursively, this rule also applies to nested tuples, meaning tuples containing (arbitrarily nested) empty tuples or values of type `void` are also illegal.

These rules also apply to function arguments.