# Elina Interpreter (JPP 2022/2023)

Project written for the Languges and Paradigms of Programming (pol. Języki i Paradygmaty Programowania, JPP) course offered by the Faculty of Mathematics, Informatics and Mechanics at the University of Warsaw 2022/2023.

Interpreter of a custom-designed imperative language called Elina, with a static type-checker, written in Haskell.

## Building and running

```bash
make
```

##### From file

```bash
./interpreter file-with-program.elina
```

##### From standard input

```bash
./interpreter
```

## Examples

Sample programs are available in the [examples](examples) folder:

- Correct programs can be found in the [good](examples/good) subfolder.
- Incorrect programs can be found in the [bad](examples/bad) subfolder.

## Testing

To test the solution on programs from the `examples` folder, use the following command:

```bash
bash test.sh
```

## Description

Detailed description about the language is available in the [Elina.md](Elina.md) file.

### Prerequisites

- GHC
- BCNF
- Happy
- Alex
