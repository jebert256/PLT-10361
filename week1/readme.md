# Racket Cheat Sheet

## Core Concepts
| Function        | Description                                  |
|----------------|----------------------------------------------|
| `define`        | Bind a name to a value or function           |
| `lambda`        | Create anonymous functions                   |
| `if`            | Conditional branching                        |
| `cond`          | Multi-branch conditional                     |
| `begin`         | Sequence multiple expressions                |
| `quote` / `'`   | Prevent evaluation (e.g., `'a`, `'(1 2 3)`)  |

## List Operations
| Function        | Description                                  |
|----------------|----------------------------------------------|
| `list`          | Create a list                                |
| `cons`          | Add an element to the front of a list        |
| `car`           | Get the first element of a list              |
| `cdr`           | Get the rest of the list                     |
| `null?`         | Check if a list is empty                     |
| `length`        | Get the number of elements in a list         |
| `append`        | Concatenate lists                            |
| `reverse`       | Reverse a list                               |
| `member`        | Check if an element is in a list             |
| `map`           | Apply a function to each element             |
| `filter`        | Keep elements that satisfy a predicate       |
| `foldl` / `foldr` | Reduce a list from left/right              |

## Math & Numbers
| Function        | Description                                  |
|----------------|----------------------------------------------|
| `+`, `-`, `*`, `/` | Basic arithmetic                         |
| `modulo`        | Remainder after division                     |
| `abs`           | Absolute value                               |
| `expt`          | Exponentiation (`expt 2 3` → 8)              |
| `sqrt`          | Square root                                  |
| `max` / `min`   | Largest/smallest of arguments                |
| `random`        | Generate a random number                     |

## Logic & Comparison
| Function        | Description                                  |
|----------------|----------------------------------------------|
| `=`, `<`, `>`, `<=`, `>=` | Numeric comparisons               |
| `equal?`        | Deep equality check                          |
| `eq?`           | Identity check (same memory reference)       |
| `and`, `or`, `not` | Logical operations                       |

## Testing & Debugging
| Function        | Description                                  |
|----------------|----------------------------------------------|
| `display` / `displayln` | Print to console                    |
| `printf`        | Formatted output                             |
| `trace`         | Show function calls and arguments            |
| `error`         | Raise an error                               |
| `require rackunit` | Load unit testing framework              |
| `check-equal?`, `check-not-equal?` | Assertions for tests     |

## Modules & Files
| Function        | Description                                  |
|----------------|----------------------------------------------|
| `require`       | Import libraries or modules                  |
| `provide`       | Export definitions from a module             |
| `module+`       | Add code to a module (e.g., for testing)     |

## Strings
| Function        | Description                                  |
|----------------|----------------------------------------------|
| `string?`       | Check if value is a string                   |
| `string-length` | Get length of a string                       |
| `substring`     | Extract part of a string                     |
| `string-append` | Concatenate strings                          |
| `string->list`  | Convert string to list of characters         |
| `list->string`  | Convert list of characters to string         |
