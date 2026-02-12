# Chestnut

This is the first C++ compiler for my programming language, Chestnut.

It is under heavy development, so expect segmentation faults and bugs
everywhere :-)

## Building
You must have the LLVM and Boost libraries. On Arch Linux, you may install them
by running `sudo pacman -S llvm boost`.

To actually build, use `make build`, `make run` (to compile and run the file
`my_module`) or `make test` (to run all compiler-internal tests).

## Examples

### Statement showcase
All kinds of statements are showcased in this sample. It is encouraged to
figure out the language by trying out things, though :)

```
@extern
func putchar(anon _: char);

@extern
func main() {
        const a: uint64 = 9;   // mutability must be specified
        mut   b         = 10;  // types may be inferred

        const x: bool;            // takes default value (false)
        mut y: bool = undefined;  // takes no value!

        b = a;  // we can mutate mutable variables
        x = y;  // this will throw an error

        // we can have subscopes
        {
                putchar(' ');  // we can call functions
                2 + 3;         // this will throw a warning due to the value being discarded!
        }

        // some high-level control flow
        if (x != y) return;

        // some low-level control flow
'b:
        goto 'c;  // unconditional jump
'c:
        branch (a == b) 'a 'b;  // branch to 'a if the condition is true, otherwise to 'b
'a:
        return;
}
```

### Hello World
You must link with libc for this example to run. Since we don't have strings
yet, this looks silly :P

```
@extern
func putchar(anon _: char);

@extern
func main() {
        putchar('H');
        putchar('e');
        putchar('l');
        putchar('l');
        putchar('o');
        putchar(',');
        putchar(' ');
        putchar('w');
        putchar('o');
        putchar('r');
        putchar('l');
        putchar('d');
        putchar('!');
}
```
