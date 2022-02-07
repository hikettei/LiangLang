
# Liang Programming Language

自己満足で書いてます

To build this project, please refer to ./build.sh. Here is the way to build it on roswell.

There is some sample codes in ./ExampleCodes.

# Usage

```bash
./liang <Command> <Args1> <Args2> ...
```

### Example

To compile the examples...

```bash
./liang compile ./ExampleCodes/fib.liang ./ExampleCodes/fib.lvm
```

And then, liang-compiler generates a compiled liang-program. To execute it...

```bash
./liang run ./ExampleCodes/fib.liang
```

You can do these process in the same time by using scr command.

```bash
./liang scr ./ExampleCodes/fib.liang
```

Here is the list of another command.

```
eval <Liang Code>
disasm <Liang Source Code File>  ; it displays the lvm data (lvm is compiled liang code)
time <Liang Source Code File>    ; it measures executing time.
profile <Liang Source Code File> 
```
# Basic

In Liang, the first-class object is a function. All features is based on lambda-expressions.

### Define functions

Use lambda-expression.

```
a = { "Hello World" };
print(a()); #=> Hello World

a = (text){ text };
print(a("Hello World")); #=> Hello World
```

To describe it simply, you can do it in this way too.

```
a(text) = { text };
print(a("Hello World"));
```

In the special condition of been defined two-argument functions, it can be used as a operators too.

```
# In ./ExampleCodes/sum.liang

! = (a, b){
  if(a equals b, { a }, { a + !(a+1, b)});
};

# Calling !, as a function or operator.

print(1 ! 10);
print(!(1, 10));
```

### conditional branch

You can use if or when. Both of them are implemented as a just function not a syntax.

```
if(condition, then, else)

when(condition, then)
```

For example

```
text = readline();

if(text equals "A", { print("you said A") }, { print("you didn't say A") });

when(text equals "A", { print("you said A") });
```

(In addition: Programs being just one-line in lambda, you can omit a semicolons. However, if programs is sequentially, you have to append a semicolons.

### Array

### Structures

Structures is implemented by macro features and is just working as array.

### Macros

# Environment

```
MacOS Monterey (ver 12.0.1)
SBCL 2.0.2
roswell 19.05.10.99(NO-GIT-REVISION)
```


