# Contributing to AVR Emulator

🎉 Welcome, and thank you for considering contributing to the AVR Emulator project! This project aims to provide a robust emulator for the AVR instruction set, and your input—whether through code, documentation, or discussions—is invaluable.

## How to contribute

We welcome contributions in the form of bug fixes, new features, tests, or improvements to the documentation. If you're unsure where to start, take a look at the open [issues](https://github.com/Flu/avr-emulator/issues) — some are tagged as "good first issue" for newcomers.

You are not limited to only doing features or fixes. You could fix typos in comments/README. You can add more comments in the code if you think the code is not well-documented enough. You can add more tests for different functions or parts of the application. 

### Step 1: Fork the repository

If you're new to forking, here's a guide from GitHub: [Fork a repo](https://docs.github.com/en/get-started/quickstart/fork-a-repo).

1. Click the "Fork" button at the top-right corner of the repository page.
2. Clone the fork to your local machine:
```bash
git clone https://github.com/<your-username>/avr-emulator.git
```
3. Navigate to the project directory:
```bash
cd avr-emulator
```

### Step 2: Create a new branch

Always create a new branch for your changes. This keeps your work isolated and makes it easier to manage. If you're adding something, like a new feature, your branch name should be something like `feature/your-feature-branch`. If it fixes a bug, it should be something like `fix/some-bug`.

```bash
git checkout -b feature/your-feature-branch
```

```bash
git checkout -b fix/some-bug
```

### Step 3: Make your changes
- Address an issue or implement a feature from the issues page. Be sure to leave a comment on the issue to let others know you're working on it.
- Mention the issue number in your commits if possible, so there's a clear link between your commits and the issue it solves.
- If your changes add significant functionality, consider writing tests for them (see "Running and Adding Tests" below).
- Ensure your code follows the style and conventions of the project.
- For a short documentation of the code, refer to the section Documentation, below. 

### Step 4: Run the tests
Before submitting your changes, make sure all tests pass:

```bash
cabal test all
```

If you’ve added new functionality, add corresponding tests to cover the changes. Tests ensure that the project remains reliable as it grows. You can find existing tests in the `test/` directory.

### Step 5: Push and create a pull request
Push your branch to your forked repository:

```bash
git push origin your-feature-branch
```

Go to the original repository's page and click the "Pull Request" button.
Fill in a clear title and description for your pull request, detailing:
 - The problem you're solving or feature you're adding, mentioning the issue number (e.g. Fixing issue #344).
 - How you’ve tested your changes.

### Step 6: Wait for review

One of the maintainers will review your pull request. Please be patient, as it may take some time. Be prepared to make further changes if requested.

## Guidelines

### Code style
 - Follow the conventions used in the existing codebase.
 - Keep your code clean and well-documented.
 - Write descriptive commit messages that explain your changes.

### Bug Reports and feature requests
If you're not ready to contribute code but have identified a bug or have a feature idea, feel free to open an issue on the issues page. Make sure to provide as much detail as possible, including steps to reproduce the bug or a clear description of the feature. There are some issue templates that you can use to make this process easier.

# Documentation

What follows is a quick and dirty documentation so that you can more quickly get up to speed with the project and find the relevant code for your desired changes.

## Overview

The code is split up into modules. Every module is concerned, in general, with one part of the pipeline, going from front-end to back-end. Front-end in this case means everything to do with the user interface and parsing of the code: basically what a user sees when using the application. So command line arguments is one part, the REPL (*R*ead, *E*valuate, *P*rint, *L*oop) shell is another, and the code parsing is the last. The back-end is the emulator itself, so everything to do with executing the code and returning the result, organizing the memory, keeping track of register values and the instruction set. Keep in mind that when adding a missing instruction for example, we will work on both of these parts.

## Front-end

### Command-line arguments
The first step is parsing and understanding the command-line arguments. These are the options/parameters you can set when starting the application. You can see the available options using the '-h' or '--help' flag. The heavy lifting here is done by a library called opt-parse. All we need to do is tell it what arguments we want the app to have, what type they are and if they're optional or not. You can see these defined in `Options.hs`. The library will parse them for us and put them in an algebraic data type called `data Options`, also defined in the same file. This all happens in the background though, so no need to worry about how it works exactly. In the `app/Main.hs` there is a function with the signature `main :: IO ()`. As you may have guessed, this is the entry point of our app. What it does is it takes those arguments and feeds them through the opt-parse. Based on what we get back, the library will call `entryFunction`. It deconstructs the `Options` data structure we discussed earlier and chooses one of the candidates based on the values inside. If the user requested the `-v` option, it doesn't matter what the other args were, we just print the version and some other information and exit. Similarly we handle the other cases. Haskell's pattern matching makes this all easy. From there, you can follow the calls for every case, see what functions are called and what they do. The code is mostly commented (thought it could be better, you could help comment more if you want) and the names are pretty self-explanatory.

### REPL

The REPL is the interactive part of the application. It is called when the program is launched with the `-i` or `--interactive` flag along with an assembly file. It supports user commands, stepping through the code line by line, printing some values, etc. Of course, this is all contained within `Repl.hs`. To be clear, the REPL is considered only with the commands that a user can supply in an interactive session, like `step`, `run`, etc. **NOT** with assembly instructions. The parsing for those is explained in the next section.

If you look in `Main.hs`, a call to `replLoop` is made in the case outlined above. What that function does is it starts an infinite loop. Every loop, the program asks for one of the possible commands that the user can use, then tries to match them with a very simple parser, and then tries its best to execute that command. After that, the cycle continues, either until the user types `exit`, presses Control-D or, God forbid, encounters an error.

Function names that have to do with parsing (understanding) the command usually end with `-Parser`. The `dispatcher` function uses pattern-matching to select the correct course of action, be it advancing the emulator, printing some part of its state, or exiting the application. 

In general, the parsing code is more esoteric and harder to understand. If you do want to add a new command, just copy and paste the parser for a command that already has the same form as the one that you want to implement, don't start from scratch. If you feel comfortable working with monads, functors and the utilities that MegaParsec puts at our disposal, go ahead.

### Code parsing

Similarly, the code for parsing the assembly file is also esoteric and looks like dark magic. Again, if you want to add a missing instruction, look for an instruction that takes the same types of arguments and copy what that one does. For example, say we want to add the instruction `ONES`, that takes a register as an argument and sets all the bits in that register to 1 (don't make a pull request for this, it is a fictitious instruction). We would now like to find another instruction that also takes only 1 register as an argument. We find that the `CLR` instruction is implemented already. Therefore, we look in the `Parser.hs` file and we see this:

```haskell
pCLR :: Parser Instruction
pCLR = do
    cistring "CLR" >> (space1 <|> eof)
    CLR <$> pRegister
```

Therefore we copy-paste it and replace the names to fit the new instruction:

```haskell
pONES :: Parser Instruction
pONES = do
    cistring "ONES" >> (space1 <|> eof)
    ONES <$> pRegister
```

This means that the "template" for such an instruction is the string "ONES", case insensitive (this is what `cistring` means), followed by a space or the end-of-file character. If those requirements are fulfilled, we return a `ONES` along with invoking the parser for a register, which could be R0-R31, XL, XH, etc. You can go look at the function `pRegister` to see what it matches.

We then need to add this parser to the list of parsers that Megaparsec will try to use to match a given instruction, so we will add it in the big list at the end of the file, in the `instructionParser` function. You will also need to create the `Instruction` variant for it, because as you can see, the parses finishes by returning a `ONES` (the `Instruction` algebraic type is defined in the `Instructions.hs` file). However, this already means we are done with the front-end part and you can move to the back-end to finish setting up this new instruction.

## Back-end

The back-end is the most beautiful and simultaneously the most complex part of this machine. It goes through every instruction, line by line, executes it and updates the Emulator state, then moves on to the next instruction, and so on, until there are no more instructions.

It is so complex in fact, it got its own folder, called `Emulator`. Inside, there are more files:
1. `Core.hs` - contains the core functionality of the emulator, mainly the 'public' functions that are going to be called in `src/REPL.hs`, `src/Emulator.hs` and `app/Main.hs`.
2. `Instructions.hs` - contains the algebraic data type for the instructions, so a data type that can hold all possible types of an assembly instruction, along with their arguments. Therein you can also find a function that treats each instruction. You will probably notice that although these functions may take different types and number of arguments, the return type for all of them is the same. They all take in a mutable vector of the registers, a mutable vector of the memory (each byte is an element in these vectors), the status flags as they were before the instruction was executed, the value of the stack pointer register, and then 0 or more arguments depending on the instruction, usually a register or a literal. However, they all return an ST monad (ignore that part for now) containing the new status flags (as they are after the instruction is done executing), a relative jump in bytes and the updated stack pointer. There's no need to return an updated version of the register or memory vectors, as they're mutable and whatever modifications we make in those will persist even though we don't return them. This may seem to go against the Haskell way of doing things, and it seems more imperative, but emulation in itself is an imperative process, so this makes sense and is more efficient compared to the alternative: copying the memory vector (which can be several kilobytes) again and again after every instruction is costly. Again, don't worry too much about details like the relative jump. Unless you implement an instruction that jumps or branches, the relative jump will always be 0, which just means that whenever this instruction is done executing, proceed to the next one in line, don't skip anything.
3. `State.hs` - here are the types for everything that concerns the state of the emulator. Stuff like registers, memory, status flags (overflow flag, zero flag, etc.), the stack-pointer register are all defined here, along with the program memory (the memory that keeps all instructions that we loaded, so our program). Also here are defined some helper functions to be used by the functions in `Instructions.hs` to modify these values. Always use the functions defined here instead of DIY-ing, so the behaviour stays consistent across all instructions.
4. `Utils.hs` - utility functions. Some functions here would be better placed elsewhere. But mainly it contains some pretty-printing stuff for the REPL, and the functions used to transform labels (stuff like `loop:` in the assembly code) to actual addresses in program memory, so we know where to jump/branch.