# AVR Emulator
[![build](https://github.com/Flu/avr-emulator/actions/workflows/build.yml/badge.svg)](https://github.com/Flu/avr-emulator/actions/workflows/build.yml)
[![test](https://github.com/Flu/avr-emulator/actions/workflows/test.yml/badge.svg)](https://github.com/Flu/avr-emulator/actions/workflows/test.yml)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

This project is an AVR emulator written in Haskell, utilizing CABAL for building and managing dependencies. The emulator simulates a basic AVR microcontroller environment with a 2KB memory space starting from address `0x0000`. The emulator currently supports a subset of AVR instructions, and you can load and execute assembly programs to test its functionality.

## How to Build and run

### Prerequisites
You need to have `Cabal` and `GHC` installed to build and run the emulator. We recommend using `ghcup` to install these tools. To install `ghcup`, follow the instructions at [https://www.haskell.org/ghcup/](https://www.haskell.org/ghcup/).

### Build the emulator

1. Clone the repository and navigate to the project folder.
2. Run the following command to build the project:

```bash
cabal build
```

3. At this point, if you want to be able to run the emulator from anywhere on your computer, you can run this command inside the project folder:

```bash
cabal install exe:avr-emulator --overwrite-policy=always
```

Verify the installation with:
```bash
avr-emulator --version
```

Alternatively you can just download the latest release binary for your operating system and run it that way.

### Test the emulator

1. Clone the repository and navigate to the project folder.
2. Run the following command to run the unit tests for the project:
```bash
cabal test all
```

This will run all the test suites and will tell you if anything failed. In general, any commit on the `main` branch should pass all tests. If yours doesn't, open up an issue.

### Run the emulator

```bash
cabal run exes -- /path/to/assembly/file.asm -d -m 500
```

This tells Cabal that you want to run the application. Here there are also some command line options that you can use to change the emulator's parameters. For example, `-d` dumps and prints the whole memory of the emulator, besides the registers and the flags. `-m 500` specifies the amount of memory to allocate for the emulator in bytes. There are more options, which you can see by using `-h` or `--help`.

### Ending the program

The emulator will automatically end when there are no more instructions to execute.

### Interactive session

If you prefer to use the emulator interactively rather than letting it execute your porgram all at once (for debugging purposes for example), you might be interested in the REPL. To bring it up for an assembly file:
```bash
cabal run exes -- /path/to/assembly/file.asm -i
```

The `-i` stands for interactive and will open up a prompt if the file was succesfully parsed. In the prompt, you can type `help` to see what commands are available. You can inspect the register values, the memory, the flags, step one instruction at a time (or multiple), run the program until the function you are in returns and more.

### Test files

You can find test assembly programs in the `test_files/` directory in the root of the project. These files provide examples of how an AVR assembly program is structured and can be used to verify the emulator's functionality.

## Features
- **Memory**: 2KB by default, starting from `0x0000`.
- **Stack**: Starts from the last memory address and grows towards lower addresses.
- **Supported Instructions**:
  - `ADC` – Add with carry between two registers.
  - `ADD` – Add two register values and store the result in one of the registers.
  - `ADIW` – Add an immediate word to a register pair.
  - `AND` – Perform a bitwise AND between two registers.
  - `ANDI` – Perform a bitwise AND between a register and an immediate value.
  - `ASR` – Arithmetic shift to the right.
  - `BCLR` – Bit clears a flag in SREG given a value from 0 to 7.
  - `BLD` – Sets a bit in a registry equal to the T flag.
  - `BRCC` – Branch if carry flag is cleared.
  - `BRCS` – Branch if carry flag is set.
  - `BREQ` – Branch if equal to zero.
  - `BRGE` – Branch if greater (signed).
  - `BRHC` – Branch if half carry flag is cleared.
  - `BRHS` – Branch if half carry flag is set.
  - `BRID` – Branch if global interrupt flag is cleared.
  - `BRIE` – Branch if global interrupt flag is set.
  - `BRLO` – Branch if lower (unsigned).
  - `BRLT` – Branch if less than (signed).
  - `BRMI` – Branch if minus.
  - `BRNE` – Branch if not equal to zero.
  - `BRPL` – Branch if positive.
  - `BRSH` – Branch if same or higher (unsigned).
  - `BRTC` – Branch if T flag is cleared.
  - `BRTS` – Branch if T flag is set.
  - `BRVC` – Branch if overflow flag is cleared.
  - `BRVS` – Branch if overflow flag is set.
  - `BSET` – Sets a single flag or bit in SREG.
  - `CALL` – Direct call to a subroutine with the return address pushed on the stack.
  - `CBR` – Clears specific bits of a register based on the argument provided.
  - `CLC` – Clear the Carry flag.
  - `CLH` – Clear the Half Carry flag.
  - `CLI` – Clear the Global Interrupt flag.
  - `CLN` – Clear the Negative flag.
  - `CLR` – Sets register to `0x00`.
  - `CLS` – Clear Signed flag.
  - `CLT` – Clear T flag.
  - `CLV` – Clear Overflow flag.
  - `CLZ` – Clear Zero flag.
  - `COM` – Take one's complement of register.
  - `CP` – Compare two registers.
  - `CPC` – Compare with carry between two registers.
  - `CPI` – Compare a register with an immediate value.
  - `CPSE` – Compare and skip if equal.
  - `DEC` – Decrement a register.
  - `EOR` – Perform bitwise exclusive OR between two registers.
  - `INC` – Increment a register.
  - `JMP` – Jump to a label.
  - `LD` – Load indirect from data space using an indirect address.
  - `LDI` – Load an immediate value into a register.
  - `LDS` – Load a direct value from data space.
  - `LSL` – Logical shift left.
  - `LSR` – Logical shift right.
  - `MOV` – Move the value from one register to another.
  - `MOVW` – Move a word from a pair of registers to the other.
  - `MUL` – Multiply two registers.
  - `MULS` – Multiply signed values in two registers.
  - `NEG` – Take two's complement of register.
  - `NOP` – No operation.
  - `OR` – Perform a bitwise OR between two registers.
  - `ORI` – Perform a bitwise OR between a register and an immediate value.
  - `POP` – Pops an element from the stack into a register.
  - `PUSH` – Pushes an element from a register onto the stack.
  - `RET` – Return from subroutine.
  - `ROL` – Rotate left through carry.
  - `ROR` – Rotate right through carry.
  - `SBC` – Subtract one register value from another with carry.
  - `SBR` – Set bits in register with a mask, equivalent to ORI.
  - `SBRC` – Skip next instruction if bit in register is cleared.
  - `SBRS` – Skip next instruction if bit in register is set.
  - `SEC` – Set the Carry flag.
  - `SEH` – Set the Half Carry flag.
  - `SEI` – Set the Global Interrupt flag.
  - `SEN` – Set the Negative flag.
  - `SER` – Sets all bits in a register.
  - `SES` – Set Signed flag.
  - `SET` – Set T flag.
  - `SEV` – Set Overflow flag.
  - `SEZ` – Set Zero flag.
  - `ST` – Store indirect to data space using an indirect address.
  - `STS` – Store direct to data space.
  - `SUB` – Subtract one register value from another.
  - `SUBI` – Subtract an immediate value from a register.
  - `SWAP` – Swap nibbles of register.
  - `TST` – Test if a register is zero or negative.
  - `XCH` – Exchanges one byte indirect between the register and data space.
  
## Roadmap
  - [x] Configurable memory size through command line arguments.
  - [x] Implement in-memory stack with `PUSH` and `POP` instructions.
  - [x] `CALL` and `RET` instructions.
  - [x] Maybe (?) move to MegaParsec for input parsing.
  - [x] Implement nicer formatting for the emulator state after the program ended.
  - [x] Registers should be mapped to the first 32 memory locations.
  - [x] Fix Parser returning an error when last line of input file is an empty line or a comment.
  - [x] Implementing step by step emulation of instructions.
  - [x] Implement Parser support for lowercase instructions.
  - [x] Implementing `ADIW`.
  - [x] Unit tests for instructions.
  - [x] Better error reporting.
  - [ ] Implement limitations to instructions similar to real hardware.
  - [ ] Add more to the roadmap.
  - [ ] Buy Grolsch beer when this is all done :tada: :beer:

## How to contribute

Take a look at the `CONTRIBUTING.md` file for more instructions.
