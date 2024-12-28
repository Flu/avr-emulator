# Revision history for avr-emulator

## 0.3.2.0 -- 2024-12-28

* The general purpose registers are now mapped to SRAM, so they should stay in sync.
* Fix critical bug in ADD and ADC instructions, now the carry flag is set properly.
* Add memory inspection tools in the REPL with the `memory` command.
* Implement parser support for XL, XH, YL, YH, ZL, ZH.
* More example files to test the emulator with.

## 0.3.1.0 -- 2024-12-23

* Implement Y and Z registers access for the LD and ST command.
* Fix ADIW instruction syntax.

## 0.3.0.0 -- 2024-12-23

* Add a REPL for step-by-step execution.
* Convert program array to a more efficient data structure for faster access.
* Reorganize code into smaller modules - better concern separation.

## 0.2.1.1 -- 2024-12-9

* Add command line options for configuring the emulator.
* Pretty printing the registers, memory and program memory
* Fix bug where asking for `-v` but no file it would complain about missing file and ignore the `-v` flag.
* Add most of the instructions supported by the Arduino Uno.

## 0.1.0.0 -- 2024-09-22

* First version. Released on an unsuspecting world.
* Basic emulator functionality.
