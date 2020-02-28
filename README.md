# AVRA-RS

*Assembler for Microchip(near Atmel) AVR microcontroller family*

AVRA-RS Rust rewrites version of AVRA (https://github.com/hsoft/avra).
I create AVRA-RS because original software has some issues and mistakes and I can't fix it.

First of all I need to say - it is not full implementation of orignal assembler and it has 
some differences front original AVRA. For example everything constructions have tests -
fully as possible.

## Differece between AVRA-RS and AVRA

AVRA-RS has incomplete implementation of directives, and not supported any extra options of
AVRA

## Build and Install

To build the `avra-rs` you can use `cargo build` and `cargo install`, or you can install
`avra-rs` from https://crates.io

## Usage

To compile source file you need to run `avra-rs` with argument `-s` for describe path to
source and optionally you can provide output path by `-o`. Other options is not supported.
Detail information of assembler will be added in near future.

## Change log

See [CHANGELOG.md](CHANGELOG.md).