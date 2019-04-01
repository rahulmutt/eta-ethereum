[![Build Status](https://circleci.com/gh/rahulmutt/eta-ethereum.svg?style=shield)](https://circleci.com/gh/rahulmutt/eta-ethereum)

# Eta Ethereum

This is a work-in-progress implementation of a full node Ethereum client that takes inspiration from the top two Ethereum clients: [go-ethereum](https://github.com/ethereum/go-ethereum) and [parity](https://github.com/paritytech/parity-ethereum).

## Goals

The goal is to eventually be a fully functional, competitive Ethereum implementation and serve as platform for quickly prototyping new ideas for Ethereum 2.0 and beyond using a strongly typed, pure functional language that runs on the JVM.

## Building

### Prerequisites

- JDK 7+
- Eta v0.8+

### Instructions

You can run the following command to build all the executables and their dependencies:

```
etlas build all
```

### Testing


You can run the following command to run the tests for all the packages:

```
etlas test all
```


## License

The eta-ethereum libraries (i.e. all code outside of the `tools` directory) is licensed under the
[GNU Lesser General Public License v3.0](https://www.gnu.org/licenses/lgpl-3.0.en.html), also
included in our repository in the `COPYING.LESSER` file.

The eta-ethereum binaries (i.e. all code inside of the `tools` directory) is licensed under the
[GNU General Public License v3.0](https://www.gnu.org/licenses/gpl-3.0.en.html), also included
in our repository in the `COPYING` file.