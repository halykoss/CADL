# CADL
My bachelor thesis: a compiler from Datalog rules to OCaml type checker (standard or incremental) 

## Table of contents
* [General info](#general-info)
* [Requirements](#requirements)
* [Setup](#setup)
* [How to use it?](#how-to-use)
* [License](#license)

## General info
The aim of CADL is to simplify the writing of a type checker (standard or incremental). You can input Datalog rules and CADL will output an OCaml type checker. 
	
## Requirements
CADL needs:
* OCaml : 4.08.1
* Dune : 2.7.1
* OUnit2 : 2.2.3
	
## Setup
To run this project you need to install:

```
apt install ocaml-nox # If you don't want X11 support.
apt install ocaml
add-apt-repository ppa:avsm/ppa
apt update
apt install opam
$ opam install dune
$ opam install ounit2
$ make
```

To clean the project tree:

```
$ make clean
```

## How to use it?

You can generate a standard type checker with:

```
$ ./cadl <input-file>
```

examples for **\<input-file\>** are _samples/input.pl_ and _samples/spi.pl_.

The compiled code will be located in '__generated.ml__' file, inside '__generated/__' module.

You can run your tests with:

```
$ ./cadl <input-file> -t <path-to-test-file-ml>
```

If you want to generate a module for [Incremental Type Checking of MinCaml](https://github.com/mcaos/incremental-mincaml), you need to run:

```
$ ./cadl <input-file> -i
```

examples of working CADL generated module are [Pierce](https://github.com/freek9807/CADL/tree/incrementalizer-pierce) and [Spi calculus](https://github.com/freek9807/CADL/tree/incrementalizer-spi-calculus).
## License

[GNU GENERAL PUBLIC LICENSE](https://github.com/freek9807/TSP-DP-PARALLEL/blob/master/LICENSE) © [Federico Pennino](mailto:federico@freek.io?subject=[GitHub]%20TSP%20CPP)
