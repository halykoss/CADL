# CADL (Nome provvisorio)
Compila regole Datalog in Ocaml

## Table of contents
* [General info](#general-info)
* [Requirements](#requirements)
* [Setup](#setup)
* [How to use it?](#how-to-use)
* [License](#license)

## General info
Lorem ipsum dolor sit amet, consectetur adipiscing elit. Fusce nec dapibus felis. Mauris enim nisl, dictum vel libero nec, ullamcorper auctor nibh. Pellentesque id leo sit amet massa tincidunt ultricies.
	
## Requirements
Progetto creato con:
* OCaml : 4.08.1
* Dune : 2.7.1
* OUnit2 : 2.2.3
	
## Setup
Per eseguire il progetto:

```
$ opam install dune
$ opam install ounit2
$ make
```

Per ripulire l'albero del progetto:

```
$ make clean
```

## How to use it?

Per poter generare un semplice type checker il comando è:

```
$ ./cadl <input-file>
```

esempio di **\<input-file\>** è _samples/input.pl_.

Verrà quindi generato un modulo '__generated/__' dentro cui '__generated.ml__' conterrà il codice compilato.

Per eseguire una batteria di test basta eseguire il seguente comando:

```
$ ./cadl <input-file> -t <path-a-file-ml-di-test>
```

Se, invece, si volesse generare il modulo da integrare con [Incremental Type Checking of MinCaml](https://github.com/mcaos/incremental-mincaml) eseguire:

```
$ ./cadl <input-file> -i
```

Come nel caso precedente il modulo si troverà in '__generated/__'.

## License

[GNU GENERAL PUBLIC LICENSE](https://github.com/freek9807/TSP-DP-PARALLEL/blob/master/LICENSE) © [Federico Pennino](mailto:federico@freek.io?subject=[GitHub]%20TSP%20CPP)