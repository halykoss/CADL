# CADL
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
apt install ocaml-nox # If you don't want X11 support.
apt install ocaml
add-apt-repository ppa:avsm/ppa
apt update
apt install opam
$ opam install dune
$ opam install ounit2
$ make
```

Per ripulire l'albero del progetto:

```
$ make clean
```

## How to use it?

Per poter generare un type checker standard il comando è:

```
$ ./cadl <input-file>
```

esempio di **\<input-file\>** è _samples/input.pl_.

Verrà quindi generata una cartella '__generated/__' dentro cui '__generated.ml__' conterrà il codice compilato.

Per eseguire una batteria di test basta eseguire il seguente comando:

```
$ ./cadl <input-file> -t <path-a-file-ml-di-test>
```

Se, invece, si volesse generare il modulo da integrare con [Incremental Type Checking of MinCaml](https://github.com/mcaos/incremental-mincaml) bisogna eseguire:

```
$ ./cadl <input-file> -i
```

Come nel caso precedente il modulo si troverà in '__generated/generated.ml__', basterà poi clonare [Incremental Type Checking of MinCaml](https://github.com/mcaos/incremental-mincaml) e sostiture _src/fun/langspec/funSpecification.ml_ con __generated/generated.ml__ (dopo averlo ovviamente rinominato in _funSpecification.ml_). All'interno dei sub-module _incrementalizer/pierce_ e _incrementalizer/spi-calculus_ vi sono due implementazione già funzionanti (il codice compilato è già all'interno del sottomodulo). Per eseguire una delle due basta entrare in una delle due cartelle ed eseguire 
```
$ make
$ main.native <file-1> <file-2>
```
I file di test si trovano in _src/fun/examples/_ dentro i sottomoduli.
Se si volesse testare un type checker standard, basta eseguire 

```
$ ./cadl samples/input.pl
```

si otterrà un type checker per un linguaggio di programmazione che permette di usare funzioni, binding di variabili e comuni strutture dati come tuple e liste. Per eseguire automaticamente una batteria di test su questo type checker basta eseguire 
```
$ ./cadl samples/input.pl -t test/input.ml
```
modificando _test/input.ml_ si possono aggiungere ulteriori programmi di test.

Nel caso in cui si volesse testare un'implementazione con parser e lexer basta eseguire il comando 

```
$ ./cadl samples/input.pl
```

e copiare la cartella _generated/_ ottenuta in un clone di questa [repo](https://github.com/freek9807/CADL/tree/standard-pierce).
## License

[GNU GENERAL PUBLIC LICENSE](https://github.com/freek9807/TSP-DP-PARALLEL/blob/master/LICENSE) © [Federico Pennino](mailto:federico@freek.io?subject=[GitHub]%20TSP%20CPP)
