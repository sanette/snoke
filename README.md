# Snóke, old snake game with new ideas

![title](snoke_title.png)

## The rules of the game

The game has several levels to complete. The goal is simple: find the
snake **shed skin**, eat the exact amount of **fruit** to reach the size of
that skin, and **cover** it precisely.

* You **lose** if you eat to much, bumb into the area border, or into
  yourself

* **Bananas** make you move faster (except when you reach your target
  size)

* You get **bonus points** if you eat the next fruit with an *optimal path*!

## Screenshots

![game](snoke_game.png)

## Video

https://youtu.be/h1MC9-xDKFA

## Install

Currently, you have to install Snóke as an opam package. Either
```
opam pin add https://github.com/sanette/snoke.git
opam install snoke
```

Or, download or clone this repo, `cd` into the `snoke` dir, and
```
opam install .
```

Then you should be able to launch the game with
```
snoke
```

Alternatively, instead of the install step, you can directly run the game with
```
dune exec src/snoke.exe
```

### ???

Yes, I know... I'm trying to learn how to distribute a binary package
for those who don't want to install the whole `ocaml` stack!

## How did you make such a wonderful and entertaining game?

I came across this
[nice blog article](http://decapode314.free.fr/re/tut/ocaml-re-tut.html)
by Florent Monnier and said to myself: why not do this with
[Bogue](http://sanette.github.io/bogue/Principles.html)?

You can read [here](https://github.com/sanette/snake-bogue) how it all
started.

## TODO

* More levels
* High-scores list
* More fruit with effects
* Background music(?)
