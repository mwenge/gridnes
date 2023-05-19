# Gridrunner by Jeff Minter (NES)
<img width="460" src="https://github.com/mwenge/gridnes/assets/58846/c9c73f89-cf4d-430d-a427-53450b8bef8d">

Jeff Minter never ported his game [Gridrunner] to the Nintendo Entertainment system. So I did. Maybe I don't know what I'm doing.

<!-- vim-markdown-toc GFM -->

* [Play](#play)
  * [Controls](#controls)
* [Build](#build)
* [About](#about)

<!-- vim-markdown-toc -->

## Play
On Ubuntu you can install [FCEUX], the NES emulator, as follows:
```
sudo apt install fceux
```

Once you have that installed, you can [download the game](https://github.com/mwenge/psynes/raw/master/bin/gridrunner.nes) and play it:

```
fceux gridrunner.nes
```

### Controls
On FCEUX, you can use F to fire and the Arrow keys to move.

## Build
On Ubuntu you can install the build dependencies as follows:
```
sudo apt install cc65 fceux python3
```

Then you can compile and run:

```sh
$ make
```

## About
Made out of curiosity as part of the [Gridrunner](https://github.com/mwenge/gridrunner) project.
This [example project](https://github.com/bbbradsmith/NES-ca65-example/) was a big help in getting started.
Let's face it: it's slow. I'm still thinking of what I can do about that.


[cc65]: https://cc65.github.io/
[FCEUX]: https://fceux.com/
[llamaSource]: https://en.wikipedia.org/wiki/Trip-a-Tron
[Gridrunner]: https://en.wikipedia.org/wiki/Gridrunner_(light_synthesizer)
