# Gridrunner by Jeff Minter (NES)
<img src="https://user-images.githubusercontent.com/58846/236032019-893943fa-659e-4ac0-9242-4ed6b0fe3a4f.gif" height=300><img src="https://user-images.githubusercontent.com/58846/236034673-b2414ffb-12a7-4650-966b-c276b5b374a7.jpg" height=300>

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
You have less fine-grained control than on the C64 version as this version uses the gamepad only. Apart from
moving the cursor around and firing, the other buttons on your gamepad will cycle through the available
symmetries and preset patterns. 

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
