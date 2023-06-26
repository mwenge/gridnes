# Gridrunner by Jeff Minter (NES)
<img width="460" src="https://github.com/mwenge/gridnes/assets/58846/26e05987-7d5c-476b-9375-a9b0b69ba2d6">

Jeff Minter never ported his game [Gridrunner] to the Nintendo Entertainment system. So I did. Maybe I don't know what I'm doing.

<!-- vim-markdown-toc GFM -->

* [Play Online](#play-online)
* [Play](#play)
  * [Controls](#controls)
* [Build](#build)
* [About](#about)
* [NES-Specific Code](#nes-specific-code)
* [Graphics](#graphics)
* [Sound](#sound)
* [Gameplay](#gameplay)
* [So how does Gridrunner work?](#so-how-does-gridrunner-work)

<!-- vim-markdown-toc -->

## Play Online
You can play it [online here](https://mwenge.github.io/gridnes).

## Play
On Ubuntu you can install [FCEUX], the NES emulator, as follows:
```
sudo apt install fceux
```

Once you have that installed, you can [download the game](https://github.com/mwenge/gridnes/raw/master/bin/gridrunner.nes) and play it:

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

### NES-Specific Code
This is mostly confined to [nes.asm](src/nes.asm). It's where I've put all the helper functions as well the boiler
plate robbed from [Brad Smith's example project](https://github.com/bbbradsmith/NES-ca65-example/).

### Graphics
The C64 is a very friendly system to develop on - you write a value to a location in memory and it magically
appears on screen. You write a value to a different location, it changes color. The NES on the other hand requires
a lot more convolutions from you. While it allows up to 64 8x8 sprites, it does not really envisage you writing
lots of characters to screen every frame - which is what we're doing here, just like the original Gridrunner. Of
course, there is a world in which I could have just used sprites instead (and maybe there will be) but for a first
pass what I have done is write as many as characters as possible to screen as I can in the small interval allowed by
the NES between each frame. This all happens in the `MainNMIInterruptHandler`, which is the routine that runs every
time the NES takes a little rest between drawing a frame on the screen. This little rest is known as the VBLANK, or
Vertical Blank. Every time Gridrunner wants to write a character to the screen, such as when moving the ship to a new
position, I instead write it to a buffer array called `NMT_UPDATE`. This is done using a few different utility 
functions, depending on the use case, but mainly `WriteCurrentCharacterToCurrentXYPosBatch` which will append updates
to the list in `NMT_UPDATE` until there are enough of them to warrant writing to the screen itself in `MainNMIInterruptHandler`.

### Sound
Crappy, almost non-existent. I stopped short of figuring out how to make sound work in NES properly.

### Gameplay
The NES version seems faster than the original Gridrunner. This is not because I'm a coding genius, it's because it's
the way it worked when I got the thing working. 

<img width="350" src="https://github.com/mwenge/gridnes/assets/58846/26e05987-7d5c-476b-9375-a9b0b69ba2d6"> <img src="https://user-images.githubusercontent.com/58846/103443482-9fb16180-4c57-11eb-9403-4968bd16287f.gif" width=400>

### So how does Gridrunner work?
I wrote up the basic workings of C64 gridrunner in this [Little Black Book](https://github.com/mwenge/llamaSource/blob/main/GridrunnerTheLittleBlackBook.md). Everything there applies to the NES version.

[cc65]: https://cc65.github.io/
[FCEUX]: https://fceux.com/
[llamaSource]: https://en.wikipedia.org/wiki/Trip-a-Tron
[Gridrunner]: https://en.wikipedia.org/wiki/Gridrunner_(light_synthesizer)
