# Rilouworld

Play, Create and Share in the Rilouworld!

Rilouworld is a decentralized game universe in which players can explore worlds made by others.

## Why?

You like playing games, but most of all, you like sharing what you do in games with others ? This project is for you.
The goal is to create a shared plateform for creating small "worlds", play them with your friends, and share them with the community.

Rilouworld started as a game engine for a multiplayer RPG/plateform game, and evolved into the idea that anyone would be allowed to modify how the game works. From the mechanics, to monsters, quests and magical spells, I wanted to give anyone the power to make the game their own.

## How does it work?

~~You can install Rilouworld, start the program and connect to the main Rilouworld server. It will list all the *worlds* you can enter. Jut choose one and start playing!~~ (not ready yet)

## Can I make my own world?

Of course! You need to create a new world file, and use Rilouworld's Quest language to describe places, monsters, objects, quests...etc

It looks like this (work in progress):
```
#lang rilouworld/quest
(from spacecat use player troll star)
(world
  (name "Spacecat")
  (resources
    (animation spacecat-anim "spacecat/spacecat.png" (size 34 21) (durations 4 4 4 4 4 4))
    (image space-bg-img "spacecat/space-bg.png")
    (image bg-star-img "spacecat/bg-star.png")
    (image star-img "spacecat/star.png" (rect 0 0 10 10))
    (image troll-img "spacecat/troll.png" (rect 0 0 10 10)))
  (zones
    (zone space
      (name "Space")
      (rectangles (rect 0 0 32 24))
      (actors
        ;; the space background scrolling behind the scene
        (scrolling-bg (image space-bg-img) (direction left) (speed 0.2))

        ;; an area that spawns background stars
        (spawner
          (rect 0 0 512 384)
          (actors
            [(freq .2) (particle (image bg-star-img)
                                 (pos <x> <y>)
                                 (direction left)
                                 (speed .30))]))

        ;; an area that spawns trolls and stars
        (spawner
          (rect 512 0 16 384)
          (actors
            [(freq .5) (troll (image troll-img) (pos <x> <y>))]
            [(freq .01) (star (image star-img) (pos <x> <y>))]))

        ;; a test star
        (star (image star-img) (pos 100. 100.))

        ;; the player
        (player (image spacecat-anim) (pos 50. 256.))

        ))))
```

This file creates a world in which a colorfoul cat runs infinitely through space and catches stars.

## Ok, I made my game, how do I share it?

TBD

## Complete guides and documentation

Visit the [complete Rilouworld documentation here](https://docs.racket-lang.org/rilouworld).

## Under the hood

Rilouworld is powered by [Racket](https://racket-lang.org), the language-oriented programming language. It allows us to create Domain Specific Languages for video games.

## License

Rilouworld is distributed under the terms of the GNU General Public License version 3.
Copyright © 2018 - Jérôme Martin

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program. If not, see <http://www.gnu.org/licenses/>.
