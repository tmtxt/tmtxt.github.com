---
layout: post
showtn: yes
title: "My Battle Ship game"
description: ""
category: Misc
thumbnail: /files/2012-12-27-battle-ship-game/start.png
tags: [game, java, battleship]
---
{% include JB/setup %}

# BattleShip Game

> This is my assignment 2 in the Programming 2 course at RMIT

Battleship is a turn-based strategy game for 2 players. In this game, each
player has 5 ships placed on a grid. Player cannot see the opponent’s ships. The
game will end when one of two players has no more ships on his grid.

# Download

Here is the source of the game on Github:
[Battle Ship Game on Github](https://github.com/tmtxt/battleship)

<!-- more -->

The actual source is the BattleShip folder which is a Netbeas project. Import it
into Netbeans and have fun ;) Remember this is just for my assignment.

# How to play

### Start Game Window

![Start game](/files/2012-12-27-battle-ship-game/start.png ) 

To play, click **Play Game**.  
To quit, click **Quit**.

### Ship Selection Window

![Ship selection](/files/2012-12-27-battle-ship-game/selection.png ) 

To place ship, click on one of the five buttons on the right to select the ship.
After that, click on the grid to place the ship. Repeat for the other 4 ships.  
To select difficulty, click on the radio button on the left to select. There are
two difficulty levels.  
After finishing placing ships, click “Play Game”.  
If you do not want to select ship, just want to play game quickly, click “Play
Using Random Ship Location”. The computer will generate random location for your
ships.

### First Turn Dialog

![First turn](/files/2012-12-27-battle-ship-game/first.png )  
This dialog demonstrates which player will go first. Click **OK** to play game.

### Main game window

![Main game](/files/2012-12-27-battle-ship-game/main.png )  
The computer’s grid is on the left.  
Your grid is on the right.  
Click on the cell on the Computer’s grid to fire.  
The **Remaining Cells** labels indicate the number of cells that contain ship
each player has.  
The game ends when one of the two has no more ships.  
To play again, click **Play Again**.  
To quit game, click **Quit**.  

# Additional information

Additional information (requirement, design, diagrams,...) can be found in those
files in the source:

* COSC2082_2012B_assign2.dot
* Programming 2 – Assignment 2.pdf
* Programming 2 – Assignment 2.docx
