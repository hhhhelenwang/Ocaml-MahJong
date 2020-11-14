# Mahjong - CS 3110 Final Project

##Running the Game
- From the terminal, change directory into the folder 'Mahjong' and type the command
```
make play
```
A message should appear in your terminal. Follow the instruction and type
```
start
```
to play the game.

## Game Description

### Background

Riichi Mahjong (Japanese: 麻雀, 麻将 or マージャン; mājan) is a traditional four-player board game in Asia. People play this grame with their friends or family members during holidays to entertain and connect.

## Actions and Specification



### Quick Note

- **Tile**: Similar to cards in Poker. There are a total of 136 tiles.

- **Wall tile**: Tiles that have not been drawn.

- **Hand tile**: Tiles held by players.

- **Discard tile**: Tiles discarded by players.

- **Yaku(役 or 役)**: Winning pattern. A player wins when their hand tiles form a winning pattern.

- **Chii (吃 or チー)**: Players can make an open-sequential-tiles, a sequence (3 consecutive tiles in the same suit), by calling “chii” using a tile discarded by the left player, who is the prior player.  This means every player can claim the left player’s discarded tile to make a sequence.

- **Riichi (立直 or リーチ)**: A player can declare Riichi if and only if the player needs exactly one more tile to have a winning pattern and has not Chii-ed in the game. After declaring Riichi, the player can only discard the tile they draw on that round. 
