package com.example.boardgame;

public class Main {
    public static void main(String[] args) {

        /*
        Colour and Stone class should be enum type; player to keep track of the stones used so far
        (By default we start at the black stone)

        Represent the tile stack at each position as ArrayList<>(),
        TileStack to tell if there has POSSIBILITY to move stack or simply adding stone
        Also, allow the implementation of moving/adding

        Initialize each element in grid as an object of tile stack. grid to return the stack
        at certain position and if valid move


        Game class:
        has function makeMove and moveStack
        also check for wining condition after every move by each player,
        check winning condition when play to win or has to end

        Jhavafx for GUI

         */
        Game game = new Game();
    }
}
