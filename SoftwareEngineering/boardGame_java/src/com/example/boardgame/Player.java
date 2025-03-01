package com.example.boardgame;

public class Player {
    private Colour colour;
    private int stonesLeft;

    public Player(Colour colour, int initialStones) {
        this.colour = colour;
        this.stonesLeft = initialStones;
    }

    public Colour getColour() {
        return colour;
    }

    public void useStone() {
        if (stonesLeft > 0) {
            stonesLeft--;
        } else {
            throw new IllegalStateException("No stones left to use.");
        }
    }

    public int getStonesLeft() {
        return stonesLeft;
    }

    public int getRemainingStones() {
        return stonesLeft;
    }
}
