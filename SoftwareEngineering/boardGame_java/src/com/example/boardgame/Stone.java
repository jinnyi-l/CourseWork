package com.example.boardgame;

public class Stone {

    /*
    Stones have two fields: orientation and color
    which are two Enums
     */

    private Orientation orientation;
    private  Colour colour;

    public Stone(Orientation orientation, Colour colour) {
        this.orientation = orientation;
        this.colour = colour;
    }

    public Orientation getOrientation() {
        return orientation;
    }

    public Colour getColour() {
        return colour;
    }

    public boolean isStanding() {
        return orientation == Orientation.STANDING;
    }
}

