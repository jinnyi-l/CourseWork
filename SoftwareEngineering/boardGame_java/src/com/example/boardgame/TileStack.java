package com.example.boardgame;
import java.util.*;

public class TileStack {

    private List<Stone> stones;

    public TileStack() {
        // ArrayList to store the stone type
        stones = new ArrayList<>();
    }

    public void addStone(Stone stone, Player currentPlayer) {
        System.out.println("Attempting to add stone. Stack size: " + stones.size());
        if (isStandingBlock()) {
            Stone topStone = peekTopStone();
            System.out.println("Top stone is standing. Top stone color: " + topStone.getColour() + ", Current player color: " + currentPlayer.getColour());
            if (topStone != null && stones.size() > 1 && topStone.getColour() == currentPlayer.getColour()) {
                // Allow movement if the stack has more than one stone and the top stone is the current player's color
                stones.add(stone);
                System.out.println("Stone added on standing block.");
            } else {
                throw new IllegalStateException("Cannot place stone on a standing stone.");
            }
        } else {
            stones.add(stone);
            System.out.println("Stone added normally.");
        }
    }

    public Stone peekTopStone() {
        if (stones.isEmpty()) {
            return null;
        }
        return stones.get(stones.size() - 1);
    }

    public int getStackSize() {
        return stones.size();
    }

    public boolean isEmpty() {
        return stones.isEmpty();
    }

    public Stone getStone(int index) {
        if (index >= 0 && index < stones.size()) {
            return stones.get(index);
        }
        return null;
    }

    // Helper methods

    // Check if the top stone is a standing stone
    public boolean isStandingBlock() {
        Stone topStone = peekTopStone();
        return topStone != null && topStone.getOrientation() == Orientation.STANDING;
    }

    // Pick up a number of stones from the top
    public List<Stone> pickUpStones(int number) {
        List<Stone> pickedUp = new ArrayList<>(); // Subarray of tilestack array
        for (int i = 0; i < number && !stones.isEmpty(); i++) {
            pickedUp.add(stones.remove(stones.size() - 1));
        }
        return pickedUp;
    }

    // Place stones on the stack (for moving stacks)
    public void placeStones(List<Stone> newStones) {
        System.out.println("Placing stones. Number of stones: " + newStones.size());
        // Add the stones in the order they appear in the list
        for (int i = newStones.size() - 1; i >= 0; i--) {
            stones.add(newStones.get(i));
            System.out.println("Stone moved to new location.");
        }
    }


}
