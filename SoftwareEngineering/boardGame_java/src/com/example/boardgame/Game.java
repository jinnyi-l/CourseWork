package com.example.boardgame;

import java.util.List;

public class Game {
    private static final int BOARD_SIZE = 5;
    private Grid grid;
    private Player player1;
    private Player player2;
    private Player currentPlayer;
    private boolean gameOver;

    public Game() {
        grid = new Grid();
        player1 = new Player(Colour.BLACK, 21);
        player2 = new Player(Colour.WHITE, 21);
        currentPlayer = player1;
        gameOver = false;
    }

    public Grid getGrid() {
        return grid;
    }

    public Player getCurrentPlayer() {
        return currentPlayer;
    }

    public boolean isGameOver() {
        return gameOver;
    }

    public Player getWinner() {
        int player1Score = 0;
        int player2Score = 0;

        for (int i = 0; i < BOARD_SIZE; i++) {
            for (int j = 0; j < BOARD_SIZE; j++) {
                TileStack stack = grid.getTileStack(i, j);
                if (!stack.isEmpty()) {
                    Stone topStone = stack.peekTopStone();
                    if (topStone != null && topStone.getOrientation() == Orientation.FLAT) {


                        if (topStone.getColour() == Colour.WHITE) {
                            player1Score++;
                        } else if (topStone.getColour() == Colour.BLACK) {
                            player2Score++;
                        }
                    }
                }
            }
        }

        if (player1Score > player2Score) {
            return player1;
        } else if (player2Score > player1Score) {
            return player2;
        } else {
            return null;
        }
    }

    public void makeMove(int x, int y, Orientation orientation) {
        TileStack stack = grid.getTileStack(x, y);

        Stone stone = new Stone(orientation, currentPlayer.getColour());
        stack.addStone(stone, currentPlayer);
        currentPlayer.useStone();

        if (checkVictoryByLineFormation() || checkVictoryByNoMovesLeft() || checkVictoryByNoStonesLeft()) {
            gameOver = true;
        } else {
            switchPlayer();
        }
    }


    public void moveStack(int x, int y, int numberOfStones, String direction) {
        TileStack stack = grid.getTileStack(x, y);
        if (stack.isEmpty()) {
            throw new IllegalStateException("Cannot move from an empty stack.");
        }
        Stone topStone = stack.peekTopStone();

        // Check if the top stone is controlled by the current player
        if (topStone.getColour() != currentPlayer.getColour()) {
            throw new IllegalStateException("Cannot move a stack you don't control.");
        }

        if (numberOfStones >= stack.getStackSize()) {
            throw new IllegalStateException("Must leave at least one stone.");
        }

        List<Stone> pickedUpStones = stack.pickUpStones(numberOfStones);

        int newX = x, newY = y;
        switch (direction.toLowerCase()) {
            case "up":
                newX = x - 1;
                break;
            case "down":
                newX = x + 1;
                break;
            case "left":
                newY = y - 1;
                break;
            case "right":
                newY = y + 1;
                break;
            default:
                throw new IllegalStateException("Invalid direction.");
        }

        if (newX < 0 || newY < 0 || newX >= BOARD_SIZE || newY >= BOARD_SIZE) {
            throw new IllegalStateException("Move out of bounds.");
        }

        TileStack destinationStack = grid.getTileStack(newX, newY);
        destinationStack.placeStones(pickedUpStones);
        if (checkVictoryByLineFormation() || checkVictoryByNoMovesLeft() || checkVictoryByNoStonesLeft()) {
            gameOver = true;
        } else {
            switchPlayer();
        }
    }



    private void switchPlayer() {
        currentPlayer = (currentPlayer == player1) ? player2 : player1;
    }

    private boolean checkVictoryByLineFormation() {
        // Check rows
        for (int i = 0; i < BOARD_SIZE; i++) {
            boolean rowWin = true;
            Stone firstStone = grid.getTileStack(i, 0).peekTopStone();
            if (firstStone == null) continue;
            Colour colour = firstStone.getColour();

            for (int j = 1; j < BOARD_SIZE; j++) {
                Stone stone = grid.getTileStack(i, j).peekTopStone();
                if (stone == null || stone.getColour() != colour) {
                    rowWin = false;
                    break;
                }
            }
            if (rowWin) return true;
        }

        for (int i = 0; i < BOARD_SIZE; i++) {
            boolean colWin = true;
            Stone firstStone = grid.getTileStack(0, i).peekTopStone();
            if (firstStone == null) continue;
            Colour colour = firstStone.getColour();

            for (int j = 1; j < BOARD_SIZE; j++) {
                Stone stone = grid.getTileStack(j, i).peekTopStone();
                if (stone == null || stone.getColour() != colour) {
                    colWin = false;
                    break;
                }
            }
            if (colWin) return true;
        }

        return false;
    }


    private boolean checkVictoryByNoMovesLeft() {

        for (int i = 0; i < BOARD_SIZE; i++) {
            for (int j = 0; j < BOARD_SIZE; j++) {
                TileStack stack = grid.getTileStack(i, j);
                if (stack != null && !stack.isStandingBlock() && stack.getStackSize() < BOARD_SIZE) {
                    return false;
                }
            }
        }
        return true;
    }


    private boolean checkVictoryByNoStonesLeft() {
        return player1.getStonesLeft() == 0 && player2.getStonesLeft() == 0;
    }


    public Player getPlayer2() {
        return player2;
    }

    public Player getPlayer1() {
        return player1;
    }

    public boolean canCurrentPlayerAddStone() {
        return currentPlayer.getRemainingStones() > 0;
    }

}
