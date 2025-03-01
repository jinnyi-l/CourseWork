package com.example.boardgame;

public class Grid {
    private TileStack[][] grid;
    private static final int BOARD_SIZE = 5;

    public Grid() {
        //initialization each element in 2d array with TileStack object
        grid = new TileStack[BOARD_SIZE][BOARD_SIZE];
        for (int i = 0; i < BOARD_SIZE; i++) {
            for (int j = 0; j < BOARD_SIZE; j++) {
                grid[i][j] = new TileStack();
            }
        }
    }

    public TileStack getTileStack(int x, int y) {
        if (x >= 0 && x < BOARD_SIZE && y >= 0 && y < BOARD_SIZE) {
            return grid[x][y];
        }
        return null;
    }

    public boolean isValidMove(int x, int y) {
        TileStack stack = getTileStack(x, y);

        //allow moving stack: stack exist and the top is not standing stone, return boolean value
        return stack != null && !stack.isStandingBlock();
    }
}
