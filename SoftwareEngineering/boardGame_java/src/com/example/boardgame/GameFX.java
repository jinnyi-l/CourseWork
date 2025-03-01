package com.example.boardgame;

import javafx.application.Application;
import javafx.geometry.Insets;
import javafx.geometry.Pos;
import javafx.scene.Scene;
import javafx.scene.canvas.Canvas;
import javafx.scene.canvas.GraphicsContext;
import javafx.scene.control.*;
import javafx.scene.layout.*;
import javafx.scene.paint.Color;
import javafx.stage.Stage;
import java.util.*;

public class GameFX extends Application {
    private static final int BOARD_SIZE = 5;
    private static final int TILE_SIZE = 100;

    private Game game;
    private Canvas[][] canvasGrid;
    private TextField coordinateInput;
    private Label currentPlayerLabel;
    private Label player1StonesLabel;
    private Label player2StonesLabel;

    @Override
    public void start(Stage primaryStage) {
        game = new Game();
        canvasGrid = new Canvas[BOARD_SIZE][BOARD_SIZE];

        BorderPane root = new BorderPane();
        GridPane boardGrid = new GridPane();

        for (int i = 0; i < BOARD_SIZE; i++) {
            for (int j = 0; j < BOARD_SIZE; j++) {
                Canvas canvas = new Canvas(TILE_SIZE, TILE_SIZE);
                canvasGrid[i][j] = canvas;
                boardGrid.add(canvas, j, i); // 注意这里是 (column, row)
            }
        }

        // Sidebar for player info and actions
        VBox sidebar = new VBox(10);
        sidebar.setPadding(new Insets(10));
        sidebar.setAlignment(Pos.TOP_CENTER);

        currentPlayerLabel = new Label("Current Player: " + game.getCurrentPlayer().getColour());
        currentPlayerLabel.setAlignment(Pos.CENTER);

        player1StonesLabel = new Label("Player 1 (White) Stones Left: " + game.getPlayer1().getRemainingStones());
        player2StonesLabel = new Label("Player 2 (Black) Stones Left: " + game.getPlayer2().getRemainingStones());

        Separator separator = new Separator(); // Line separator between sidebar and board

        sidebar.getChildren().addAll(currentPlayerLabel, player1StonesLabel, player2StonesLabel, separator);

        coordinateInput = new TextField();
        coordinateInput.setPromptText("Enter coordinates (e.g., 1,1)");
        Button placeStoneButton = new Button("Place Stone / Move Stack");
        placeStoneButton.setOnAction(e -> handlePlaceStone());

        HBox controls = new HBox(10, new Label("Coordinates:"), coordinateInput, placeStoneButton);
        controls.setAlignment(Pos.CENTER);
        sidebar.getChildren().add(controls);

        root.setCenter(boardGrid);
        root.setRight(sidebar);

        updateBoard();

        Scene scene = new Scene(root, TILE_SIZE * BOARD_SIZE + 200, TILE_SIZE * BOARD_SIZE + 100);
        primaryStage.setScene(scene);
        primaryStage.setTitle("Stacking Stones Game");
        primaryStage.show();
    }

    private void handlePlaceStone() {
        // Check if the player wants to place a new stone or move an existing one
        String[] actions = {"Place New Stone", "Move Existing Stack"};
        ChoiceDialog<String> actionDialog = new ChoiceDialog<>(actions[0], actions);
        actionDialog.setTitle("Choose Action");
        actionDialog.setHeaderText(null);
        actionDialog.setContentText("Choose an action:");

        actionDialog.showAndWait().ifPresent(action -> {
            if (action.equals("Place New Stone")) {
                placeNewStone();
            } else {
                moveExistingStack();
            }
        });
    }

    private void placeNewStone() {
        String input = coordinateInput.getText().trim();
        String[] coordinates = input.split(",");
        if (coordinates.length == 2) {
            try {
                int x = Integer.parseInt(coordinates[0].trim());
                int y = Integer.parseInt(coordinates[1].trim());

                // Validate if coordinates are within the board range
                if (x >= 0 && x < BOARD_SIZE && y >= 0 && y < BOARD_SIZE) {
                    String[] options = {"Flat", "Standing"};
                    ChoiceDialog<String> orientationDialog = new ChoiceDialog<>(options[0], options);
                    orientationDialog.setTitle("Choose Stone Orientation");
                    orientationDialog.setHeaderText(null);
                    orientationDialog.setContentText("Choose orientation:");

                    orientationDialog.showAndWait().ifPresent(orientation -> {
                        Orientation stoneOrientation = orientation.equals("Flat") ? Orientation.FLAT : Orientation.STANDING;

                        // Check if the move is valid
                        if (game.getGrid().isValidMove(x, y)) {
                            game.makeMove(x, y, stoneOrientation);
                            updateBoard(); // Update board to reflect new stone

                            if (game.isGameOver()) {
                                determineWinner();
                            }
                        } else {
                            new Alert(Alert.AlertType.ERROR, "Invalid move. Please try again.").showAndWait();
                        }
                    });
                } else {
                    new Alert(Alert.AlertType.ERROR, "Coordinates out of bounds. Please enter values between 0 and 4.").showAndWait();
                }
            } catch (NumberFormatException ex) {
                new Alert(Alert.AlertType.ERROR, "Invalid input. Please enter coordinates in the format x,y.").showAndWait();
            }
        } else {
            new Alert(Alert.AlertType.ERROR, "Invalid input. Please enter coordinates in the format x,y.").showAndWait();
        }
    }

    private void moveExistingStack() {
        String input = coordinateInput.getText().trim();
        String[] coordinates = input.split(",");
        if (coordinates.length == 2) {
            try {
                int x = Integer.parseInt(coordinates[0].trim());
                int y = Integer.parseInt(coordinates[1].trim());

                // Validate if coordinates are within the board range
                if (x >= 0 && x < BOARD_SIZE && y >= 0 && y < BOARD_SIZE) {
                    TextInputDialog numberOfStonesDialog = new TextInputDialog();
                    numberOfStonesDialog.setTitle("Move Stack");
                    numberOfStonesDialog.setHeaderText(null);
                    numberOfStonesDialog.setContentText("Enter number of stones to move:");

                    Optional<String> numberOfStonesResult = numberOfStonesDialog.showAndWait();
                    if (numberOfStonesResult.isPresent()) {
                        int numberOfStones = Integer.parseInt(numberOfStonesResult.get().trim());

                        String[] directions = {"Up", "Down", "Left", "Right"};
                        ChoiceDialog<String> directionDialog = new ChoiceDialog<>(directions[0], directions);
                        directionDialog.setTitle("Move Stack");
                        directionDialog.setHeaderText(null);
                        directionDialog.setContentText("Choose direction:");

                        directionDialog.showAndWait().ifPresent(direction -> {
                            try {
                                game.moveStack(x, y, numberOfStones, direction);
                                updateBoard();

                                if (game.isGameOver()) {
                                    determineWinner();
                                }
                            } catch (IllegalStateException e) {
                                // Display error dialog with the exception message
                                new Alert(Alert.AlertType.ERROR, e.getMessage()).showAndWait();
                            }
                        });
                    }
                } else {
                    new Alert(Alert.AlertType.ERROR, "Coordinates out of bounds. Please enter values between 0 and 4.").showAndWait();
                }
            } catch (NumberFormatException ex) {
                new Alert(Alert.AlertType.ERROR, "Invalid input. Please enter coordinates in the format x,y.").showAndWait();
            }
        } else {
            new Alert(Alert.AlertType.ERROR, "Invalid input. Please enter coordinates in the format x,y.").showAndWait();
        }
    }

    private void updateBoard() {
        for (int i = 0; i < BOARD_SIZE; i++) {
            for (int j = 0; j < BOARD_SIZE; j++) {
                GraphicsContext gc = canvasGrid[i][j].getGraphicsContext2D();
                TileStack tileStack = game.getGrid().getTileStack(i, j);

                gc.clearRect(0, 0, TILE_SIZE, TILE_SIZE); // Clear the tile

                if (!tileStack.isEmpty()) {
                    drawStack(gc, tileStack);
                }
            }
        }
        currentPlayerLabel.setText("Current Player: " + game.getCurrentPlayer().getColour());
        player1StonesLabel.setText("Player 1 (Black) Stones Left: " + game.getPlayer1().getRemainingStones());
        player2StonesLabel.setText("Player 2 (White) Stones Left: " + game.getPlayer2().getRemainingStones());
    }

    private void drawStack(GraphicsContext gc, TileStack tileStack) {
        int size = 80;
        int stackSize = tileStack.getStackSize();
        double offsetY = 0;

        for (int i = 0; i < stackSize; i++) {
            Stone stone = tileStack.getStone(i);
            int stoneSize = size - (i * 5);
            drawStone(gc, stone, stoneSize, offsetY);
            offsetY += 5;
        }
    }

    private void drawStone(GraphicsContext gc, Stone stone, int size, double offsetY) {
        Color color = stone.getColour() == Colour.WHITE ? Color.WHITE : Color.BLACK;
        gc.setFill(color);
        gc.setStroke(Color.BLACK);

        if (stone.getOrientation() == Orientation.FLAT) {
            gc.fillOval((TILE_SIZE - size) / 2, (TILE_SIZE - size) / 2 - offsetY, size, size);
            gc.strokeOval((TILE_SIZE - size) / 2, (TILE_SIZE - size) / 2 - offsetY, size, size);
        } else {
            gc.fillPolygon(new double[]{TILE_SIZE / 2.0, TILE_SIZE / 4.0, 3 * TILE_SIZE / 4.0},
                    new double[]{TILE_SIZE / 4.0 - offsetY, 3 * TILE_SIZE / 4.0 - offsetY, 3 * TILE_SIZE / 4.0 - offsetY}, 3);
            gc.strokePolygon(new double[]{TILE_SIZE / 2.0, TILE_SIZE / 4.0, 3 * TILE_SIZE / 4.0},
                    new double[]{TILE_SIZE / 4.0 - offsetY, 3 * TILE_SIZE / 4.0 - offsetY, 3 * TILE_SIZE / 4.0 - offsetY}, 3);
        }
    }

    private void determineWinner() {
        Player winner = game.getWinner();
        String result;
        if (winner != null) {
            result = "Winner: " + winner.getColour();
        } else {
            result = "It's a draw!";
        }

        new Alert(Alert.AlertType.INFORMATION, result).showAndWait();
    }

    public static void main(String[] args) {
        launch(args);
    }
}
