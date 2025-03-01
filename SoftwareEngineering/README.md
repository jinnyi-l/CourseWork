# The UU game
The UU game is a 1-2 player board game built using pygame.
You can choose to play against another person locally via the same keyboard or against an AI.


## Installation
1. Clone the repo:
   ```bash
   git clone https://github.com/martingundhus/SEPM-Group-1.git
   ```
2. Navigate to the project directory:
   ```bash
   cd SEPM-Group-1
   ```
3. Install dependencies: Make sure you have Pygame installed. You can install it using pip:
   ```bash
   pip install pygame
   ```
4. Run the game:
   ```bash
   python Game.py
   ```

## How to play
### Objective
 Be the first to connect your stones from one side of the board to the other. The path can twist and turn, but diagonal connections don’t count. All stones in the winning line have to be flat.

### Controls
- WASD or arrow keys to move
- J to place flat stone
- K to place standing stone
- L to pick up and move stack
- O to undo choice of stack to move

### Rules
#### Stones:
- Each player has 21 stones.
- Stones can be placed flat or standing, on an empty tile or on top of flat stones.
- Flat stones placed on top of each other make a stack.
- Stones can never be placed on top of standing stones.

#### Moving Stones:
- You can move stones and stacks if the top stone is yours.
- When moving a stack, you must drop at least one stone per tile moved.
- Stacks move in straight lines.

#### Turns:
- First Move: Each player starts by placing one of their opponent's stones.
- On each turn, players either place a stone or move an existing stone/stack.

#### Winning:
- The game ends when one player connects a line across the board.
- If no line is made, the player with the most top stones wins.

