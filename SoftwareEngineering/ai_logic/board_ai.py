import itertools
import ai_logic.ai_logic as ai_logic

COL = 5
ROW = 5


class Stone:
    # owner has 0 = Player, 1 = AI
    # orientation has 0 = flat, 1 = standing
    def __init__(self, owner, orientation):
        self.owner = owner
        self.orientation = orientation

    def __str__(self):
        return f"Stone(owner={self.owner}, orientation={self.orientation})"

    def __repr__(self):
        return f"Stone(owner={self.owner}, orientation={self.orientation})"


def distribute_stones(height, length):
    remaining_stones = height - length
    combinations = []

    for combo in itertools.product(range(remaining_stones + 1), repeat=length):
        if sum(combo) == remaining_stones:
            combinations.append(tuple(stones + 1 for stones in combo))

    return combinations


class Board:
    # initializes the board with a given grid set at the top of the file
    def __init__(self):
        self.board = [[[] for _ in range(COL)] for _ in range(ROW)]
        self.ai_pieces = 21  # owner 1 (AI)
        self.player_pieces = 21  # owner 0 (Player)
        self.turns = 0
        self.level = 1

    # evaluate the state of the board. High value favors the AI vs low value favors player
    def evaluate(self):
        score = 0
    
        if self.has_path(1): # AI wins
            return float('inf') # Best case for AI
        if self.has_path(0): # Player wins
            return float('-inf') # Worst case for AI

        score += self.evaluate_control(1) - self.evaluate_control(0) # AI controlled stacks - Player controlled stacks
        score += self.evaluate_blocking(1) - self.evaluate_blocking(0)  # AI blocking - Player blocking
        score += self.evaluate_mobility(1) - self.evaluate_mobility(0)  # AI mobility - Player mobility
        score += self.evaluate_proximity_to_win(1) - self.evaluate_proximity_to_win(0)  # AI proximity - Player proximity
        return score

    def evaluate_control(self, player):
        control_score = 0
        for row in range(ROW):
            for col in range(COL):
                top_stone = self.get_top_stone(row, col)
                if top_stone is not None and top_stone.owner == player and top_stone.orientation == 0:
                    control_score += 10
        return control_score
    
    def evaluate_blocking(self, player):
        block_score = 0
        opponent = 1 - player

        for row in range(ROW):
            for col in range(COL):
                top_stone = self.get_top_stone(row, col)
                if top_stone is not None and top_stone.owner == player and top_stone.orientation == 1:
                    # Count how many opponent stones are adjacent that this blocks
                    if row > 0 and self.get_top_stone(row - 1, col) is not None and self.get_top_stone(row - 1, col).owner == opponent:
                        block_score += 2  # Example block score
                    if row < ROW - 1 and self.get_top_stone(row + 1, col) is not None and self.get_top_stone(row + 1, col).owner == opponent:
                        block_score += 2
                    if col > 0 and self.get_top_stone(row, col - 1) is not None and self.get_top_stone(row, col - 1).owner == opponent:
                        block_score += 2
                    if col < COL - 1 and self.get_top_stone(row, col + 1) is not None and self.get_top_stone(row, col + 1).owner == opponent:
                        block_score += 2
        return block_score

    def evaluate_mobility(self, player):
        valid_moves = self.get_valid_moves(player)
        return len(valid_moves)/10
    
    def evaluate_proximity_to_win(self, player):
        prox_score = 0
        top_stones = []
        for row in range(ROW):
            for col in range(COL):
                top_stone = self.get_top_stone(row, col)
                if top_stone is not None and top_stone.owner == player and top_stone.orientation == 0:
                    top_stones.append((row, col))
        for row in range(ROW):
            rowx = []
            rowx = [x for x in top_stones if x[0]==row]
            if row+1 < ROW and len(rowx)!= 0:
                rowx += [x for x in top_stones if x[0] == row+1 and x[1] not in {x[1] for x in rowx}]
            prox_score += len(rowx)**3

        for col in range(COL):
            colx = []
            colx = [x for x in top_stones if x[1]==col]
            if col+1 < COL and len(colx)!= 0:
                colx += [x for x in top_stones if x[1] == col+1 and x[0] not in {x[0] for x in colx}]
            prox_score += len(colx)**3
        
        return prox_score

    # Owner is the one who made the latest move.
    def has_win(self, owner):
        # You win by creating a path from one side of the board to the opposite side

        # If a move creates a winning path for both your own color and your opponent at the same time, 
        # whoever made the move wins the game
        if self.has_path(owner):
            return (True, owner)  # if the player making the move won

        opponent = abs(owner - 1)

        if self.has_path(self, opponent):
            return (True, opponent)

        return (False, None)

    def has_path(self, owner):
        '''
        Returns True iff owner has a path from one side to the other of flat stones in either a vertical or horizontal direction.
        '''

        # Helper function to check if a position is valid
        def is_valid_position(row, col):
            return 0 <= row < ROW and 0 <= col < COL

        # Helper function to check if a stone belongs to the owner and is flat
        def is_flat_owned_stone(row, col):
            stack = self.get_position(row, col)
            flat = stack and stack[-1].owner == owner and stack[-1].orientation == 0
            return flat

        # DFS to find a path from one side to the opposite side
        def dfs(row, col, visited, direction):
            reached_end = (direction == "vertical" and row == ROW - 1) or (direction == "horizontal" and col == COL - 1)
            if reached_end:
                return True

            visited.add((row, col))
            # Check all four directions: up, down, left, right
            for d_row, d_col in [(-1, 0), (1, 0), (0, -1), (0, 1)]:
                new_row, new_col = row + d_row, col + d_col
                if is_valid_position(new_row, new_col) and (new_row, new_col) not in visited and is_flat_owned_stone(
                        new_row, new_col):
                    if dfs(new_row, new_col, visited, direction):
                        return True

            return False

        # Check for a vertical path (top to bottom)
        for col in range(COL):
            if is_flat_owned_stone(0, col):  # Start from the top row
                if dfs(0, col, set(), "vertical"):
                    return True

        # Check for a horizontal path (left to right)
        for row in range(ROW):
            if is_flat_owned_stone(row, 0):  # Start from the left column
                if dfs(row, 0, set(), "horizontal"):
                    return True

        return False
    
    #checks if there is a stalemate and possible winnner.Returns a tuple, bool with true being that a stalemate exists and the second being the id of the winner, or None if there is a tie
    def has_stalemate(self, owner):
        opponent = abs(owner - 1)
        if(self.is_stalemate(opponent)):
            winner = self.stalemate_winner(owner)
            if (winner == owner):
                return (True,owner)
            elif (winner == opponent):
                return (True,opponent)
            else:
                return (True, None)
        return (False,None)

    # checks if there is a stalemate, returns False if there are possible moves for opponent, and else returns true
    def is_stalemate(self, opponent):
        possible_moves = self.get_valid_moves(opponent)

        if len(possible_moves) > 0:
            return False
        return True
    
    # checks who has the most stones on top of stacks, returns the winner or none in case of tie
    def stalemate_winner(self, owner):
        opponent = abs(owner - 1)
        opponent_stacks = 0
        owner_stacks = 0
        for row in range(ROW):
            for col in range(COL):
                top_stone = self.get_top_stone(row, col)
                if top_stone is not None:
                    if top_stone.owner == owner:
                        owner_stacks += 1
                    else:
                        opponent_stacks+=1

        if (owner_stacks>opponent_stacks):
            return owner
        elif (opponent_stacks>owner_stacks):
            return opponent
        else:  
            return None
        

    # Visually displays the board in the terminal for development purposes
    def display_board(self):
        for row in self.board:
            print(row)

    # returns the Stack of stones at a given position
    # Note: top of stack is last in list
    def get_position(self, row, col):
        return self.board[row][col]

    # returns true if the stack at a given position is empty else false
    def is_empty_stack(self, row, col):
        return self.get_position(row, col) == []

    # returns the top stone of a stack at a given position
    def get_top_stone(self, row, col):
        if self.get_position(row, col) == []:
            return None
        stack = self.get_position(row, col)
        top_stone = stack[-1]
        return top_stone

    # Checks if the move is a valid move
    def valid_move(self, row, col, orientation):
        if row >= ROW or row < 0 or col >= COL or col < 0:
            raise ValueError("Cannot place outside of board")

        if self.get_top_stone(row, col) != None and self.get_top_stone(row, col).orientation == 1:
            raise ValueError("Cannot place on top a standing stone")

        if orientation < 0 or orientation > 1:
            raise ValueError("Must choose standing or flat")

            # places a stone at given position for given owner in given orientation

    def place_stone(self, row, col, owner, orientation):
        self.valid_move(row, col, orientation)

        stone = Stone(owner, orientation)
        self.board[row][col].append(stone)
        if owner == 1:
            self.ai_pieces -= 1
        else:
            self.player_pieces -= 1

    # Nora & Maria
    # Players move a stone or stack
    def move(self, row, col, owner):
        # Get direction
        direction = self.get_direction()
        # Check whether stack is empty
        if self.is_empty_stack(row, col) == True:
            raise ValueError("You cannot move an empty stack")
        
        # Get owner of stack
        stack_owner = (self.get_top_stone(row, col)).owner
        if owner != stack_owner:
                raise ValueError("You need to be the owner of the stack")

        flat = 0
        
        # Check whether stone or stack
        print(len(self.board[row][col]))
        if len(self.board[row][col]) > 1: #TODO: make it.. > 2:, since u dont have any option of how many your leaving if u onlya have two in the stack.

            # For each step taken
            ongoing = True
            while ongoing:
                if len(self.board[row][col]) <= 1:
                    raise ValueError("jalladå")
                # Ask how many stones to leave behind and check they are a valid amount
                dropoff_len = int(input("How many stones would you like to leave behind you?"))

                if dropoff_len < len(self.board[row][col]):
                    # Check if valid move with new coordinates
                    # If yes move stack, if not raise error
                    if direction == 2:  # 2 = down
                        self.valid_move(row + 1, col, flat)
                        ongoing = self.aux_stack(row, col, row + 1, col, dropoff_len)
                        row = row + 1
                        if (not ongoing):
                            break
                    if direction == 4:  # 4 = left
                        self.valid_move(row, col - 1, flat)
                        ongoing = self.aux_stack(row, col, row, col - 1, dropoff_len)
                        col = col -  1
                        if (not ongoing):
                            break
                    if direction == 6:  # 6 = right
                        self.valid_move(row, col + 1, flat)
                        ongoing = self.aux_stack(row, col, row, col + 1, dropoff_len)
                        col = col + 1
                        if (not ongoing):
                            break
                    if direction == 8:  # 8 = up
                        self.valid_move(row - 1, col, flat)
                        ongoing = self.aux_stack(row, col, row - 1, col, dropoff_len)
                        row = row - 1
                        if (not ongoing):
                            break
                else:
                    raise ValueError("You cannot leave behind your entire stack")
                
                
                ongoing = input("Do you want to move more? (Yes/No): ").strip().lower() == 'yes'
                if ongoing != True and ongoing != False:
                    raise ValueError("Not a valid option")
        else:
            # Check if valid move with new coordinates
            # If yes move stone, if not raise error
            if direction == 2:
                self.valid_move(row + 1, col, flat)
                self.aux_stone(row, col, row + 1, col)
            if direction == 4:
                self.valid_move(row, col - 1, flat)
                self.aux_stone(row, col, row, col - 1)
            if direction == 6:
                self.valid_move(row, col + 1, flat)
                self.aux_stone(row, col, row, col + 1)
            if direction == 8:
                self.valid_move(row - 1, col, flat)
                self.aux_stone(row, col, row - 1, col)

    # AI move a stack
    def ai_move(self, moves):
        for i in range(1, len(moves)):
            move = moves[i]
            current_row = move[0][0]
            current_col = move[0][1]
            dropoff_len = move[1]
            if(i+1 <= len(moves)):
                next_move = move[i+1]
                next_row = next_move[0][0]
                next_col = next_move[0][1]
                self.aux_stack(current_row, current_col, next_row, next_col, dropoff_len)


    def move_stack(self, action):
        # action : ["move", (0,2), 0, [1,2,1]]
        #          ["move", stack to move, direction, [nr stones left behind each step]]
        row = action[1][0]
        col = action[1][1]
        dir = action[2]
        drop_stones = action[3]
        ongoing = True

        if drop_stones != []:
            if dir == 0:
                while len(drop_stones) > 0 and ongoing:
                    ongoing = self.aux_stack(row, col, row + 1, col, drop_stones[0])
                    drop_stones.pop(0)
                    row = row + 1

            if dir == 1:
                while len(drop_stones) > 0 and ongoing:
                    ongoing = self.aux_stack(row, col, row - 1, col, drop_stones[0])
                    drop_stones.pop(0)
                    row = row - 1

            if dir == 2:
                while len(drop_stones) > 0 and ongoing:
                    ongoing = self.aux_stack(row, col, row, col - 1, drop_stones[0])
                    drop_stones.pop(0)
                    col = col -1

            if dir == 3:
                while len(drop_stones) > 0 and ongoing:
                    ongoing = self.aux_stack(row, col, row, col + 1, drop_stones[0])
                    drop_stones.pop(0)
                    col = col + 1

        else:
            if dir == 0:
                self.aux_stone(row, col, row + 1, col)

            if dir == 1:
                self.aux_stone(row, col, row - 1, col)

            if dir == 2:
                self.aux_stone(row, col, row, col - 1)

            if dir == 3:
                self.aux_stone(row, col, row, col + 1)
            



    # Nora & Maria
    # Help function to move stacks
    def aux_stack(self, current_row, current_col, next_row, next_col, dropoff_len):

        # Separate drop off from top to move
        dropoff = self.board[current_row][current_col][:dropoff_len:]
        remainder = self.board[current_row][current_col][~(dropoff_len):]

        # Drop stones off
        self.board[current_row][current_col] = dropoff

        # Check if top is stone or stack
        if len(remainder) == 1:
            # Place top stone on next tile and quit loop
            self.board[next_row][next_col] = remainder
            return False
        else:
            # Place top stack on next tile
            self.board[next_row][next_col] = remainder
            return True

    # Nora & Maria
    # Help function to move stone
    def aux_stone(self, current_row, current_col, next_row, next_col):
        stone = self.board[current_row][current_col].pop()
        self.board[next_row][next_col].append(stone)

    # Nora & Maria
    def get_direction(self):
        direction = int(input("Choose direction! (Up = 8, Down = 2, Left = 4, Right = 6): "))
        if direction not in [2, 4, 6, 8]:
            raise ValueError("Not a valid option for direction")
        return direction

    # Applies a given action to the board
    # Action is a list of the form ["move", (0,2), 0, [1,2,1]] or ["place", (0,1), 1]
    # If a single stone is to be moved, the list will be ["move", (0,2), 0, []]

    def apply_action(self, action, owner):
        if action[0] == 'move':
            self.move_stack(action) #also for moving singular stones
        elif action[0] == 'place':
            self.place_stone(action[1][0], action[1][1], owner, action[2])


    # Nina & Victoria
    # The user will always be the first to play
    def user_turn(self):
        if self.turns == 0:
            try:
                level = int(input("Choose level of difficulty: 1 for easy, 2 for medium, 3 for hard: "))
                if level not in [1, 2, 3]:
                    raise ValueError("Invalid level chosen.")
                self.level = level
            except ValueError as e:
                print(f"Error: {e}. Please enter 1, 2, or 3.")
            self.level = int(level)
            row = int(input("Choose row: "))
            col = int(input("Choose col: "))
            orientation = int(input("Choose orientation: 0 for flat, 1 for standing"))
            owner = 1  # player
            self.place_stone(row, col, owner, orientation)
            self.turns += 1
            return

        action = int(input("Choose your action between 1. Move a stack or 2. Place a stone :"))
        if action == 1:
            row = int(input("Choose row: "))
            col = int(input("Choose col: "))
            owner = 0  # player
            self.move(row, col, owner)  # what about the orientation ?
            self.turns += 1

        if action == 2:
            row = int(input("Choose row: "))
            col = int(input("Choose col: "))
            orientation = int(input("Choose orientation: 0 for flat, 1 for standing"))
            owner = 0  # player
            self.place_stone(row, col, owner, orientation)
            self.turns += 1

    # Returns the action chosen by the AI agent
    def get_action(self, player):
        # moves = self.get_valid_moves(player)
        if self.level == 1:
            return ai_logic.get_action_level1(self, player)
        elif self.level == 2:
            return ai_logic.get_action_level2(self, player)
        elif self.level == 3:
            return ai_logic.get_action_level3(self, player)

    # TODO : refactor to make a MVC structure?
    def AI_turn(self):
        print("AI's turn")
        # if self.turns == 1:
        #     action = action = self.get_action(0)  # places a stone for the opponent
        #     self.apply_action(action, 0)
        #     self.turns += 1
        # else:
        action = self.get_action(1)
        self.apply_action(action, 1)
        self.turns += 1

    def generate_stone_combinations(self, stones_to_distribute, max_distance, allow_zero_at_first_step):
        """
        Generate all valid combinations of how to distribute stones over a given maximum distance.
        Example: If there are 2 stones and you can move 3 steps, you can leave [1, 1], [0, 1, 1], [0, 2] or [2] stones behind.
        """
        combinations = []

        # Base case: If only one stone, it can only be left behind in one place
        if stones_to_distribute == 1:
            return [[1]]
        
        if stones_to_distribute == 0:
            return [[0]]

        start_range = 0 if allow_zero_at_first_step else 1

        # Generate combinations based on the available distance
        for i in range(start_range, stones_to_distribute + 1):
            remaining = stones_to_distribute - i

            if max_distance > 1:
                if remaining > 0:
                    for rest in self.generate_stone_combinations(remaining, max_distance - 1, allow_zero_at_first_step=False):
                        combination = [i] + rest
                        if len(combination) > 1:
                            combinations.append(combination)
                        # combinations.append([i] + rest)
                else:
                    if i != stones_to_distribute:  # Exclude the case where i == stones_to_distribute (like [4])
                        combinations.append([i])

            else:
                if stones_to_distribute != i:  # Prevent adding [stones_to_distribute] when i == stones_to_distribute
                    combinations.append([0, stones_to_distribute])  # If only one step allowed, leave all stones here

        return combinations

    def check_travel_paths(self, height, row, col):    
    # Checks how far a stack can travel in each direction and returns a list of tuples
    # where each tuple contains the direction and the maximum distance that can be traveled
    # in that direction.
    
    # Returns:
    #     List of tuples: [(direction, max_distance), ...]
    #     Directions: 0 = down, 1 = up, 2 = left, 3 = right
    
        def get_max_distance_in_direction(row_step, col_step):
            curr_row, curr_col = row + row_step, col + col_step
            remaining_height = height # Since we start with the initial stack
            max_distance = 0

            if height == 1:
                remaining_height = 1

            # Loop to find how far the stack can move in this direction
            while remaining_height > 0 and 0 <= curr_row < ROW and 0 <= curr_col < COL:
                top_stone = self.get_top_stone(curr_row, curr_col)
                if top_stone and top_stone.orientation != 0:
                    break  # Obstacle encountered, stop moving further in this direction
                max_distance += 1
                curr_row += row_step
                curr_col += col_step
                remaining_height -= 1

            return max_distance

        # Directions: (row_step, col_step) for down, up, left, right
        directions = [(1, 0), (-1, 0), (0, -1), (0, 1)]
        travel_paths = []
        for index, (row_step, col_step) in enumerate(directions):
            distance = get_max_distance_in_direction(row_step, col_step)
            if distance != 0:
                travel_paths.append((index, distance))


        # For each direction, get the max distance the stack can travel
        # travel_paths = [(index, get_max_distance_in_direction(row_step, col_step))
        #                 for index, (row_step, col_step) in enumerate(directions)]
        


        return travel_paths

    # Returns a list of all the valid moves for a player
    # Example of output : [ ['place', (0, 1), 1]]
    def get_valid_moves(self, player):
        valid_moves = []
        if player == 0:
            pieces_left = self.player_pieces
        else:
            pieces_left = self.ai_pieces

        for row in range(0, ROW):
            for col in range(0, COL):
                # if a stack is empty, player can place a stone
                if self.is_empty_stack(row, col) and pieces_left > 0:
                    valid_moves.append(["place", (row, col), 0])
                    valid_moves.append(["place", (row, col), 1])
                # If the player owns the stack, calculate stack moves
                elif not self.is_empty_stack(row, col):
                    if self.get_top_stone(row, col).owner == player:
                        height = len(self.board[row][col])
                        travel_paths = self.check_travel_paths(height, row, col)
                        if height > 1:
                            # For each direction and max distance, calculate stone combinations
                            for direction, max_distance in travel_paths:
                                for stones_left_behind in self.generate_stone_combinations(height, max_distance, allow_zero_at_first_step=True):
                                    if len(stones_left_behind) == 1:
                                        raise ValueError("Invalid move: stones_left_behind cannot contain only 1 element")

                                    valid_moves.append(["move", (row, col), direction, stones_left_behind])

                        else:
                            for direction, _ in travel_paths:
                                valid_moves.append(["move", (row, col), direction, [0, 1]])

                    if self.get_top_stone(row, col).orientation == 0 and pieces_left > 0:
                        valid_moves.append(["place", (row, col), 0])
                        valid_moves.append(["place", (row, col), 1])
                
        if not valid_moves:
            # TODO : end of the game - merge winning conditions ?
            print("No valid moves")
            pass

        # Filter out invalid "move" moves with an empty list as the last element
        valid_moves = [move for move in valid_moves if not (move[0] == "move" and not move[3])]
        return valid_moves

