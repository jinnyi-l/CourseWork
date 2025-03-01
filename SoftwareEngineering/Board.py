
import stone
import stack
import Player
import pygame
import ai_logic.ai_logic as ai_logic
from image import Image
import numpy as np



class tile():
    def __init__(self,grid_path,position=(0,0)) -> None:
        self.img_grid=Image(grid_path,position)
        self.position=position
        self.stack=stack.Stack()

    
    def add_stone(self,player_index, upright_input):
        self.stack.push_stone(player_index, upright_input)
    
    def draw(self,screen):
        self.img_grid.draw(screen)
        self.stack.draw(screen,self.position)

    #clears the stack
    def empty(self):
        self.stack=stack.Stack()
    
    def is_empty(self):
        return self.stack.height != 0

    def set_stack(self,stack):
        self.stack = stack
    
        

class Board():
    offset_x=31
    offset_y=31
    grid_size=100

   
    def __init__(self, board_size,dificulty, position=(0,0)) -> None:
        self.board_size = board_size
        self.turn = 0
        self.current_x = -1
        self.current_y = -1
        #square where stack was picked up from
        self.initial_x = -1
        self.initial_y = -1
        self.direction = "none"
        self.players = [Player.Player(0,21),Player.Player(1,21)]
        self.isMove = False
        self.round = 0
        self.winner_found = False
        self.img_board=Image("assets/picture/board.png",position)
        self.position=position
        self.error_message = ""
        self.difficulty = dificulty
        self.winner_found = False
        self.init_grid()

        

          
    def init_grid(self):
            orig_x,orig_y=self.position
            self.tiles=np.empty((5,5),dtype=tile)
            for x in range(5):
                for y in range(5):
                    index=int((x+y)%2)
                    if index==0:
                        self.tiles[x,y]=tile("assets/picture/white_grid.png",(orig_x+Board.offset_x+x*Board.grid_size,
                                                                                orig_y+Board.offset_y+y*Board.grid_size))
                    elif index==1:
                        self.tiles[x,y]=tile("assets/picture/brown_grid.png",(orig_x+Board.offset_x+x*Board.grid_size,
                                                                                orig_y+Board.offset_y+y*Board.grid_size))
    

    def draw_board(self,screen):
        self.img_board.draw(screen)
        for x in range(5):
            for y in range(5):
                self.tiles[x,y].draw(screen)

        if self.turn==1:
            self.turn_Icon=Image("assets/picture/redIcon_redStone.png")
        else:
            self.turn_Icon=Image("assets/picture/blueIcon_blueStone.png")
        
        if self.round==0:
            self.turn_Icon=Image("assets/picture/blueIcon_redStone.png")
        if self.round==1:
            self.turn_Icon=Image("assets/picture/redIcon_blueStone.png")
            
        self.turn_Icon.set_flip()
        center_X=self.grid_size *4-20
        self.turn_Icon.set_position(center_X,0)
        self.turn_Icon.draw(screen)

    def draw(self,screen):
        self.draw_board(screen)
        self.players[0].draw_player_stats(screen,self)
        self.players[1].draw_player_stats(screen,self)
        self.draw_error_message(screen)
    
    def draw_error_message(self,screen):
        Background = (197, 209, 235)
        Text_color = (181, 49, 32)
        center_x = self.grid_size * (self.board_size + 3.5)
        font = pygame.font.Font('assets/fonts/Oswald-VariableFont_wght.ttf', 20)
        text = font.render(self.error_message, True, Text_color, Background)
        textRect = text.get_rect()
        textRect.center = (center_x // 2, self.grid_size - 10)
        screen.blit(text, textRect)

    def hasSelected(self):
        return (self.picked_up_stack.height() > 0)
    
    # Returns true if it is the AIs turn to make a move
    def isAiTurn(self):
        return self.difficulty > 0 and self.turn == 1
    

    # Returns the action chosen by the AI agent
    def get_action(self, player):
        if self.difficulty == 1:
            return ai_logic.get_action_level1(self,player)
        elif self.difficulty == 2:
            return ai_logic.get_action_level2(self, player)
        elif self.difficulty == 3:
            return ai_logic.get_action_level3(self, player)


## Change turn
## If user plays against AI, game mode > 0, and an AI action is requested
    def changeTurn(self):
        self.winner_found, self.winner = self.find_winner()
        if not self.possible_moves_left():
            self.winner_found, self.winner = self.majority_tiles()
        
        self.players[self.turn].picked_up_stack=None
        self.isMove = False
        if self.turn == 0:
            self.turn = 1
        else:
            self.turn = 0
        
        self.round +=1

    #preforms an ai turn
    def do_ai_turn(self):
        #only add delay to AI for easy dificulty
        if(self.difficulty == 1):
            pygame.time.wait(1000)
            
        action = self.get_action(self.players[1])
        print(action)
        self.apply_action(action)

    ## applies the action, if is_test==True the game state variables such as turn will not update, allowing ai to place multiple times in a row
    def apply_action(self, action):
        if action[0] == 'move':
            self.apply_move(action)
        if action[0] == 'place':
            self.apply_place(action)

    def apply_place(self,action):
        row,col = action[1]
        orientation = action[2]
        self.placeStone(col,row,orientation)
    
    def apply_move(self,action):
        row,col = action[1]
        direction = action[2]
        placements = action[3]
        self.pickUpStack(col,row)

        
        ## loop through all tiles
        for placement in placements:
            for i in range(placement):
                self.moveStack(col,row)
            # move x,y to next position according to direction
            if direction == 0: #down
                row += 1
            if direction == 1: # up
                row -= 1
            if direction == 2: #left
                col -= 1
            if direction == 3: #right
                col += 1
            
            
        
    def getStack(self, x, y):
        return self.tiles[x][y].stack
    
    def emptyTile(self,x,y):
        self.tiles[x][y].empty()
    
    def isEmptyTile(self,x,y):
        return self.tiles[x][y].is_empty()

    def placeStone(self, x, y, upright_input):
        if self.isMove:
            return False
        player_index = self.turn
        if self.players[player_index].getStonesLeft() == 0:
            self.error_message = "You have no stones left"
            return False
        if self.round < 2:
            player_index = (self.turn+1)%2
        if self.getStack(x,y).is_stackable():
            self.getStack(x,y).push_stone(player_index, upright_input) 
            self.changeTurn()
            self.players[player_index].useStone()
            self.error_message = ""
            return True
        else:
            self.error_message = "Invalid tile to place stone"
            return False
    
   

    def checkLeft(self, x, y):
        if (x != 0 and self.getStack(x-1,y).stackable):
            return True
        else:
            return False
            
    def checkRight(self, x, y):
        if ( x != 4 and self.getStack(x+1,y).stackable):
            return True
        else:
            return False

    def checkDown(self, x, y):
        if (y != 0 and self.getStack(x, y-1).stackable):
            return True
        else:
            return False

    def checkUp(self, x, y):
        if (y != 4 and self.getStack(x,y+1).stackable):
            return True
        else:
            return False


    def isValidMove(self, xTo, yTo):
        if (self.getStack(xTo, yTo).stackable):
           return True

    def pickUpStack(self, x, y):        
        
        if self.getStack(x,y).height() >= 1 and self.getStack(x,y).check_top_stone(self.turn):
            self.players[self.turn].pickUpStack(self.getStack(x,y))
            #reset tile
            self.emptyTile(x,y)
            self.current_x = x
            self.initial_x = x
            self.current_y = y
            self.initial_y = y
            self.isMove = True
            self.direction = "none"
            self.error_message = ""
            return True
        else:
            self.error_message = "Cannot pick up stack"
            return False
        
 

    def moveStack(self, xTo, yTo):
       
        if (self.isValidMove(xTo, yTo)):
            if (self.direction == "none"): #If first move direction has to be set
                if (self.current_x > xTo and self.current_y == yTo):
                    self.direction = "left"
                elif (self.current_x < xTo and self.current_y == yTo):
                    self.direction = "right"
                elif (self.current_y > yTo and self.current_x == xTo):
                    self.direction = "down"
                elif (self.current_y < yTo and self.current_x == xTo):
                    self.direction = "up"
            if self.current_x == xTo and self.current_y == yTo:  # Allows player to place multiple stones without moving
                self.players[self.turn].picked_up_stack.drop_stone(self.getStack(xTo, yTo))
            elif self.direction == "up" and (yTo-self.current_y == 1) and self.current_x == xTo or \
            self.direction == "down" and (yTo-self.current_y == -1) and self.current_x == xTo or \
            self.direction == "left" and (xTo-self.current_x == -1) and self.current_y == yTo or \
            self.direction == "right" and (xTo-self.current_x == 1) and self.current_y == yTo:
                self.players[self.turn].picked_up_stack.drop_stone(self.getStack(xTo, yTo))

            # Update position based on direction
                if self.direction in ["up", "down"]:
                    self.current_y = yTo
                elif self.direction in ["left", "right"]:
                    self.current_x = xTo
            else:
                self.error_message = "Have to move in one direction"
                return False
            
            if self.players[self.turn].picked_up_stack.height() == 0:
                self.players[self.turn].picked_up_stack = None
                self.changeTurn()
                self.isMove = False
                
            self.error_message = ""
            return True
        else:
            self.error_message = "Cannot move this stack"
            return False
    

    def resetMove(self):
       
        if(self.isMove and self.current_x == self.initial_x and self.current_y == self.initial_y):
            self.isMove = False
            self.tiles[self.current_x, self.current_y].set_stack(self.players[self.turn].picked_up_stack)
            self.players[self.turn].picked_up_stack = None

    
    def majority_tiles(self):
        self.first_player = 0
        self.second_player = 0
        for x in range(self.board_size):
            for y in range(self.board_size):
                if self.getStack(x,y).height() > 0: 
                    if self.getStack(x,y).check_top_stone(0):
                        self.first_player += 1
                    else:
                        self.second_player += 1
        if self.first_player > self.second_player:
            return True, 0
        elif self.first_player < self.second_player:
            return True, 1
        else:
            return True, 2
        
    def possible_moves_left(self):
         #check stones left
        self.stones_left = True
        if self.turn == 0:
           if self.players[0].getStonesLeft() == 0:
               self.stones_left = False
        else:
            if self.players[1].getStonesLeft() == 0:
               self.stones_left = False
        
        #check playable tiles left
        for x in range(self.board_size):
            for y in range(self.board_size):
                if self.getStack(x,y).height() == 0 or self.getStack(x,y).is_stackable():
                    if self.stones_left:
                        return True
                    else:
                        #check if there are movable stacks in proximity to playable tile
                        if (y > 0 and self.getStack(x, y - 1).check_top_stone(self.turn)) or \
                            (y < self.board_size - 1 and self.getStack(x, y + 1).check_top_stone(self.turn)) or \
                            (x > 0 and self.getStack(x - 1, y).check_top_stone(self.turn)) or \
                            (x < self.board_size - 1 and self.getStack(x + 1, y).check_top_stone(self.turn)) or \
                            (x > 0 and y > 0 and self.getStack(x - 1, y - 1).check_top_stone(self.turn)) or \
                            (x < self.board_size - 1 and y < self.board_size - 1 and self.getStack(x + 1, y + 1).check_top_stone(self.turn)):
                            return True
                        else:
                            continue
        return False
   

    ###############################################################################
    #                             AI UTILITY FUNCTIONS                            #
    ###############################################################################

    # Returns a list of all the valid moves for a player
    # Example of output : [['move', (1,3), 2, [2,1,1]], ['place', (0, 1), 1]...]
    def get_valid_moves(self, player):
        valid_moves = []
        if player == 0:
            pieces_left = self.players[0].stonesLeft #TODO does it update?
        else:
            pieces_left = self.players[1].stonesLeft

        for row in range(0, 5): #TODO hard coded 5
            for col in range(0, 5): #TODO hard coded 5
                # if a stack is empty, player can place a stone
                if self.isEmptyTile(row, col) and pieces_left > 0:
                    valid_moves.append(["place", (row, col), 0])
                    valid_moves.append(["place", (row, col), 1])
                # If the player owns the stack, calculate stack moves
                elif not self.isEmptyTile(row, col):
                    if self.getStack(row, col).check_top_stone(player):
                        height = self.getStack(col, row).height()
                        travel_paths = self.check_travel_paths(height, row, col)
                        if height > 1:
                            # For each direction and max distance, calculate stone combinations
                            for direction, max_distance in travel_paths:
                                for stones_left_behind in self.generate_stone_combinations(height - 1, max_distance, allow_zero_at_first_step=True): #TODO maybe not "height - 1"
                                    valid_moves.append(["move", (row, col), direction, stones_left_behind])

                        else:
                            for direction, _ in travel_paths:
                                valid_moves.append(["move", (row, col), direction, [0, 1]])

                    if self.getStack(row, col).is_stackable() and pieces_left > 0:
                        valid_moves.append(["place", (row, col), 0])
                        valid_moves.append(["place", (row, col), 1])
                
        if not valid_moves:
            # TODO : end of the game - merge winning conditions ?
            print("No valid moves")

        return valid_moves
    
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

            # Loop to find how far the stack can move in this direction
            while remaining_height > 0 and 0 <= curr_row < 5 and 0 <= curr_col < 5: #TODO hard coded 5 :/
                stackable = self.getStack(curr_col, curr_row).stackable()
                if not stackable:
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
            # for each direction, get the max distance the stack can travel
            distance = get_max_distance_in_direction(row_step, col_step)
            if distance != 0:
                travel_paths.append((index, distance))

        return travel_paths
    
    def generate_stone_combinations(self, stones_to_distribute, max_distance, allow_zero_at_first_step=True):
        """
        Generate all valid combinations of how to distribute stones over a given maximum distance.
        Example: If there are 2 stones and you can move 3 steps, you can leave [1, 1], [0, 1, 1], [0, 2] or [2] stones behind.
        """
        combinations = []

        # Base case: If only one stone, it can only be left behind in one place
        if stones_to_distribute == 1:
            return [[1]]

        start_range = 0 if allow_zero_at_first_step else 1

        # Generate combinations based on the available distance
        for i in range(start_range, stones_to_distribute + 1):
            if max_distance > 1:
                remaining = stones_to_distribute - i
                if remaining > 0:
                    for rest in self.generate_stone_combinations(remaining, max_distance - 1, allow_zero_at_first_step=False):
                        combinations.append([i] + rest)
                else:
                    combinations.append([i])
            else:
                combinations.append([stones_to_distribute])  # If only one step allowed, leave all stones here

        return combinations
    
    def evaluate(self):
        score = 0
    
        if self.find_winner() == 1: # AI wins
            return float('inf') # Best case for AI
        elif self.find_winner() == 0: # Player wins
            return float('-inf') # Worst case for AI

        score += self.evaluate_control(1) - self.evaluate_control(0) # AI controlled stacks - Player controlled stacks
        score += self.evaluate_blocking(1) - self.evaluate_blocking(0)  # AI blocking - Player blocking
        score += self.evaluate_mobility(1) - self.evaluate_mobility(0)  # AI mobility - Player mobility
        score += self.evaluate_proximity_to_win(1) - self.evaluate_proximity_to_win(0)  # AI proximity - Player proximity

        return score

    def evaluate_control(self, player):
        control_score = 0
        for row in range(5): #TODO hardcoded 5 :/
            for col in range(5): #TODO hardcoded 5 :/
                stack = self.getStack(col, row)
                if stack.check_top_stone(player) and stack.is_stackable():
                    control_score += 5
        return control_score
    
    def evaluate_blocking(self, player):
        block_score = 0
        opponent = 1 - player

        for row in range(0,5): #TODO Hardcoded 5 :/
            for col in range(0,5):  #TODO Hardcoded 5 :/
                stack = self.getStack(col, row)
                if stack.check_top_stone(player) and stack.is_stackable():
                    # Count how many opponent stones are adjacent that this blocks
                    if row > 0 and self.getStack(col, row - 1 ).check_top_stone(opponent):
                        block_score += 5  # Example block score
                    if row < (5 - 1) and self.getStack(col, row + 1).check_top_stone(opponent):
                        block_score += 5
                    if col > 0 and self.getStack( col - 1, row).check_top_stone(opponent):
                        block_score += 5
                    if col < (5 - 1) and self.getStack(col+1, row ).check_top_stone(opponent):
                        block_score += 5
        return block_score

    def evaluate_mobility(self, player):
        valid_moves = self.get_valid_moves(player)
        return len(valid_moves) * 2
    
    def evaluate_proximity_to_win(self, player):
        prox_score = 0
        top_stones = []
        for row in range(5): #TODO Hardcoded 5
            for col in range(5): #TODO 5
                stack = self.getStack(col, row)
                if stack.check_top_stone(player) and stack.is_stackable():
                    top_stones.append((row, col))
        for row in range(5):
            rowx = []
            rowx = [x for x in top_stones if x[0]==row]
            if row+1 < 5 and len(rowx)!= 0:
                rowx += [x for x in top_stones if x[0] == row+1 and x[1] not in {x[1] for x in rowx}]
            prox_score += len(rowx)**2

        for col in range(5):
            colx = []
            colx = [x for x in top_stones if x[1]==col]
            if col+1 < 5 and len(colx)!= 0:
                colx += [x for x in top_stones if x[1] == col+1 and x[0] not in {x[0] for x in colx}]
            prox_score += len(colx)**2
        
        return prox_score
    
    # checks if there is a stalemate, returns False if there are possible moves for a player, and else returns true
    def is_stalemate(self, player):
        possible_moves = self.get_valid_moves(player)

        if len(possible_moves) > 0:
            return False
        return True
    
    def find_winner(self):
        boardStones=np.empty([5,5],dtype=stone.Stone)

        ## get the top stone 
        for y in range(5):
            for x in range(5):
                
                ##get top stone
                if self.tiles[y,x].stack.height()>0:
                    theStone=self.tiles[y,x].stack.stack_content[-1]
                    ## dont need upright stone
                    if theStone.upright==False:
                        boardStones[y,x]=theStone
                    else:
                        boardStones[y,x]=None
        
        ##check top left edge stone
        startedIndex=np.array([[0,0],[0,1],[0,2],[0,3],[0,4],
                                [0,0],[1,0],[2,0],[3,0],[4,0]])
        win=False
        for index in startedIndex:
            y,x= index
            theStone=boardStones[y,x]
            if theStone !=None:
                ## Know whose stone belongs to
                player_index=theStone.player_index
                
                ## init visted and find connect
                self.visited=np.empty([0,2])
                self.visited=np.append(self.visited,[[y,x]],axis=0)
                self.findConnect(boardStones,index,player_index)
                #print(player_index)
                #print(self.visited)
                
                
                ## check win condition
                ## check top down
                top=np.array([[0,0],[0,1],[0,2],[0,3],[0,4]])
                down=np.array([[4,0],[4,1],[4,2],[4,3],[4,4]])
                topCheck=False
                downCheck=False
                
                for item in top:
                    y,x=item
                    if [y,x] in self.visited.tolist():
                        topCheck=True

                for item in down:
                    y,x=item
                    if [y,x] in self.visited.tolist():
                        downCheck=True
                
                if topCheck and downCheck:
                    win=True
                    ##print(f"!!player {player_index} win")
                    return win,player_index
                

                ## check left right
                left=np.array([[0,0],[1,0],[2,0],[3,0],[4,0]])
                right=np.array([[0,4],[1,4],[2,4],[3,4],[4,4]])

                leftCheck=False
                rightCheck=False
                for item in left:
                    x,y=item
                    if [x,y] in self.visited.tolist():
                        leftCheck=True
                for item in right:
                    x,y=item
                    if [x,y] in self.visited.tolist():
                        rightCheck=True
                
                if leftCheck and rightCheck:
                    win=True
                    ##print(f"!!player {player_index} win")
                    return win,player_index
                
        player_index=-1
        ##print("!!Not found win!!")
        return win,player_index

    def findConnect(self,boardStones,index,player_index):
        
        y,x=index
        ##top
        if 0<=y-1<=4 and 0<=x<=4:
            row_to_check=[y-1,x]
            if boardStones[y-1,x]!=None:
                theStone= boardStones[y-1,x]
                if theStone.player_index==player_index and np.any(np.all(self.visited==row_to_check,axis=1))==False:
                    self.visited=np.append(self.visited,[[y-1,x]],axis=0)
                    self.findConnect(boardStones,[y-1,x],player_index)
        ##down
        if 0<=y+1<=4 and 0<=x<=4:
            row_to_check=[y+1,x]
            if boardStones[y+1,x]!=None:
                theStone= boardStones[y+1,x]
                if theStone.player_index==player_index and np.any(np.all(self.visited==row_to_check,axis=1))==False:
                    self.visited=np.append(self.visited,[[y+1,x]],axis=0)
                    self.findConnect(boardStones,[y+1,x],player_index)
        ##right
        if 0<=y<=4 and 0<=x-1<=4:
            row_to_check=[y,x-1]
            if boardStones[y,x-1]!=None:
                theStone=boardStones[y,x-1]
                if theStone.player_index==player_index and np.any(np.all(self.visited==row_to_check,axis=1))==False:
                    self.visited=np.append(self.visited,[[y,x-1]],axis=0)
                    self.findConnect(boardStones,[y,x-1],player_index)
        
        ##left
        if 0<=y<=4 and 0<=x+1<=4:
            row_to_check=[y,x+1]
            if boardStones[y,x+1]!=None:
                theStone=boardStones[y,x+1]
                if theStone.player_index==player_index and np.any(np.all(self.visited==row_to_check,axis=1))==False:
                    self.visited=np.append(self.visited,[[y,x+1]],axis=0)
                    self.findConnect(boardStones,[y,x+1],player_index)
                
        
