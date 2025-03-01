import random
import math
import ai_logic.board_ai as board_ai
from copy import deepcopy

# Minimax algorithm implementation
def minimax(board, depth, is_maximizing, alpha, beta, is_first_move):
    # Base cases: terminal state (win/loss, draw, or depth limit reached)
    if board.has_path(1):  # AI has a winning path
        return float('inf')
    if board.has_path(0):  # Player has a winning path
        return float('-inf')
    if board.is_stalemate(1) or depth == 0:
        return board.evaluate()  # Evaluate current board if depth limit is reached

    if is_maximizing:
        # AI's turn (Maximizer)
        max_eval = -math.inf
        if is_first_move:
            valid_moves = board.get_valid_moves(0)  # Get player moves
        else:
            valid_moves = board.get_valid_moves(1)  # Get AI moves
        for move in valid_moves:
            new_board = simulate_move(board, move, 1)
            evaluation = minimax(new_board, depth - 1, False, alpha, beta, is_first_move)
            max_eval = max(max_eval, evaluation)
            alpha = max(alpha, evaluation)
            if beta <= alpha:
                break  # Alpha-beta pruning
        return max_eval
    else:
        # Player's turn (Minimizer)
        min_eval = math.inf
        if is_first_move:
            valid_moves = board.get_valid_moves(1)  # Get AI moves
        else:
            valid_moves = board.get_valid_moves(0)  # Get player moves
        for move in valid_moves:
            new_board = simulate_move(board, move, 0)
            evaluation = minimax(new_board, depth - 1, True, alpha, beta, is_first_move)
            min_eval = min(min_eval, evaluation)
            beta = min(beta, evaluation)
            if beta <= alpha:
                break  # Alpha-beta pruning
        return min_eval

# Function to simulate a move and return a new board state
def simulate_move(board, move, owner):
    new_board = deepcopy(board)  # Deep copy the board to simulate the move
    move_copy = deepcopy(move)  # Deep copy the move to prevent in-place modifications
    new_board.apply_action(move_copy, owner)  # Apply the move
    return new_board

# AI action selection for different levels
def get_action_level1(board,player):
    piece_color = player.id
    if board.round < 2:
        piece_color = 1 - player.id
    # Random selection for easy difficulty (placeholder)
    moves = board.get_valid_moves(piece_color)
    # Random selection for easy difficulty (placeholder)
    return random.choice(moves)

def get_action_level2(old_board, player):
    # Medium difficulty with shallow minimax
    best_move = None
    best_score = -math.inf
    is_first_move = False
    piece_color = player.id
    board = convert_board_to_ai_board(old_board)

    if board.turns < 2:
        piece_color = 1 - player.id
        is_first_move = True

    for move in board.get_valid_moves(piece_color):
        new_board = simulate_move(board, move, 1)
        score = minimax(new_board, depth=1, is_maximizing= not bool(player), alpha=-math.inf, beta=math.inf, is_first_move=is_first_move)
        if score > best_score:
            best_score = score
            best_move = move
    return best_move

def get_action_level3(board, player):
    # Hard difficulty with deeper minimax
    best_move = None
    best_score = -math.inf
    is_first_move = False
    piece_color = player.id
    board = convert_board_to_ai_board(board)

    if board.turns < 2:
        piece_color = 1 - player.id
        is_first_move = True
    
    for move in board.get_valid_moves(piece_color):
        new_board = simulate_move(board, move, 1)
        score = minimax(new_board, depth=2, is_maximizing= not bool(player), alpha=-math.inf, beta=math.inf, is_first_move=is_first_move)
        if score > best_score:
            best_score = score
            best_move = move
    return best_move


# Converts the Board
def convert_board_to_ai_board(board):
    new_board = board_ai.Board()

    # copy all stones
    for col in range(board.board_size):
        for row in range(board.board_size):
            stack = board.getStack(col,row)
            for i in range(stack.height()):
                stone = stack.stack_content[i]
                new_stone = board_ai.Stone(stone.player_index, stone.upright)
                new_board.board[row][col].append(new_stone)

    new_board.ai_pieces = board.players[1].getStonesLeft() # owner 1 (AI)
    new_board.player_pieces = board.players[0].getStonesLeft() # owner 0 (Player)
    new_board.turns = board.round
    new_board.level = board.difficulty

    return new_board
    



