import unittest
import Board
import ai_logic.ai_logic as ai_logic
import ai_logic.board_ai as ai_board

class TestCopyBoard(unittest.TestCase):
    def test_copy_empty(self):
        board = Board.Board(board_size=5, dificulty=0)
        new_board = ai_logic.convert_board_to_ai_board(board)

        self.assertEqual(new_board.ai_pieces, 21)
        self.assertEqual(new_board.player_pieces, 21)
        self.assertEqual(new_board.turns, 0)
        self.assertEqual(new_board.level, 0)
        self.assertEqual(new_board.board, [[[] for _ in range(5)] for _ in range(5)])

    
    def test_copy_only_stones(self):
        board = Board.Board(board_size=5, dificulty=0)
        #red stone on 0,0
        board.getStack(0,0).push_stone(1,0)
        #blue stone on 4,4
        board.getStack(4,4).push_stone(0,1)

        #blue stone on row 3, col 2 i.i x = 2, y = 3
        board.getStack(2,3).push_stone(0,0)


        new_board = ai_logic.convert_board_to_ai_board(board)
        self.assertEqual(new_board.ai_pieces, 21)
        self.assertEqual(new_board.player_pieces, 21)
        self.assertEqual(new_board.turns, 0)
        self.assertEqual(new_board.level, 0)
        self.assertEqual(new_board.get_position(0,0)[0].owner, 1)
        self.assertEqual(new_board.get_position(0,0)[0].orientation, 0)

        self.assertEqual(new_board.get_position(4,4)[0].owner, 0)
        self.assertEqual(new_board.get_position(4,4)[0].orientation, 1)

        self.assertEqual(new_board.get_position(3,2)[0].owner, 0)
        self.assertEqual(new_board.get_position(3,2)[0].orientation, 0)

    def test_copy_one_stack(self):
        board = Board.Board(board_size=5, dificulty=0)
        
        #stack on x = 2, y = 3 (row = 3, col = 2)
        # order of owners: 0 0 1
        board.getStack(2,3).push_stone(0,0)
        board.getStack(2,3).push_stone(0,1)
        board.getStack(2,3).push_stone(1,0)


        new_board = ai_logic.convert_board_to_ai_board(board)

        self.assertEqual(new_board.get_position(3,2)[0].owner, 0)
        self.assertEqual(new_board.get_position(3,2)[0].orientation, 0)

        self.assertEqual(new_board.get_position(3,2)[1].owner, 0)
        self.assertEqual(new_board.get_position(3,2)[1].orientation, 1)

        self.assertEqual(new_board.get_position(3,2)[2].owner, 1)
        self.assertEqual(new_board.get_position(3,2)[2].orientation, 0)


   
        



if __name__ == "__main__":
    unittest.main() 




