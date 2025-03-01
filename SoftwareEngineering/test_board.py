import unittest
from collections import deque
import stone
import stack
import Board
import numpy as np

class TestBoard(unittest.TestCase):
    def setUp(self):
        self.board = Board.Board(board_size=5, dificulty=0)

    def test_stack_on_tile(self):
        self.assertEqual(self.board.getStack(0,0).stack_content, list(), "Should be an empty stack here")
        self.assertEqual(self.board.getStack(0,1).stack_content, list(), "Should be an empty stack here")
        self.assertEqual(self.board.getStack(4,3).stack_content, list(), "Should be an empty stack here")

        self.assertTrue(self.board.getStack(0,0).stackable, "Should be stackable")
        self.assertTrue(self.board.getStack(0,1).stackable, "Should be stackable")
        self.assertTrue(self.board.getStack(4,3).stackable, "Should be stackable")
        
    def test_place_stone(self):
        self.board.placeStone(0,4,False)
        self.assertFalse(self.board.getStack(0,4).stack_content[0].upright)
        self.assertEqual(self.board.getStack(0,4).stack_content[0].player_index, 1)
        self.board.placeStone(2,4,True)
        self.assertTrue(self.board.getStack(2,4).stack_content[0].upright)
        self.assertEqual(self.board.getStack(2,4).stack_content[0].player_index, 0)
       
    def test_check_stack_height_and_stackableness(self):
        self.board.placeStone(0,0,False)
        self.assertTrue(self.board.getStack(0,0).stackable, "Should be stackable")
        self.assertEqual(self.board.getStack(0,0).height(), 1, "Should be height 1")
        self.board.placeStone(0,0,True)
        self.assertFalse(self.board.getStack(0,0).stackable, "Shouldn't be stackable")
        self.assertEqual(self.board.getStack(0,0).height(), 2, "Should be height 2")
  
    def test_check_direction_test_false(self):
        self.board.placeStone(3,4,True)
        self.board.placeStone(3,2,True)
        self.board.placeStone(2,3,True)
        self.board.placeStone(4,3,True)
        self.assertFalse(self.board.checkRight(3,3), "Shouldn't be able to move right")
        self.assertFalse(self.board.checkLeft(3,3), "Shouldn't be able to move left")
        self.assertFalse(self.board.checkUp(3,3), "Shouldn't be able to move up")
        self.assertFalse(self.board.checkDown(3,3), "Shouldn't be able to move down")

    def test_check_direction_test_true(self):
        self.board.placeStone(3,4,False)
        self.board.placeStone(3,2,False)
        self.board.placeStone(2,3,False)
        self.board.placeStone(4,3,False)
        self.assertTrue(self.board.checkRight(3,3), "Should be able to move right")
        self.assertTrue(self.board.checkLeft(3,3), "Should be able to move left")
        self.assertTrue(self.board.checkUp(3,3), "Should be able to move up")
        self.assertTrue(self.board.checkDown(3,3), "Should be able to move down")

        #checking empty tile then adding a stone and checking again
        self.assertTrue(self.board.checkUp(1,2), "Should be able to move up")
        self.board.placeStone(1,3,True)
        self.assertFalse(self.board.checkUp(1,2), "Shouldn't be able to move up")

    def test_pick_up_stack(self):
        self.board = Board.Board(board_size=5, dificulty=0)
        self.board.placeStone(3,4,False)
        self.board.placeStone(3,4,False)
        self.board.placeStone(3,4,False)
        self.board.placeStone(4,4,False)
        self.assertTrue(self.board.pickUpStack(3,4))
        self.assertEqual(self.board.current_x, 3)
        self.assertEqual(self.board.current_y, 4)
        right_answer = stack.Stack()
        right_answer.push_stone(1, False)
        right_answer.push_stone(1, False)
        right_answer.push_stone(1, False)
        self.assertEqual(self.board.players[0].picked_up_stack.height(), 3)

    def test_move_stack(self):
        self.board.current_x = 3
        self.board.current_y = 3
        self.board.placeStone(self.board.current_x, self.board.current_y, False)
        self.board.placeStone(3,4,False)
        self.board.placeStone(3,4,False)
        self.board.pickUpStack(3,3)
        self.board.moveStack(3, 4)
        self.assertEqual(self.board.getStack(3,4).height(), 3, "Should be height 3")
        self.assertEqual(self.board.getStack(3,3).height(), 0, "Should be height 0")
        self.assertEqual(self.board.current_x, 3)
        self.assertEqual(self.board.current_y, 4)
        
        with self.assertRaises(TypeError):
            self.board.placeStone(3, 3, True, 1)
            self.board.pickUpStack(3, 4)
            self.board.moveStack(self.board.current_x, self.board.current_y, 3, 3)

        
    def test_move_stack_far(self):
        self.board.current_x = 0
        self.board.current_y = 1
        self.board.turn = 1
        self.board.placeStone(0,1,False)
        self.board.turn = 1
        self.board.placeStone(0,1,False)
        self.board.turn = 1
        self.board.placeStone(0,1,False)
        self.board.turn = 1
        self.board.placeStone(0,1,False)
        self.board.turn = 1
        self.board.placeStone(0,1,False)
        self.board.turn = 1
        self.board.placeStone(0,1,False)
        self.board.turn = 1
        self.board.placeStone(0,1,False)
        self.board.turn = 1
        self.board.placeStone(0,1,True)
        self.board.turn = 1

        
        self.board.placeStone(2,1,False)
        self.board.placeStone(2,1,False)
        self.board.placeStone(2,1,False)
        self.board.placeStone(2,1,False)

        self.board.pickUpStack(0,1)
        self.board.moveStack( 0, 1)
        self.board.moveStack( 0, 1)
        self.board.moveStack( 1, 1)
        self.board.moveStack( 2, 1)
        self.board.moveStack( 3, 1)
        self.board.moveStack( 3, 1)
        self.board.moveStack( 3, 1)
        self.board.moveStack( 4, 1)

        self.assertEqual(self.board.getStack(0,1).height(), 2, "Should be height 2")
        self.assertEqual(self.board.getStack(1,1).height(), 1, "Should be height 1")
        self.assertEqual(self.board.getStack(2,1).height(), 5, "Should be height 5")
        self.assertEqual(self.board.getStack(3,1).height(), 3, "Should be height 3")
        self.assertEqual(self.board.getStack(4,1).height(), 1, "Should be height 1")
        self.assertEqual(self.board.players[self.board.turn].picked_up_stack, None, "Should be None")

        self.assertEqual(self.board.turn, 0, "Should be turn 0")

    def test_apply_move_down(self):
        self.board = Board.Board(board_size=5, dificulty=0)
        self.board.placeStone(0,0,False)
        self.board.placeStone(0,0,False)
        self.board.placeStone(0,0,False)
        self.board.placeStone(0,0,False)
        self.board.placeStone(4,4,False)
        action = ['move', (0, 0), 0, [1, 1, 1, 1]]
        self.board.apply_action(action)

        self.assertEqual(self.board.getStack(0,0).height(),1)
        self.assertEqual(self.board.getStack(0,1).height(),1)
        self.assertEqual(self.board.getStack(0,2).height(),1)
        self.assertEqual(self.board.getStack(0,3).height(),1)

    def test_apply_move_left(self):
        self.board = Board.Board(board_size=5, dificulty=0)
        self.board.placeStone(4,4,False)
        self.board.placeStone(4,4,False)
        self.board.placeStone(4,4,False)
        self.board.placeStone(4,4,False)
        self.board.placeStone(0,0,False)
        action = ['move', (4, 4), 2, [1, 1, 1, 1]]
        self.board.apply_action(action)

        self.assertEqual(self.board.getStack(4,4).height(),1)
        self.assertEqual(self.board.getStack(3,4).height(),1)
        self.assertEqual(self.board.getStack(2,4).height(),1)
        self.assertEqual(self.board.getStack(1,4).height(),1)
        self.assertEqual(self.board.getStack(0,4).height(),0)
        
    
    def test_apply_move_different_steps_right(self):
        self.board = Board.Board(board_size=5, dificulty=0)
        self.board.placeStone(0,0,False)
        self.board.placeStone(0,0,False)
        self.board.placeStone(0,0,False)
        self.board.placeStone(0,0,False)
        self.board.placeStone(0,0,False)
        self.board.placeStone(4,4,False)
        action = ['move', (0, 0), 3, [0,2,3]]
        self.board.apply_action(action)

        self.assertEqual(self.board.getStack(0,0).height(),0)
        self.assertEqual(self.board.getStack(1,0).height(),2)
        self.assertEqual(self.board.getStack(2,0).height(),3)
        self.assertEqual(self.board.getStack(3,0).height(),0)

        
if __name__ == "__main__":
    unittest.main() 