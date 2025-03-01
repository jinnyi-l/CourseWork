import unittest
import stone
import stack

class TestStone(unittest.TestCase):
    def setUp(self):
        self.stone = stone.Stone(1,False)

    # Testing if stone is created with correct properties
    def test_stone_properties(self):
        self.assertEqual(self.stone.player_index, 1, "Stone should be 1")
        self.assertFalse(self.stone.upright, "Upright should be False")
        self.stone.upright = True
        self.assertTrue(self.stone.upright, "Upright should be True")


class TestStack(unittest.TestCase):
    def setUp(self):
        self.stack = stack.Stack()

    # Testing if placing a stone in a stack works 
    def test_push_stone(self):
        self.stack.push_stone(1, False)
        self.assertEqual(len(self.stack.stack_content), 1, "stack should contain 1")
    
    # Testing if placing several stones in a stack works
    def test_push_several_stones(self):
        self.stack.push_stone(1, False)
        self.stack.push_stone(2, False)
        self.stack.push_stone(1, False)

    #Testing if height of stack function works
    def test_height_function(self):
        self.stack.push_stone(1, False)
        self.stack.push_stone(2, False)
        self.stack.push_stone(1, True)
        self.assertEqual(self.stack.height(), 3, "Height should be 3")
    
    # Testing if stackable boolean is correctly updated when placing new stones
    def test_stackable(self):
        self.stack.push_stone(1, False)
        self.stack.push_stone(2, False)
        self.assertTrue(self.stack.stackable)
        self.stack.push_stone(1, True)
        self.assertFalse(self.stack.stackable)

    #Testing if check_top_stone function returns correct value
    def test_check_top_stone(self):
        self.stack.push_stone(1, False)
        self.stack.push_stone(2, False)
        self.stack.push_stone(1, False)
        self.assertTrue(self.stack.check_top_stone(1), "Should be True")
        self.stack.push_stone(2, False)
        self.assertFalse(self.stack.check_top_stone(1), "Should be false")

    # Testing if drop_stone drops the correct stone and adds it in correct order
    def test_drop_stone(self):
        self.stack.push_stone(1, False)
        self.stack.push_stone(2, False)
        self.stack.push_stone(1, False)

        next_stack = stack.Stack()
        self.assertEqual(next_stack.height(), 0, "Height should be 0")

        self.stack.drop_stone(next_stack)
        self.assertEqual(len(next_stack.stack_content), 1, "should contain 1")
        self.assertEqual(len(self.stack.stack_content), 2, "should contain 2")

        # test to drop standing 
        self.stack.push_stone(1, True)
        self.stack.drop_stone(next_stack)
        self.stack.drop_stone(next_stack)

        #self.assertEqual(next_stack.stack_content)
        test_content = []
        for stone in next_stack.stack_content:
            print(f"Stone(player={stone.player_index}, upright={stone.upright})")
            test_content.append([stone.player_index, stone.upright])
        print(test_content)
        self.assertEqual(test_content, [[1, False], [2, False], [1, False]])
       # next_stack.push_stone()
       
    # test if dropping a stone on an existing stack updates the stackable boolean
    def test_stackable_with_drop(self):
        self.stack.push_stone(1, True)
        self.assertFalse(self.stack.stackable, "Should be false")
        next_stack = stack.Stack()
        next_stack.push_stone(2, False)
        self.assertTrue(next_stack.stackable)
        self.stack.drop_stone(next_stack)
        self.assertFalse(next_stack.stackable, "Should be false")

    
        
if __name__ == "__main__":
    unittest.main()
       