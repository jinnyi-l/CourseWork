
import stone

class Stack:
    def __init__(self):
        self.stack_content = list()
        self.stackable = True
    
    def draw(self,screen,position=(0,0)):
         if self.height() > 0:
            for i in range(self.height()):
                x,y=position
                self.stack_content[i].draw(screen,x,y-15*i)

    

    
    # returns amount of stones in stack (length of queue)
    def height(self):
        return len(self.stack_content)
    
    def is_stackable(self):
        return self.stackable

    def check_top_stone(self, player_index):
        '''Checks if top stone belongs to player'''
        if self.height() == 0:
            return False
        topstone = self.stack_content[-1]
        if topstone.player_index == player_index:
            return True
        return False

    '''Pushes a stone from a players hand on the stack in a flat or standing stance'''
    def push_stone(self, player_index, upright_input):
        
        if upright_input == True:
            new_stone = stone.Stone(player_index,True)
            self.stackable = False
            self.stack_content.append(new_stone)
        else:
            new_stone = stone.Stone(player_index,False)
            self.stack_content.append(new_stone)

    '''Drops a stone from the stack to the next stack'''
    def drop_stone(self, next_stack):
        #removed_stone = self.stack_content.popleft()
        removed_stone = self.stack_content.pop(0)
        if removed_stone.upright:
            next_stack.stackable = False
        next_stack.stack_content.append(removed_stone)
        return next_stack
    