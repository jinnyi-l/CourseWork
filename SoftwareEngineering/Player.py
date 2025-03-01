import pygame
from image import Image


class Player:
    def __init__(self, id, stonesLeft):
        self.id = id
        self.stonesLeft = stonesLeft
        self.picked_up_stack = None

        if self.id==1:
            self.icon=Image("assets/picture/red_icon.png")
            self.icon.set_flip()
        else:
            self.icon=Image("assets/picture/blue_icon.png")
            self.icon.set_flip()


    
    def pickUpStack(self,stack):
        self.picked_up_stack = stack

    def getId(self):
        return self.id
    
    def getStonesLeft(self):
        return self.stonesLeft
    
    def useStone(self):
        if (self.stonesLeft > 0):
            self.stonesLeft -= 1
        else:
            raise TypeError("No stones left")
    
    def getRemaining(self):
        return self.stonesLeft
    

    def draw_player_stats(self,screen, Board):
        center_x = Board.grid_size
        Background = (197, 209, 235)
        Text_color = (45, 45, 42)
        #if(self.id == 1):
        #    center_x = Board.grid_size*8
        xPos=10
        yPos=130
        if self.id ==1:
            xPos=xPos+Board.grid_size*7

        self.icon.set_position(xPos,20)
        self.icon.draw(screen)

        font = pygame.font.Font('assets/fonts/Oswald-VariableFont_wght.ttf', 30)
        stones_left = font.render(f"X:{self.stonesLeft}", True, Text_color)
        stonesRect = stones_left.get_rect()
        if self.id==1:
            stonesRect.topleft = (xPos+30, 60)
        else:
            stonesRect.topleft = (xPos+120, 60)
        screen.blit(stones_left,stonesRect)


        if(self.picked_up_stack != None):
            # draw stack
            pygame.draw.rect(screen, (0,0,0), (xPos,yPos+270,155,250), 1)
            self.picked_up_stack.draw(screen,(xPos+30,500))

            
            font = pygame.font.Font('assets/fonts/Oswald-VariableFont_wght.ttf', 25)
            text = font.render("Picked up stack", True, Text_color)
            textRect = text.get_rect()
            textRect.topleft = (xPos, Board.grid_size*3.6)
            screen.blit(text,textRect)

    
   