import pygame
from image import Image

class Stone:
    #Default is flat stone
    #Player is the ID for player, used to get color of stone here
    def __init__(self, player_index,upright):
        self.player_index = player_index
        self.upright = upright
        
        
        if player_index==0 and self.upright==False:
            self.img_stone=Image("assets/picture/blue_flat_stone.png")
        if player_index==1 and self.upright==False:
            self.img_stone=Image("assets/picture/red_flat_stone.png")
        if player_index==0 and self.upright==True:
            self.img_stone=Image("assets/picture/blue_stand_stone.png")
        if player_index==1 and self.upright==True:
            self.img_stone=Image("assets/picture/red_stand_stone.png")


        
    def draw(self,screen,x,y):
        self.img_stone.set_position(x,y)
        self.img_stone.draw(screen)