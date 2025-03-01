import pygame
import numpy as np
import Board

class select():
    def __init__(self,):
        self.y=0
        self.x=0
        self.select_grid=None
        self.isSelect=False
        self.color = (0, 255, 0)
    

    def input(self,y,x):
        self.set_color_valid()
        self.y=self.y+y
        self.x=self.x+x

        if self.y>4:
            self.y=4
        if self.y<0:
            self.y=0
        
        if self.x>4:
            self.x=4
        if self.x<0:
            self.x=0

        print(f"y={self.y} x={self.x}")
    
    def draw(self,screen):
        pygame.draw.rect(screen, self.color, (170+Board.Board.offset_x+self.x*Board.Board.grid_size
                                                 ,100+Board.Board.offset_y+self.y*Board.Board.grid_size
                                                 , 100, 100)
                                                 , 3)
    
    def select_grid(self,grid):
        
        self.select_grid=grid
        
    def get_selection_pos(self):
        return (self.y,self.x)
    
    def __str__(self) -> str:
        return f"y={self.y} x={self.x}"
    
    def set_invalid_color(self):
        self.color = (181, 49, 32)
    def set_color_valid(self):
        self.color = (0, 255, 0)
    
        

