import pygame

class Image():
    def __init__(self,path,position=(0,0)) -> None:
        self.img=pygame.image.load(path)

        self.rect=self.img.get_rect()
        self.rect.topleft=position
    
    def set_position(self,x,y):
        self.rect.topleft=x,y
    def set_flip(self):
        self.img=pygame.transform.flip(self.img, True, False) 
    def draw(self,screen):
        screen.blit(self.img,self.rect)
