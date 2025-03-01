import pygame
import graphic

pygame.init()

screen=pygame.display.set_mode((800,800))

game_over=False

bg=(127,127,127)
player_turn=0

board=graphic.board(position=(int(100),int(100)))
selection=graphic.select()

def key_control(event):
    global player_turn
    if event.key== pygame.K_w:
        print("w is pressed")
        selection.input(-1,0)
    if event.key==pygame.K_s:
        print("s is pressed")
        selection.input(1,0)
    if event.key==pygame.K_a:
        print("a is pressed")
        selection.input(0,-1)
    if event.key==pygame.K_d:
        print("d is pressed")
        selection.input(0,1)
    if event.key==pygame.K_j:
        print("add flat stone")
        board.grids[selection.get_selection()].add_stone(graphic.stone(player_turn,0))
        player_turn=(player_turn+1)%2
    if event.key==pygame.K_k:
        print("add stand stone")
        board.grids[selection.get_selection()].add_stone(graphic.stone(player_turn,1))
        player_turn=(player_turn+1)%2
    
while not game_over:

    for event in pygame.event.get():
        
        screen.fill(bg)
        board.draw(screen)
        selection.draw(screen)
        #print(pygame.mouse.get_pos())
        
        if event.type==pygame.KEYDOWN:
            key_control(event)
        
        if event.type==pygame.QUIT:
            game_over=True
    
    pygame.display.update()
    