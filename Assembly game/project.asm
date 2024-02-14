
.model small
.stack 100h
.386
.data
;-----------MAIN MENU-------------------------------------------------------------------------
	main_menu db 'MAIN MENU','$'
	game1 db 'PRESS 1 TO PLAY CHONKER','$';
	game2 db 'PRESS 2 TO PLAY TRUTH AND DARE', '$';
	main_menu_exit db 'PRESS 3 TO EXIT TO DOSBOX','$';
	subject db 'CPE 412 FINAL PROJECT','$'
	prepared_by db 'PREPARED BY: KARL LAWRENCE S. ECONAR','$'
	
	exit_game db 0
	choice db 0

;-------------------CHONKER GAME---------------------------------------------------------------------
	msg DB "GAME OVER", 0	; first msg
	nSize DW ($ - msg)-1
	
    xpos DB 20h            ; chonker x-position
	ypos DB 8h			   ; not used in this code. Maybe later.
	seed dw 11             ; Default initial seed of 11   
	
;                 CHONKER GAME MAIN MENU
	prompt_menu_chonker db 'CHONKER GAME','$' 
	start_game_chonker db 'PRESS 1 TO START THE GAME','$'
	mechanics_chonker db 'PRESS 2 FOR MECHANICS','$'
	chonker_exit_program db 'PRESS 3 TO EXIT TO THE MAIN MENU','$'
	game_over db 'GAME OVER!','$'
	back_to_main_menu2 db 'PRESS X TO CHONKER GAME MENU', '$'
	
;				  CHONKER MECHANICS
chonker_game_mechanics db 'CHONKER GAME MECHANICS:','$' 
mechanics_chonker1 db 'The player controls a character called',0AH,0DH,0AH,0DH, '  the "Chonker" at bottom of the screen', 0AH,0DH,0AH,0DH
				   db ' by pressing "A" to move left or "D"', 0AH,0DH,0AH,0DH,' to move right to avoid randomly falling', 0AH,0DH
				   db 'rocks that continuously spawn each round', 0AH,0DH,0AH,0DH,'$'
					
					
;-----------------TRUTH OR DARE GAME--------------------------------------------------------
	tod_start_game 	db 'PRESS "SPACE" TO TOSS A COIN','$'
	tod_toss_coin 	db  'Tossing a coin....','$'
	toss_coin_result db 'The result is: ','$'
	result_one		db 'You got HEAD.','$'
	result_heads 	db	'Here is your truth question','$'
					
	result_two 		db 'You got TAILS','$'
	result_tails	db	'Here is your dare challenge','$'
	
	back_to_main_menu db 'PRESS X TO TRUTH AND DARE GAME MENU', '$'
	please_try_again db 'PRESS A TO RETRY', '$'
	
    truth_question1    DB 'WHAT IS YOUR BIGGEST FEAR?$','$'
    truth_question2    DB 'WHEN WAS THE LAST TIME YOU CRIED?', '$'
    truth_question3    DB 'DO YOU HAVE A HIDDEN TALENT?', '$'
    truth_question4    DB 'WHAT IS YOUR DREAM LIFE?','$'
    truth_question5    DB 'WHAT DO YOU VALUE THE MOST -' ,0AH,0DH,0AH,0DH,' MONEY, FAME, SUCCESS, FRIENDS, FAMILY?',0AH,0DH,0AH,0DH, '$'
    truth_question6    DB 'WHAT IS THE BIGGEST MISCONCEPTION ABOUT YOU?',  '$'
    truth_question7    DB 'WHAT IS THE BEST PIECE OF', 0AH,0DH,0AH,0DH, '       ADVICE YOU HAVE BEEN GIVEN?' ,  '$'
    truth_question8    DB 'WHAT IS THE BEST THING ANYONE HAS EVER DONE FOR YOU?','$'
    truth_question9	   DB 'WHAT IS THE BIGGEST MISTAKE',0AH,0DH,0AH,0DH,'           YOU HAVE EVER MADE?', '$'
    truth_question10   DB 'WHAT IS YOUR PROUDEST ACCOMPLISHMENT?',  '$'
	
	truth_questions dw offset truth_question1
					dw offset truth_question2
					dw offset truth_question3
					dw offset truth_question4
					dw offset truth_question5
					dw offset truth_question6
					dw offset truth_question7
					dw offset truth_question8
					dw offset truth_question9
					dw offset truth_question10

    num_truth         EQU ($ - truth_questions) /2

    dare_challenge1    DB 'SING A SONG', '$'
    dare_challenge2    DB 'DANCE A TIKTOK CHALLENGE',  '$'
    dare_challenge3    DB 'TELL A JOKE','$'
    dare_challenge4    DB 'SPIN AROUND 10 TIMES', '$'
    dare_challenge5    DB 'CLAP YOUR HANDS 20 TIMES','$'
	dare_challenge6	   DB 'PAPASRA MI MAAM, PLEASE', '$'
    dare_challenge7    DB 'MAKE A FUNNY FACE', '$'
    dare_challenge8    DB 'CLAP YOUR HANDS 20 TIMES', '$'
    dare_challenge9    DB 'MAKE A FUNNY FACE','$'
    dare_challenge10   DB 'DO PUSH-UPS 10 TIMES', '$'

	
	dare_challenges dw offset dare_challenge1
					dw offset dare_challenge2
					dw offset dare_challenge3
					dw offset dare_challenge4
					dw offset dare_challenge5
					dw offset dare_challenge6
					dw offset dare_challenge7
					dw offset dare_challenge8
					dw offset dare_challenge9
					dw offset dare_challenge10
					 			 
    num_dare          EQU ($ - dare_challenges) /2	

;                      TRUTH OR DARE GAME
	prompt_menu_tod db 'TRUTH AND DARE GAME','$' 
	start_game_tod db 'PRESS 1 TO START THE GAME','$'
	mechanics_tod db 'PRESS 2 FOR MECHANICS','$'
	tod_exit_program db 'PRESS 3 TO EXIT TO THE MAIN MENU','$'
	
	chonker_back_to_main_menu db 'PRESS X TO CHONKER GAME MENU', '$'
	chonker_try_again db 'PRESS A TO RETRY', '$'
	spaces db "                                        ",'$'

; 					   TRUTH AND DARE MECHANICS	
	TOD_game_mechanics db 'TRUTH OR DARE GAME MECHANICS', '$'
	mechanics_tod1 db  '1.Play with your friends.','$'
	mechanics_tod2 db	'2.Decide if Heads or Tail.','$'
	mechanics_tod3 db	'3.The result will randomly generate.','$'
	mechanics_tod4 db	'4.If, Heads: answer a Truth question,',0AH,0DH,0AH,0DH,'Else, Tails: perform a Dare challenge',0AH,0DH,'$'
	mechanics_tod5 db	'5.Retry until boredom ','$'

;-----------------------------------------------------------------------------------------------------------------------------------------------
;-----------------------------------------------------------------------------------------------------------------------------------------------

.code
	main proc near
		mov ax, @data ; initialize data segment
		mov ds, ax
		
		call clear_screen
		call project_main_menu

	;Wait for a key to press
		choice_key: 
			call wait_for_keys
	
	; Process user choice
		cmp choice, '1'
		je play_game1       ; If '1' was pressed, jump to play_game1
		cmp choice, '2'
		je play_game2       ; If '2' was pressed, jump to play_game2
		cmp choice, '3'
		je exit_program     ; If '3' was pressed, jump to exit_program
		jmp choice_key
		
		exit_program:
			mov exit_game, 1
			call exit_process
		ret
		
	play_game1:
		call clear_screen
		jmp run_game1
		ret
		
		run_game1:
			call clear_screen
			call video_mode_black
			call chonker_game_menu	;start the game menu of the chonker game
		; waits for a key press
			chonker_wait_for_key:
				call wait_for_keys
				
				;Process user choice
				cmp choice, '1'
				je play_start_chonker       ; If '1' was pressed, jump to play_start_chonker
				cmp choice, '2'
				je play_mechanics_chonker     ; If '2' was pressed, jump to play_mechanics_chonker
				cmp choice, '3'
				je exit_chonker_program     ; If '3' was pressed, jump to exit_chonker_program
				jmp chonker_wait_for_key
				
			;Process on chonker game	
		play_start_chonker:
			call chonker_game	
			jmp exit_to_main_chonker
			jmp exit_program
				
				exit_to_main_chonker:
					call black_background
				
					mov dh, 20 ; row position (top)
					mov dl, 2 ; column position (left)
					call set_cursor
				
					lea dx, chonker_back_to_main_menu
					call display_screen
					
					mov dh, 22 ; row position (top)
					mov dl, 2 ; column position (left)
					call set_cursor
					
					lea dx, chonker_try_again
					call display_screen
				
					;read a character from the keyboard	
					call wait_for_keys
					
					cmp choice, 'X'
					je chonker_main_menu
					cmp choice, 'x'
					je chonker_main_menu
					
					cmp choice, 'A'
					je chonker_please_try_again
					cmp choice, 'a'
					je chonker_please_try_again
					
					jmp exit_to_main_chonker
				
				chonker_main_menu:
					jmp run_game1
					jmp main
				
				chonker_please_try_again:
					jmp play_start_chonker
					jmp main
		
		play_mechanics_chonker:
			call clear_screen
			call video_mode_black
			call mechanics_chonker_game
			
			mechanics_to_back_to_main_menu:
				mov dh, 22 ; row position (top)
				mov dl, 2 ; column position (left)
				call set_cursor
				
				mov dx, offset back_to_main_menu2
				call display_screen
				
				call wait_for_keys
			
				cmp choice, 'X'
				je chonker_main_menu2
				cmp choice, 'x'
				je chonker_main_menu2
				
				jmp mechanics_to_back_to_main_menu
			
			chonker_main_menu2:
				jmp run_game1
				jmp main
			
		
		exit_chonker_program:
			jmp main
				
;------------------------ END GAME 1 ------------------------------- END GAME 1 ----------------------------------------------
;-------------------------------------------------------------------------------------------------------------------------
;--------------------------------------------------------------------------------------------------------------------------	
	
	play_game2:
		call clear_screen
		jmp run_game2
		jmp main
		
		run_game2:
			; Code to start game 2 (TRUTH OR DARE)
			call clear_screen
			call video_mode_black
			call tod_game_menu
			
			;waits for a key to press
			tod_wait_for_key:
				call wait_for_keys
					
			;Process user choice
				cmp choice, '1'
				je play_start_tod       ; If '1' was pressed, jump to play_game1
				cmp choice, '2'
				je play_mechanics_tod      ; If '2' was pressed, jump to play_game2
				cmp choice, '3'
				je exit_tod_program     ; If '3' was pressed, jump to exit_program
				jmp tod_wait_for_key
					
			; Add a new label to handle the game refresh logic
				tod_main_menu2:
					jmp main 
				try_again2:
					jmp main
				
	play_start_tod:
		call clear_screen	
		call video_mode_black
		
		;Initialize blinking state to visible (1)
		mov bl, 1

		blinking_loop:
			;Check for keyboard input
			mov ah, 01h   ; Check for key press
			int 16h
			jnz check_space  ; If a key is pressed, check if it's space

			;Toggle visibility every certain interval (delay)
			;Delay for 250 milliseconds (adjust as needed)
			mov cx, 5000
		 delay_loop:
			loop delay_loop
  
			;Toggle the bl register between 0 and 1 for visibility
			xor bl, 1  ; Toggle bl between 0 and 1

			;Display or hide the text based on the bl register
			cmp bl, 1  ; Check if the text should be visible
			je show_text  ; If bl is 1, show the text
			jmp clear_text  ; Otherwise, clear the text

			show_text:
				;Display "Press 'Space' to Toss a Coin"
				mov dh, 17    ; Row position (top)
				mov dl, 6    ; Column position (left)
				call set_cursor

				lea dx, tod_start_game
				call display_screen
				jmp blinking_loop  ; Continue blinking loop

			clear_text:
				call black_background
				
				;Clear the text by printing spaces over it
				mov dh, 17    ; Row position (top)
				mov dl, 6    ; Column position (left)
				call set_cursor

				lea dx, spaces
				call display_screen
				
				jmp blinking_loop  ; Continue blinking loop

			check_space:	
				;wait to press "space" key
				call wait_for_keys
							
				cmp choice, ' '
				je tossing_loop
				jmp blinking_loop      ; Otherwise, continue blinking	
			tossing_loop:
				call clear_screen
				call coin_toss_function
				;make consequence
				jmp exit_to_main
				jmp exit_program
			
			;Add a new label to handle the game refresh logic
			tod_main_menu:
				jmp run_game2
				jmp tod_main_menu2 ; Return to main menu
			try_again:
				jmp play_start_tod
				jmp try_again2
				
			exit_to_main:
				mov dh, 20 ; row position (top)
				mov dl, 2 ; column position (left)
				call set_cursor
				
				mov dx, offset back_to_main_menu
				call display_screen
				
				mov dh, 22 ; row position (top)
				mov dl, 2 ; column position (left)
				call set_cursor
				
				mov dx, offset please_try_again
				call display_screen
			
			;read a character from the keyboard	
				call wait_for_keys
			
				cmp choice, 'X'
				je tod_main_menu
				cmp choice, 'x'
				je tod_main_menu
				
				cmp choice, 'A'
				je try_again
				cmp choice, 'a'
				je try_again
				
				jmp exit_to_main		
	
	play_mechanics_tod:
		call clear_screen	
		call video_mode_black
		call mechanics_tod_game
		
		mechanics_to_tod_menu:
				mov dh, 20 ; row position (top)
				mov dl, 2 ; column position (left)
				call set_cursor
				
				mov dx, offset back_to_main_menu
				call display_screen
				
				call wait_for_keys
			
				cmp choice, 'X'
				je tod_main_menu
				cmp choice, 'x'
				je tod_main_menu
				
				jmp mechanics_to_tod_menu
			
			
				
	exit_tod_program:
		jmp main

;----------------------------------- END GAME 2 ------ END GAME 2 -------- END GAME 2 ----------------------------------------------		
;--------------------------------------------------------------------------------------------------------------

;###################################################################################################################
;###################                            ####################################################################
;###################  GAME MAIN MENU PROCEDURE  ####################################################################
;###################                            ####################################################################
;###################################################################################################################

project_main_menu proc near 								
	call video_mode_black									
															
; 	show the main menu										
    mov dh, 4 ; row position (top)						
    mov dl, 15 ; column position (left)				   
    call set_cursor									
													
	lea dx, main_menu								
	call display_screen
	
; 	show the game1
    mov dh, 7 ; row position (top)
    mov dl, 5 ; column position (left)
	call set_cursor

	lea dx, game1
	call display_screen
	
; 	show the game2
    mov dh, 9 ; row position (top)
    mov dl, 5 ; column position (left)
	call set_cursor

	lea dx, game2
	call display_screen
	
; 	show the exit to dosbox
    mov dh, 11 ; row position (top)
    mov dl, 5 ; column position (left)
	call set_cursor

	lea dx, main_menu_exit
	call display_screen
	
; 	show the exit to dosbox
    mov dh, 17 ; row position (top)
    mov dl, 10 ; column position (left)
    call set_cursor

	lea dx, subject
	call display_screen
	
; 	show the exit to dosbox
    mov dh, 19 ; row position (top)
    mov dl, 2 ; column position (left)
    call set_cursor

	lea dx, prepared_by
	call display_screen
ret
project_main_menu endp

;###################################################################################################################
;###################                            ####################################################################
;###################     CHONKER PROCEDURES     ####################################################################
;###################                            ####################################################################
;###################################################################################################################

chonker_game proc near
	mov ah,00h ;set video mode
	mov al,00h ;text mode 40x25 chars
	int 10h
	
	;Next set-up the random number generator	
    call srandsystime   ; Seed PRNG with system time, call once only 

	;This is the start of the loop that will run continuously
	OuterLoop:	

	;draw some rocks
	mov CX, 10         ; CX holds the number of rocks to draw each loop.

drawRocks:
	push CX
    call rand           ; Get a random number in AX
 ;   call rand2num1to10  ; Convert AX to num between 1 and 10 (not used for now).
; I let the set_cursor call deal with the out of range cursor positions
; instead of normalizing the random position.

; draw a rock at a random location on the bottom of the screen
	mov  dl, AL      ; AL is the low byte of AX which contains the random number  
	mov  dh, 18h     ; 18h = 24 decimal which is the bottom row of the screen        
	call set_cursor  ; place the cursor at the random position on the bottom row
	mov AL, 'R'      ; prepare to print an "R"
	mov BH, 0		 ; page 0
	mov BL, 3		 ; color, 3=cyan
	mov CX,1         ; number of characters to print
	mov AH,09h		 ; we want the 09H (print with attribute software int)
	INT 10H			 ; do the software interrupt

; draw the next rock until they are all drawn.
	pop CX			 ; get the counter back from the stack
	LOOP drawRocks	 ; CX=CX-1, if CX>0 then loop back to drawRocks

; scroll the screen
    mov CH,0h		 ; set the scrolling window to the upper-left corner
    mov CL,0h
    mov DH,18h		 	
    mov DL,4Fh		 ; down to the lower right corner.
    mov AL,1h		 ; set the number of lines to scroll to one
    mov AH,06H		 ; this is the int type (scroll)
    INT 10h			 ; call the software interrupt

; see if a rock hit the chonker
	mov  dl, xpos    ; set the cursor position x-coordinate to the xpos of the chonker
	mov  dh, 11h     ; in this version the chonker is always at ypos 11h        
	call set_cursor  ; move the cursor to the chonker's position
	mov BH,0         ; choose page zero
	mov AH,08h       ; we do a 08h call which reads the character at the cursor position
	int 10h			 ; do the software interrupt
	cmp AL,'R'       ; AL contains the ascii code for the character at the chonker location.
	je terminate     ; if there is an "R" at the chonker location, the chonker is terminated
	
; if chonker is safe and position conditions are adequate, print chonker  
	cmp xpos, 0H 		; if chonker is at left edge of screen (0h) 
	je draw_tunnel		; then dont print chonker as he/she has not moved & skip to printing just the tunnel
	cmp xpos, 4FH		; if chonker is at right edge of screen (4fh)
	je draw_tunnel		; then dont print chonker as he/she has not moved & skip to printing just the tunnel
	JMP draw_chonker	; if chonker is not at edges, print the chonker

; if chonker is at wall already then dont print the chonker #, print the tunnel X
draw_tunnel:
	mov  dl, xpos    	; if the chonker didn't move we'll draw an "X" for the tunnel behind him
	mov  dh, 10h     	; draw the tunnel at chonker ypos 11h-1 = 10h row position so the chonker cursor is not overridden         
	call set_cursor  	; place the cursor
	mov AL, 'X'      	; we'll print an "X"
	mov BH, 0		 	; page
	mov BL, 7		 	; color=white
	mov CX,1		 	; 1 copy of "X"
	mov AH,09h		 	; print char with attribute call
	INT 10H			 	; video INT
	JMP draw_chonker 	; draw the chonker cursor now

; if chonker is safe & not on wall edges, draw the chonker
draw_chonker:
	mov  dl, xpos    	; set the cursor position x-coordinate to the xpos of the chonker
	mov  dh, 11h     	; in this version the chonker is always at ypos 11h           
	call set_cursor  	; move the cursor to the chonker's position
	mov AL, '#'		 	; we'll print "#" for the chonker character
	mov BH, 0		 	; use page 0
	mov BL, 7		 	; use white as the color
	mov CX,1		 	; just one chonker
	mov AH,09h		 	; AH=09h will print a character
	INT 10H			 	; do the software interrupt
	JMP continue		; continue with OuterLoop execution

; after dealing with the chonker cursor and adequate wall conditions, continue with code
continue:	

; We wait 0.1 second.	
	CALL delay		 ; delay 0.1 s

; If the "q" is pressed, end the program otherwise loop through the code again.

;CHECK IF KEY WAS PRESSED.
	mov ah, 0bh  ; int type 0bh will see if a key was pressed
  	int 21h      ; RETURNS AL=0 : NO KEY PRESSED, AL!=0 : KEY PRESSED.
  	cmp al, 0
  	je  noKey

;PROCESS KEY.        
  	mov ah, 0	 	; AH=0 will call 
  	int 16h      	; GET THE KEY.
	cmp AL, 'q'     ; terminate if 'q' key has pressed     
	je terminate
	cmp AL, 'a'  	; key pressed was "a"?
	je checkleft    ; check x position (if at left edge) and act accordingly
	cmp AL, 'd'  	; key pressed was "s"?
	je checkright   ; check x position (if at right edge) and act accordingly

noKey:
	mov  dl, xpos    ; if the chonker didn't move we'll draw an "X" for the tunnel behind him
	mov  dh, 11h     ; row position        
	call set_cursor  ; place the cursor
	mov AL, 'X'      ; we'll print an "X"
	mov BH, 0		 ; page
	mov BL, 7		 ; color=white
	mov CX,1		 ; 1 copy of "X"
	mov AH,09h		 ; print char with attribute call
	INT 10H			 ; video INT
	
	JMP OuterLoop	 ; continue the game

; is chonker at the LEFT edge of the screen
checkleft:
	cmp xpos, 0H	 ; see if chonker x coordinate equals that of left screen limit
	jne moveleft	 ; if not equal to 0H, move left
	JMP OuterLoop    ; else continue the game without moving left or right

moveleft:
	mov  dl, xpos    ; move chonker xpos to the column
	mov  dh, 11h     ; keep the chonker on row 11h        
	call set_cursor  ; place the cursor at the chonker position
	mov AL, 'X'      ; draw the tunnel as an "X"
	mov BH, 0        ; page 0
	mov BL, 7        ; 7=white
	mov CX,1         ; one character will print
	mov AH,09h       ; print char with attribute
	INT 10H          ; print the char
	DEC xpos         ; move left

	JMP OuterLoop

; is chonker at the RIGHT edge of the screen 
checkright:
	cmp xpos, 4FH	 ; see if chonker x coordinate equals that of right screen limit
	jne moveright	 ; if equal to 4FH, move right
	JMP OuterLoop    ; else continue the game without moving left or right

moveright:
	mov  dl, xpos    ; move chonker xpos to the column   
	mov  dh, 11h     ; keep the chonker on row 11h             
	call set_cursor  ; place the cursor at the chonker position
	mov AL, 'X'      ; draw the tunnel as an "X"
	mov BH, 0        ; page 0
	mov BL, 7        ; 7=white
	mov CX,1         ; one character will print
	mov AH,09h       ; print char with attribute
	INT 10H          ; print the char
	INC xpos         ; move right

	JMP OuterLoop    ; continue the game

terminate:
	call clear_screen
	call video_mode_black
		
	mov AL, 1		 ; the following lines set up for the string print
	mov BH,0
	mov BL,6
	mov CX, nSize

	MOV DX, @data
	MOV ES, DX
	
	mov dh, 5 ; row position (top)
	mov dl, 15 ; column position (left)
	call set_cursor

	MOV BP, OFFSET msg

	mov AH, 13h              ; Print the "Chonker Terminated" message.
	int 10h

ret
chonker_game ENDP

;mechanics of the chonker game
mechanics_chonker_game proc near
	mov dh, 2 ; row position (top)
	mov dl, 2 ; column position (left)
	call set_cursor
			
	lea dx, chonker_game_mechanics
	call display_screen
			
	mov dh, 5 ; row position (top)
	mov dl, 1 ; column position (left)
	call set_cursor
			
	lea dx, mechanics_chonker1
	call display_screen
ret
mechanics_chonker_game endp

chonker_game_menu proc near	
	mov ah, 02h ; set function to set cursor position
	mov dh, 4 ; row position (top)
	mov dl, 10 ; column position (left)
	mov bh, 0 ; page number
	int 10h ; call BIOS interrupt to set cursor position
			
	mov ah, 09h
	lea bx, prompt_menu_chonker
	int 21h
	;shows the start game
	mov ah, 02h ; set function to set cursor position
	mov dh, 8 ; row position (top)
	mov dl, 6 ; column position (left)
	mov bh, 0 ; page number
	int 10h ; call BIOS interrupt to set cursor position
			
	mov ah, 09h
	lea dx, start_game_chonker
	int 21h
			
	;shows the mechanics 
	mov ah, 02h ; set function to set cursor position
	mov dh, 10 ; row position (top)
	mov dl, 6 ; column position (left)
	mov bh, 0 ; page number
	int 10h ; call BIOS interrupt to set cursor position
			
	mov ah, 09h
	lea dx, mechanics_chonker
	int 21h
			
	;shows the exit to main menu2
	mov ah, 02h ; set function to set cursor position
	mov dh, 12 ; row position (top)
	mov dl, 6 ; column position (left)
	mov bh, 0 ; page number
	int 10h ; call BIOS interrupt to set cursor position
			
	mov ah, 09h
	lea dx, chonker_exit_program
	int 21h
					
ret
chonker_game_menu endp

;Delay procedure
delay proc
	MOV CX, 01h		; 0186A0h = 100000d = .1s delay
	MOV DX, 86A0h
	MOV AH, 86h
	INT 15h	; 0.1 seconds delay	
RET
delay ENDP
	
;###################################################################################################################
; The random number code subroutines were taken from
; https://stackoverflow.com/questions/47607104/random-number-in-assembly

; This first routine is used to
; return number between 1 and 10
;  This routine is not used in the chonker game.
; I left it in the code for later use (maybe).  
; Inputs:   AX = value to convert
; Return:   (AX) value between 1 and 10

rand2num1to10 proc
	push dx
	push bx
	xor dx,dx           ; Compute randval(DX) mod 10 to get num
	mov bx,10           ;     between 0 and 9
	div bx
	inc dx              ; DX = modulo from division
							;     Add 1 to give us # between 1 and 10 (not 0 to 9)
	mov ax,dx
	pop bx
	pop dx
ret
rand2num1to10 endp
	
; Set LCG PRNG seed to system timer ticks
; Inputs:   AX = seed
; Modifies: AX 
; Return:   nothing 

srandsystime proc
    push cx
    push dx
    xor ax, ax          ; Int 1Ah/AH=0 to get system timer in CX:DX 
    int 1ah
    mov [seed], dx      ; seed = 16-bit value from DX
    pop dx
    pop cx
ret
srandsystime endp

; Updates seed for next iteration
;     seed = (multiplier * seed + increment) mod 65536
;     multiplier = 25173, increment = 13849
;
; Inputs: none
; Return: (AX) random value

rand proc
	push dx
	mov ax, 25173       ; LCG Multiplier
	mul word ptr [seed] ; DX:AX = LCG multiplier * seed
	add ax, 13849       ; Add LCG increment value
	mov [seed], ax      ; Update seed
	; AX = (multiplier * seed + increment) mod 65536
	pop dx
ret
rand endp

;###################################################################################################################
;###################                            ####################################################################
;###################  TRUTH OR DARE PROCEDURES  ####################################################################
;###################                            ####################################################################
;###################################################################################################################

;shows the truth or dare menu
tod_game_menu proc near
	mov dh, 4 ; row position (top)
	mov dl, 10 ; column position (left)
	call set_cursor
			
	lea dx, prompt_menu_tod
	call display_screen
			
	;shows the start game
	mov dh, 8 ; row position (top)
	mov dl, 6 ; column position (left)
	call set_cursor
			
	lea dx, start_game_tod
	call display_screen
			
	;shows the mechanics 
	mov dh, 10 ; row position (top)
	mov dl, 6 ; column position (left)
	call set_cursor
			
	lea dx, mechanics_tod
	call display_screen
			
	;shows the exit to main menu
	mov dh, 12 ; row position (top)
	mov dl, 6 ; column position (left)
	call set_cursor
			
	lea dx, tod_exit_program
	call display_screen	
ret
tod_game_menu endp

;Shows the mechanics of the truth and dare game
mechanics_tod_game proc near
	mov dh, 2 ; row position (top)
	mov dl, 2 ; column position (left)
	call set_cursor
			
	lea dx, TOD_game_mechanics
	call display_screen
			
	mov dh, 5 ; row position (top)
	mov dl, 1 ; column position (left)
	call set_cursor
			
	lea dx, mechanics_tod1
	call display_screen
			 
	mov dh, 7 ; row position (top)
	mov dl, 1 ; column position (left)
	call set_cursor
			
	lea dx, mechanics_tod2
	call display_screen
			
	mov dh, 9 ; row position (top)
	mov dl, 1 ; column position (left)
	call set_cursor
			
	lea dx, mechanics_tod3
	call display_screen	
	
	mov dh, 11 ; row position (top)
	mov dl, 1 ; column position (left)
	call set_cursor
			
	lea dx, mechanics_tod4
	call display_screen	
	
	mov dh, 15 ; row position (top)
	mov dl, 1 ; column position (left)
	call set_cursor
			
	lea dx, mechanics_tod5
	call display_screen	
ret
mechanics_tod_game endp
	
;Displaying the starting of the game
coin_toss_function proc near
	call video_mode_black
	
	mov dh, 2 ; row position (top)
	mov dl, 5 ; column position (left)
	call set_cursor
		
	mov dx, offset tod_toss_coin 
	call display_screen
		
	mov dh, 4 ; row position (top)
	mov dl, 5 ; column position (left)
	call set_cursor

	mov dx, offset toss_coin_result
	call display_screen
		
	mov dh, 6 ; row position (top)
	mov dl, 12 ; column position (left)
	call set_cursor
		
	mov ah, 00h   ; get system timer
	int 1Ah
	mov ax, dx    ; use DX as RNG seed

	; Generate a random number between 0 and 1
	and ax, 0001h ; ensure only the least significant bit is considered

	; Check the least significant bit for either Heads or Tails
	cmp ax, 0     ; compare with 0
	je print_heads; if equal, print Heads
	jmp print_tails;

	print_heads:
	;Code to print "Heads" message
		lea dx, result_one
		call display_screen
			
		mov dh, 8 ; row position (top)
		mov dl, 6 ; column position (left)
		call set_cursor
			
		lea dx, result_heads ; Load the message for truth question
		call display_screen
		
		mov ax, num_truth
		call random_truth
			
		push ax ; Save the value of ax
        push bx ; Save the value of bx
		
		mov dh, 12 ; row position (top)
		mov dl, 8 ; column position (left)
		call set_cursor			
		
			
		mov si, offset truth_questions ; Load the base address of the array
		mov ax, bx ; Copy the random number to ax
		shl ax, 1 ; Multiply by 2
		add si, ax ; Add to the base address
		mov si, [si] ; Load the offset of the question
		mov dx, si ; Load the offset of the question
		
		call display_screen

        pop bx ; Restore the value of bx
        pop ax ; Restore the value of ax

		jmp end_coin_toss  ; Jump to the end of the coin toss handling

		print_tails:
			;Code to print "Tails" message
			lea dx, result_two
			call display_screen
			
			mov dh, 8 ; row position (top)
			mov dl, 6 ; column position (left)
			call set_cursor
			
			lea dx, result_tails ; Load the message for dare challenge
			call display_screen

			mov ax, num_dare	
			call random_dare
			
		push ax ; Save the value of ax
        push bx ; Save the value of bx
		
		mov dh, 12 ; row position (top)
		mov dl, 8 ; column position (left)
		call set_cursor			
			
		mov si, offset dare_challenges ; Load the base address of the array
		mov ax, bx ; Copy the random number to ax
		shl ax, 1 ; Multiply by 2
		add si, ax ; Add to the base address
		mov si, [si] ; Load the offset of the question
		mov dx, si ; Load the offset of the question
		
		call display_screen

        pop bx ; Restore the value of bx
        pop ax ; Restore the value of ax


		end_coin_toss:
ret
coin_toss_function endp

; Random truth question using system timer
random_truth proc near
   mov ax, 00h
    int 1ah ;get number of clock ticks since midnight in dx
    mov ax, dx
    xor dx, dx ;clears the clock back to zero
    mov cx, num_truth ; Use the correct range
    div cx ;divide dx:ax by cx, get quotient in ax and remainder in dx
    mov bx, dx ;move the remainder to bl
    ret
random_truth endp

; random dare challenge using system timer
random_dare proc near
   mov ax, 00h
    int 1ah ;get number of clock ticks since midnight in dx
    mov ax, dx
    xor dx, dx
    mov cx, num_dare ; Use the correct range
    div cx ;divide dx:ax by cx, get quotient in ax and remainder in dx
    mov bx, dx ;move the remainder to bl
    ret
ret
random_dare endp

;###################################################################################################################
;###################                            ####################################################################
;###################    INTERRUPTS PROCEDURES   ####################################################################
;###################                            ####################################################################
;###################################################################################################################



;Wait for users to press the key
	wait_for_keys proc near
		mov ah, 00h
		int 16h
		mov choice, al
	ret
	wait_for_keys endp

;Set cursor PROCEDURE
	set_cursor proc near
		mov ah, 02h ; set function to set cursor position
		mov bh, 0 ; page number
		int 10h ; call BIOS interrupt to set cursor position
	ret
	set_cursor endp


;Display to the screen
	display_screen proc near
		mov ah, 09h
		int 21h
	ret
	display_screen endp
;Set video mode and black background
	video_mode_black proc near
		call video_mode
		call black_background
	ret
	video_mode_black endp

;Set Video mode into 5h (grayscale)
	video_mode proc near
		mov ah, 00h
		mov al, 05h
		int 10h
	ret
	video_mode endp
	
;Set blackground color to black
	black_background proc near			
		mov ah, 0Bh ; Set the configuration
		mov bh, 00h ; To the background color
		mov bl, 00h ; Black background color
		int 10h     ; Video Services - Set Video Mode
	ret
	black_background endp
	
;Set clear screen procedure
	clear_screen proc near
		mov ah, 06h         ; clear screen
		mov al, 0
		mov bh, 07h
		mov cx, 0
		mov dx, 184fh
		int 10h
		ret
	clear_screen endp

;Ternimate the program
	exit_process proc near
		mov ah, 00h
		mov al, 02h
		int 10h
		
		mov ah,4Ch
		int 21h
	exit_process endp
	
main endp
end main