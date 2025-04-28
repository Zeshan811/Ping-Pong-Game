org 0x0100
jmp start
init_ball_pos: dw 1998
ball_pos: dw 1998
add_factor: dw 162
p1_pos: dw 1440, 1600, 1760, 1920, 2080
p2_pos: dw 1598, 1758, 1918, 2078, 2238
p1_score: dw 0
p2_score: dw 0
max_score: dw 0
p: db 0
old_timer_offset: dw 0
old_timer_segment: dw 0
point_for_player: db '0'
msg: db 'Score: '
p1_name: db 'Player_1 Score: ', 0
p2_name: db 'Player_2 Score: ', 0
p1_wins: db 'Player 1 wins'
p2_wins: db 'Player 2 wins'
enter_max_score: db 'Enter Max Score: ', 0
draw_game:
     push bp
     mov bp, sp
     push ax
     push bx
     push cx
     push dx
     push si
     push di
     push es
     init_video_memory:
          mov ax, 0xB800
          mov es, ax
          xor di, di
     
     mov cx, 5
     mov si, p1_pos
     draw_p1:
          mov di, [si]
          mov word[es:di], 0x1020
          add si, 2
          add di, 2
          loop draw_p1
     
     mov cx, 5
     mov si, p2_pos
     draw_p2:
          mov di, [si]
          mov word[es:di], 0x4020
          add si, 2
          add di, 2
          loop draw_p2
     
     mov ah, 0x75
     mov al, 'O'
     draw_ball:
          mov di, [ball_pos]
          mov [es:di], ax
     
     end_draw:
          pop es
          pop di
          pop si
          pop dx
          pop cx
          pop bx
          pop ax
          pop bp
          ret

clr:
     push es
     push di
     push ax
     push cx
     mov ax, 0xb800
     mov es, ax
     mov ax, 0x7720
     xor di, di
     mov cx, 3840
     lup:
          mov [es:di], ax
          add di, 2
          loop lup

     ;line 1 and last line must be the walls
   ;  mov ah, 0x75
    ; mov al, '#'

     mov cx, 80
     mov di, 160
     lupa:
          mov [es:di], ax
          add di, 2
          loop lupa
     mov cx, 80
     mov di, 3840
     lupb:
          mov [es:di], ax
          add di, 2
          loop lupb

     
     end_clr:
          pop cx
          pop ax
          pop di
          pop es
          ret

kb_hit:
     push ax
     push bx

     mov byte[p], 1; for p1
     mov bx, p2_pos
     push bx

     mov ah, 0x00
     int 0x16; kb-input
     
     cmp ah, 0x48
     je up
     cmp ah, 0x50
     je down
     cmp ah, 0x4B
     je left
     cmp ah, 0x4D
     je right


     mov byte[p], 2; for p2
     mov bx, p1_pos
     push bx


     cmp al, 'w'
     je up
     cmp al, 's'
     je down
     cmp al, 'a'
     je left
     cmp al, 'd'
     je right
     pop_p1_pos:
          pop bx; p1_pos
     end_kb_hit:
          pop bx; p2_pos
          pop bx
          pop ax
          ret
up:
     call move_up
     cmp byte[p], 2
     je pop_p1_pos
     jmp end_kb_hit
down:
     call move_down
     cmp byte[p], 2
     je pop_p1_pos
     jmp end_kb_hit
left:
     call move_left
     cmp byte[p], 2
     je pop_p1_pos
     jmp end_kb_hit
right:
     call move_right
     cmp byte[p], 2
     je pop_p1_pos
     jmp end_kb_hit

move_up:
     push bp
     mov bp, sp
     push si
     push cx
     mov cx, 5
     mov si, word[bp+4]
     cmp word[si], 480
     jl end_up
     l1:
          sub word[si],160
          add si, 2
          loop l1
     end_up:
          pop cx
          pop si
          pop bp
          ret

move_down:
     push bp
     mov bp, sp
     push si
     push cx
     mov cx, 5
     mov si, word[bp+4]
     cmp word[si+4], 3358
     jg end_down
     l2:
          add word[si],160
          add si, 2
          loop l2
     end_down:
          pop cx
          pop si
          pop bp
          ret

move_left:
     push bp
     mov bp, sp
     push si
     push cx
     mov cx, 5
     mov si, word[bp+4]
     mov ax, [si]
     lup2:
          sub ax, 160
          cmp ax, 158
          jg lup2
     cmp ax, 0
     je end_left
     l4:
          sub word[si],2
          add si, 2
          loop l4
     end_left:
          pop cx
          pop si
          pop bp
          ret

move_right:
     push bp
     mov bp, sp
     push si
     push cx
     mov cx, 5
     mov si, word[bp+4]
     mov ax, [si]
     add ax, 2
     lup1:
          sub ax, 160
          cmp ax, 158
          jg lup1
     cmp ax, 0
     je end_right
     l3:
          add word[si],2
          add si, 2
          loop l3
     end_right:
          pop cx
          pop si
          pop bp
          ret

ball_control:
     push ax
     push dx
     push cx
     mov ax, [ball_pos]
     
     ;pad_bound:
     mov dx, ax
     sub dx, 2
     mov cx, ax
     add cx, 2

     cmp dx, word[p1_pos]
     je chng_dir_left_to_right
     cmp dx, word[p1_pos+2]
     je chng_dir_left_to_right
     cmp dx, word[p1_pos+4]
     je chng_dir_left_to_right
     cmp dx, word[p1_pos+6]
     je chng_dir_left_to_right
     cmp dx, word[p1_pos+8]
     je chng_dir_left_to_right
     
     cmp cx, word[p2_pos]
     je chng_dir_right_to_left
     cmp cx, word[p2_pos+2]
     je chng_dir_right_to_left
     cmp cx, word[p2_pos+4]
     je chng_dir_right_to_left
     cmp cx, word[p2_pos+6]
     je chng_dir_right_to_left
     cmp cx, word[p2_pos+8]
     je chng_dir_right_to_left

     ;upper and lower bound
     cmp ax, 3678
     jge chng_dir_down_to_up
     cmp ax, 480
     jle chng_dir_up_to_down
     jmp n

     chng_dir_down_to_up:
          mov bx, [add_factor]
          sub bx, 320
          mov [add_factor], bx
     jmp n

     chng_dir_up_to_down:
          mov bx, [add_factor]
          add bx, 320
          mov [add_factor], bx
     jmp n

     chng_dir_right_to_left:
          mov bx, [add_factor]
          sub bx, 4
          mov [add_factor], bx
     jmp n

     chng_dir_left_to_right:
          mov bx, [add_factor]
          add bx, 4
          mov [add_factor], bx
     jmp n
     n: 
   
         add ax, [add_factor]
          mov [ball_pos], ax
     end_control:
          pop cx
          pop dx
          pop ax
          ret

Re_initialize_ball:
     push ax
     push bx

     mov ax, [ball_pos]
     mov bl, 160
     div bl
     mov byte[point_for_player], 2; point goes to player 2
     cmp ah, 0
     je re_init

     mov ax, [ball_pos]
     sub ax, 158
     div bl
     mov byte[point_for_player], 1; point goes to player 1
     cmp ah, 0
     je re_init

     jmp end_re
     re_init:
          mov ax, [init_ball_pos]
          mov [ball_pos], ax
          cmp byte[point_for_player], 1
          je point_for_p1
          inc word[p2_score]
          jmp end_re
          point_for_p1:
               inc word[p1_score]
     end_re:
          pop bx
          pop ax
          ret

timer_handler:
    push ax
    push bx
    push cx
    push dx
    push si
    push di
    push es

     call clr
     call display_scores
     mov ax, [max_score]
     cmp word[p1_score], ax
     je near restore_timer
     cmp word[p2_score], ax
     je near restore_timer
    call ball_control
    call Re_initialize_ball
    draw:
    call draw_game
    ; Acknowledge the interrupt
    mov al, 20h
    out 20h, al

    pop es
    pop di
    pop si
    pop dx
    pop cx
    pop bx
    pop ax
    iret

; Subroutine to display scores for both players
display_scores:
    pusha

    ; Set video memory segment
    mov ax, 0xB800
    mov es, ax

     mov si, p1_name
     mov di, 0x0000
     lupaa:
          mov al, byte[si]
          mov ah, 0x79
          cmp al, 0
          je n1
          stosw
          inc si
          jmp lupaa

     n1:
    ; Display Player 1 Score
    mov di, 28         ; Screen offset for Player 1 score (Row 3, Column 20)
    mov ax, [p1_score]  ; Load Player 1 score
    call display_number ; Convert and display the number

     mov si, p2_name
     mov di, 80
     lupbb:
          mov al, byte[si]
          mov ah, 0x79
          cmp al, 0
          je n2
          stosw
          inc si
          jmp lupbb

     n2:
    ; Display Player 2 Score
    mov di, 108        ; Screen offset for Player 2 score (Row 22, Column 20)
    mov ax, [p2_score]  ; Load Player 2 score
    call display_number ; Convert and display the number

    ; Restore registers and return
    popa
    ret

; Subroutine to display a number
; Input: AX = number to display, DI = screen offset
display_number:
    push ax
    push bx
    push cx
    push dx

    xor cx, cx          ; Clear CX (digit count)
    mov bx, 10          ; Base 10 for division

    ; Convert number to ASCII (reverse order)
    number_to_ascii:
        xor dx, dx      ; Clear remainder
        div bx          ; Divide AX by 10 (AX = quotient, DX = remainder)
        add dl, '0'     ; Convert remainder to ASCII
        push dx         ; Save digit on stack
        inc cx          ; Increment digit count
        test ax, ax     ; Check if AX (quotient) is zero
        jnz number_to_ascii

    ; Display digits (correct order)
    mov ah, 0x79        ; Attribute color
    display_digits:
        pop ax          ; Get digit from stack
        mov ah, 0x79        ; Attribute color
        mov [es:di], ax ; Write digit and attribute to video memory
        add di, 2       ; Move to next position
        loop display_digits

    ; Restore registers and return
    pop dx
    pop cx
    pop bx
    pop ax
    ret


delay:
     pusha

     mov cx, 10
     mov dx, 0xFFFF
     luup1:
          luup2:
               dec dx
               cmp dx, 0
               jne luup2
          loop luup1
     
     popa
     ret

print_over:
     pusha
     mov ax, cs
     mov ds, ax
     mov ax, word[p1_score]
     mov bx, word[p2_score]
     cmp ax, bx
     jl nxt4

     mov si, p1_wins
     mov ax, 0xb800
     mov es, ax
     mov di, 1820
     mov cx, 12
     luup3:
          mov al, byte[si]
          mov ah, 0xF4
          stosw
          inc si
          loop luup3
     jmp end_over

     nxt4:
     mov si, p2_wins
     mov ax, 0xb800
     mov es, ax
     mov di, 1820
     mov cx, 12
     luup4:
          mov al, byte[si]
          mov ah, 0xF4
          stosw
          inc si
          loop luup4
     end_over:
          popa
          ret
input_max_score:
     pusha
     mov ax, 0xb800
     mov es, ax
     xor di,di
     mov si, enter_max_score
     luuuup:
          cmp byte[si], 0
          je luuup
          mov al, byte[si]
          mov ah, 0xf4
          mov [es:di], ax
          add di, 2
          inc si
          jmp luuuup

     luuup:
          mov ah, 0x00
          int 0x16
          cmp al, 0x0D
          je end_max
          cmp al, 0x30
          jl luuup
          cmp al, 0x39
          jg luuup
          mov ah, 0x00
          sub al, 0x30
          mov bx, ax
          mov ax, [max_score]
          mov cx, 10
          mul cx
          mov [max_score], ax
          add [max_score], bx
          mov bh, 0x07
          add bx, 0x30
          mov [es:di], bx
          add di, 2
     jmp luuup

     end_max:
          popa
          ret
start:
     call clr
     call input_max_score
     cli                 ; Disable interrupts
    push ax
    push es

    ; Get the current vector for INT 08h
    mov ax, 0
    mov es, ax
    mov bx, [es:08h * 4]        ; Store the old offset
    mov word [old_timer_offset], bx
    mov bx, [es:08h * 4 + 2]    ; Store the old segment
    mov word [old_timer_segment], bx

    ; Install the new handler
    mov word [es:0x08 * 4], timer_handler
    mov word [es:0x08 * 4 + 2], cs

    pop es
    pop ax
    sti

     l:
          call clr
          call draw_game
          call kb_hit
          jmp l
restore_timer:
     call delay
     call print_over
    cli
    push ax
    push es

    ; Restore the original INT 08h handler
    mov ax, 0
    mov es, ax
    mov bx, [old_timer_offset]
    mov [es:08h * 4], bx
    mov bx, [old_timer_segment]
    mov [es:08h * 4 + 2], bx

    pop es
    pop ax
    sti
     
end:
     mov ax, 0x4c00
     int 0x21