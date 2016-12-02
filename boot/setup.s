!
!	setup.s		(C) 1991 Linus Torvalds
!
! setup.s is responsible for getting the system data from the BIOS,
! and putting them into the appropriate places in system memory.
! both setup.s and system has been loaded by the bootblock.
!
! This code asks the bios for memory/disk/other parameters, and
! puts them in a "safe" place: 0x90000-0x901FF, ie where the
! boot-block used to be. It is then up to the protected mode
! system to read them from there before the area is overwritten
! for buffer-blocks.
!

! NOTE! These had better be the same as in bootsect.s!

INITSEG  = 0x9000	! we move boot here - out of the way
SYSSEG   = 0x1000	! system loaded at 0x10000 (65536).
SETUPSEG = 0x9020	! this is the current segment

.globl begtext, begdata, begbss, endtext, enddata, endbss
.text
begtext:
.data
begdata:
.bss
begbss:
.text

!!! SZ: Print some inane message
	call read_cursor_pos
	mov cx,#21
	mov bp,#msg1
	call print_str_cx_bp


entry start
start:

! ok, the read went well so we get current cursor position and save it for
! posterity.

	mov	ax,#INITSEG	! this is done in bootsect already, but...
	mov	ds,ax
	mov	ah,#0x03	! read cursor pos
	xor	bh,bh
	int	0x10		! save it in known place, con_init fetches
	mov	[0],dx		! it from 0x90000.

!!! SZ: print cursor pos
	! SZ:print "cursor_pos:"
	call read_cursor_pos
	mov cx,#14
	mov bp,#msg_cursor_pos
	call print_str_cx_bp

	! SZ: print num
	mov ax,#INITSEG
	mov bx,#0x00
	call print_num_ax_bx_4

! Get memory size (extended mem, kB)

	mov	ah,#0x88
	int	0x15
	mov	[2],ax

!!! SZ: print memory size
	! SZ:print "memory size: "
	call read_cursor_pos
	mov cx,#24
	mov bp,#msg_memory
	call print_str_cx_bp

	! SZ: print num
	mov ax,#INITSEG
	mov bx,#0x02
	call print_num_ax_bx_4

	! SZ:print "KB"
	call read_cursor_pos
	mov cx,#3
	mov bp,#msg_KB
	call print_str_cx_bp


! Get video-card data:

	mov	ah,#0x0f
	int	0x10
	mov	[4],bx		! bh = display page
	mov	[6],ax		! al = video mode, ah = window width

!!! SZ: print display page
	! SZ:print "display page: "
	call read_cursor_pos
	mov cx,#16
	mov bp,#msg_display_page
	call print_str_cx_bp

	! SZ: print num
	mov ax,#INITSEG
	mov bx,#0x04
	call print_num_ax_bx_4


!!! SZ: print video mode
	! SZ:print "video mode: "
	call read_cursor_pos
	mov cx,#14
	mov bp,#msg_video_mode
	call print_str_cx_bp

	! SZ: print num
	mov ax,#INITSEG
	mov bx,#0x06
	call print_num_ax_bx_2

!!! SZ: print window width
	! SZ:print "window width: "
	call read_cursor_pos
	mov cx,#16
	mov bp,#msg_window_width
	call print_str_cx_bp

	! SZ: print num
	mov ax,#INITSEG
	mov bx,#0x07
	call print_num_ax_bx_2

	call print_CR
	call print_CR
	call print_CR
	call print_CR
	call print_CR
	call print_CR
	

! check for EGA/VGA and some config parameters

	mov	ah,#0x12
	mov	bl,#0x10
	int	0x10
	mov	[8],ax
	mov	[10],bx
	mov	[12],cx

! Get hd0 data

	mov	ax,#0x0000
	mov	ds,ax
	lds	si,[4*0x41]
	mov	ax,#INITSEG
	mov	es,ax
	mov	di,#0x0080
	mov	cx,#0x10
	rep
	movsb

! Get hd1 data

	mov	ax,#0x0000
	mov	ds,ax
	lds	si,[4*0x46]
	mov	ax,#INITSEG
	mov	es,ax
	mov	di,#0x0090
	mov	cx,#0x10
	rep
	movsb

! Check that there IS a hd1 :-)

	mov	ax,#0x01500
	mov	dl,#0x81
	int	0x13
	jc	no_disk1
	cmp	ah,#3
	je	is_disk1
no_disk1:
	mov	ax,#INITSEG
	mov	es,ax
	mov	di,#0x0090
	mov	cx,#0x10
	mov	ax,#0x00
	rep
	stosb
is_disk1:

! now we want to move to protected mode ...

	cli			! no interrupts allowed !

! first we move the system to it's rightful place

	mov	ax,#0x0000
	cld			! 'direction'=0, movs moves forward
do_move:
	mov	es,ax		! destination segment
	add	ax,#0x1000
	cmp	ax,#0x9000
	jz	end_move
	mov	ds,ax		! source segment
	sub	di,di
	sub	si,si
	mov 	cx,#0x8000
	rep
	movsw
	jmp	do_move

! then we load the segment descriptors

end_move:
	mov	ax,#SETUPSEG	! right, forgot this at first. didn't work :-)
	mov	ds,ax
	lidt	idt_48		! load idt with 0,0
	lgdt	gdt_48		! load gdt with whatever appropriate

! that was painless, now we enable A20

	call	empty_8042
	mov	al,#0xD1		! command write
	out	#0x64,al
	call	empty_8042
	mov	al,#0xDF		! A20 on
	out	#0x60,al
	call	empty_8042

! well, that went ok, I hope. Now we have to reprogram the interrupts :-(
! we put them right after the intel-reserved hardware interrupts, at
! int 0x20-0x2F. There they won't mess up anything. Sadly IBM really
! messed this up with the original PC, and they haven't been able to
! rectify it afterwards. Thus the bios puts interrupts at 0x08-0x0f,
! which is used for the internal hardware interrupts as well. We just
! have to reprogram the 8259's, and it isn't fun.

	mov	al,#0x11		! initialization sequence
	out	#0x20,al		! send it to 8259A-1
	.word	0x00eb,0x00eb		! jmp $+2, jmp $+2
	out	#0xA0,al		! and to 8259A-2
	.word	0x00eb,0x00eb
	mov	al,#0x20		! start of hardware int's (0x20)
	out	#0x21,al
	.word	0x00eb,0x00eb
	mov	al,#0x28		! start of hardware int's 2 (0x28)
	out	#0xA1,al
	.word	0x00eb,0x00eb
	mov	al,#0x04		! 8259-1 is master
	out	#0x21,al
	.word	0x00eb,0x00eb
	mov	al,#0x02		! 8259-2 is slave
	out	#0xA1,al
	.word	0x00eb,0x00eb
	mov	al,#0x01		! 8086 mode for both
	out	#0x21,al
	.word	0x00eb,0x00eb
	out	#0xA1,al
	.word	0x00eb,0x00eb
	mov	al,#0xFF		! mask off all interrupts for now
	out	#0x21,al
	.word	0x00eb,0x00eb
	out	#0xA1,al

! well, that certainly wasn't fun :-(. Hopefully it works, and we don't
! need no steenking BIOS anyway (except for the initial loading :-).
! The BIOS-routine wants lots of unnecessary data, and it's less
! "interesting" anyway. This is how REAL programmers do it.
!
! Well, now's the time to actually move into protected mode. To make
! things as simple as possible, we do no register set-up or anything,
! we let the gnu-compiled 32-bit programs do that. We just jump to
! absolute address 0x00000, in 32-bit protected mode.
	mov	ax,#0x0001	! protected mode (PE) bit
	lmsw	ax		! This is it!
	jmpi	0,8		! jmp offset 0 of segment 8 (cs)

! This routine checks that the keyboard command queue is empty
! No timeout is used - if this hangs there is something wrong with
! the machine, and we probably couldn't proceed anyway.
empty_8042:
	.word	0x00eb,0x00eb
	in	al,#0x64	! 8042 status port
	test	al,#2		! is input buffer full?
	jnz	empty_8042	! yes - loop
	ret

gdt:
	.word	0,0,0,0		! dummy

	.word	0x07FF		! 8Mb - limit=2047 (2048*4096=8Mb)
	.word	0x0000		! base address=0
	.word	0x9A00		! code read/exec
	.word	0x00C0		! granularity=4096, 386

	.word	0x07FF		! 8Mb - limit=2047 (2048*4096=8Mb)
	.word	0x0000		! base address=0
	.word	0x9200		! data read/write
	.word	0x00C0		! granularity=4096, 386

idt_48:
	.word	0			! idt limit=0
	.word	0,0			! idt base=0L

gdt_48:
	.word	0x800		! gdt limit=2048, 256 GDT entries
	.word	512+gdt,0x9	! gdt base = 0X9xxxx

msg1:
	.ascii "Now we are in SETUP"


msg_cursor_pos:
	.byte 13,10
	.ascii "cursor_pos: "

msg_memory:
	.byte 13,10
	.ascii "extended memory size: "

msg_KB:
	.ascii " KB"

msg_display_page:
	.byte 13,10
	.ascii "display page: "

msg_video_mode:
	.byte 13,10
	.ascii "video mode: "

msg_window_width:
	.byte 13,10
	.ascii "window width: "


! SZ:打印bx中的参数	 length 4
!reference: https://github.com/1140310118/MIC/blob/master/oslab/linux-0.11/boot/setup.s
print_bx_4:
   mov cx,#4
   mov dx,bx
print_digit_4:
   rol dx,#4	!循环左移
   mov ax,#0x0e0f
   and al,dl
   add al,#0x30
   cmp al,#0x3a
   jb outp_4
   add al,#0x07
outp_4:
   int 0x10
   loop print_digit_4
   ret

! SZ:打印bx中的参数	 length 2
print_bx_2:
   mov cx,#2
   mov dx,bx
print_digit_2:
   rol dx,#4 !循环左移
   mov ax,#0x0e0f
   and al,dl
   add al,#0x30
   cmp al,#0x3a
   jb outp_2
   add al,#0x07
outp_2:
   int 0x10
   loop print_digit_2
   ret



! SZ:读取光标位置
read_cursor_pos:
	mov	ah,#0x03		! read cursor pos
	xor	bh,bh
	int	0x10
	ret

! SZ:打印字符串
print_str_cx_bp:
	mov	bx,#0x0007		! page 0, attribute 7 (normal)
	mov ax,#SETUPSEG
	mov es,ax
	mov	ax,#0x1301		! write string, move cursor
	int	0x10
	ret

! SZ:打印由ax:bx指定的内存中的数据 length:4
print_num_ax_bx_4:
	mov ax,#INITSEG
	mov ds,ax
	mov di,bx
	mov bx,(di)
	call print_bx_4
	ret

! SZ:打印由ax:bx指定的内存中的数据 length:2
print_num_ax_bx_2:
	mov ax,#INITSEG
	mov ds,ax
	mov di,bx
	mov bx,(di)
	call print_bx_2
	ret

!打印回车换行  
print_CR:  
	mov    ax,#0xe0d     ! CR  
	int    0x10  
	mov    al,#0xa     ! LF  
	int    0x10  
	ret 

.text
endtext:
.data
enddata:
.bss
endbss:
