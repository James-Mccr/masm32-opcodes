; A list of intel opcodes used by masm

.486
.model flat, stdcall
option casemap:none

include \masm32\macros\macros.asm

include \masm32\include\masm32.inc
include \masm32\include\kernel32.inc

includelib \masm32\lib\masm32.lib
includelib \masm32\lib\kernel32.lib

.data
	globalString db "Okay!", 0
	
.const
	cf equ 1
	pf equ 4
	af equ 16
	zf equ 64
	sf equ 128
	tf equ 256
	of equ 2048
	
.code

main:
	call @adc
    call @add
	call @and
	call @bound
	call @bsf
	call @bsr
	call @bswap
	call @bt
	call @btc
	call @btr
	call @bts
	call @call
	call @cbw
	call @cdq
	call @clc
	call @cld
	call @cmc
	call @cmp
	call @cmps
	call @cmpxchg
	call @cwd
	call @cwde
	call @daa
	call @das
	call @dec
	call @div
	call @enter
	call @idiv
	call @imul
	call @inc
	call @ja
	call @jae
	call @jb
	call @jbe
	call @jc
	call @jcxz
	call @je
	call @jg
	call @jge
	call @jl
	call @jle
	call @jmp
	call @jna
	call @jnae
	call @jnb
	call @jnbe
	call @jnc
	call @jne
	call @jng
	call @jnge
	call @jnl
	call @jnle
	call @jno
	call @jnp
	call @jns
	call @jnz
	call @jo
	call @jp
	call @jpe
	call @jpo
	call @js
	call @jz
	call @lahf
	call @lar
	call @lea
	call @leave
	call @lock
	call @lods
	call @loop
	call @loope
	call @loopnz
	call @mov
	call @movs
	call @movsx
	call @movzx
	call @mul
	call @neg
	call @nop
	call @not
	call @or
	call @pop
	call @popa
	call @popf
	call @push
	call @pusha
	call @pushf
	call @rcl
	call @rcr
	call @rep
	call @repe
	call @repne
	call @ret
	call @rol
	call @ror
	call @sahf
	call @sal
	call @sar
	call @sbb
	call @scas
	call @setae
	call @setb
	call @setbe
	call @sete
	call @setne
	call @setl
	call @setge
	call @setle
	call @setg
	call @sets
	call @setns
	call @setc
	call @setnc
	call @seto
	call @setno
	call @setp
	call @setnp
	call @shl
	call @shr
	call @shld
	call @stc
	call @std
	call @stos
	call @sub
	call @test
	call @verr
	call @verw
	call @wait
	call @xchg
	call @xlat
	call @xor
	exit
	
newline macro
	print chr$(13,10,13,10)
endm	
	
; @aaa - uses unpacked BCD format
; @aad - see above
; @aam - see above
; @aas - see above

@adc proc
	print "8 + 27 (+ 1) = "
	mov eax, 8
	stc
	adc eax, 27
	print str$(eax)
	newline
	ret
@adc endp
	
@add proc
	print "87 + 59 = "
	mov eax, 87
	add eax, 59
	print str$(eax)
	newline
	ret
@add endp
	

@and proc
	print "255 & 63 = "
	mov eax, 255
	and eax, 63
	print str$(eax)
	newline
	ret
@and endp

; @arpl - what does privilege level mean?

@bound proc
	LOCAL limit:DWORD
	print "Checking bound... "
	mov edx, 3
	mov limit, 196608	; array range [0 - 3]
	bound dx, [limit]	; program terminates here if index outside range
	print "Index is within range." 
	newline
	ret
@bound endp

@bsf proc
	print "Lowest bit set in 582 is at index " 
	mov edx, 582	; 582 = 1001000110
	bsf ax, dx	; index starts at 0
	print str$(eax)
	newline
	ret
@bsf endp

@bsr proc
	print "Highest bit set in 582 is at index "
	mov edx, 582	; 582 = 0x00000246h
	bsr ax, dx
	print str$(eax)
	newline
	ret
@bsr endp

@bswap proc
	print "Little endian - 1 "
	mov edx, 1	; 0x00000001 => 0x01000000
	bswap edx
	print "Big endian - "
	print str$(edx)
	newline
	ret
@bswap endp

@bt proc	; copies target bit into CF
	print "Bit test - 0x01[0] = "
	mov edx, 1
	bt dx, 0
	jnc err
	print "1"
	newline
	ret 
@bt endp

@btc proc	; bit test but inverts target bit afterwards
	print "Bit test compliment - 0x01[1] ~ 0x0"
	mov edx, 1
	btc dx, 1
	print str$(edx)
	newline
	ret
@btc endp

@btr proc	; bit test but sets target bit to 0 afterwards
	print "Bit test reset - 0x03[0] : 0x0"
	mov edx, 3
	btr dx, 0
	print str$(edx)
	newline
	ret
@btr endp

@bts proc	; bit test but sets target bit to 1 afterwards
	print "Bit test set - 0x02[0] : 0x0"
	mov edx, 2
	bts dx, 0
	print str$(edx)
	newline
	ret
@bts endp

@call proc
	LOCAL ip:DWORD
	print "Call - "
	call a	
	a:
		mov eax, [esp]
		mov ip, eax
		print " IP: "
		print str$(ip)
		print " proc a: "
		print str$(a)
		newline
		ret	; LOCAL adds a LEAVE instruction so it returns to caller
@call endp

@cbw proc	; copies msb in AL into every bit in AH
	print "Convert byte word - 0x19 : 0x"
	mov eax, 25
	cbw	
	print hex$(eax)
	print ", 0x80 : 0x"
	mov eax, 128
	cbw	
	print hex$(eax)
	newline
	ret
@cbw endp

@cdq proc	; copies msb in EAX into every bit in EDX
	LOCAL msdw:DWORD	; most significant dword
	LOCAL lsdw:DWORD	; least significant dword
	print "Convert double to quad - 0x80000000 : 0x"
	mov eax, 80000000h
	cdq
	mov lsdw, eax
	mov msdw, edx
	print hex$(msdw)
	print hex$(lsdw)
	newline
	ret
@cdq endp

@clc proc
	print "Clear carry flag - CF = "
	clc
	jc err
	print "0"
	newline
	ret
@clc endp

@cld proc
	print "Clear direction flag - DF = "
	cld
	pushf
	pop ax	; pushf uses 16 bits, pop must use 16 bit register or it will misalign stack
	bt eax, 10
	jc err
	print "0"
	newline
	ret
@cld endp

; @cli - clear interrupt freezes program

; @clts - clear task switched flag - privilege operation

@cmc proc
	print "Compliment carry flag - CF = "
	clc	; cf = 0
	cmc	; cf = 1
	lahf
	bt eax, 8
	jnc err
	print "1"
	newline
	ret
@cmc endp

@cmp proc
	print "Compare - "
	mov eax, 11
	cmp eax, eax
	jne err
	print "11 = 11"
	newline
	ret
@cmp endp

@cmps proc
	print "Compare string - "
	mov esi, offset globalString
	mov edi, offset globalString
	mov ecx, sizeof globalString
	repe cmpsb ; repe encodes cmpsb to continue comparing bytes
	jne err
	print "Test strings equal"
	newline
	ret
@cmps endp

@cmpxchg proc
	print "Compare and exchange - eax = "
	mov eax, 255
	mov edx, 25
	cmpxchg edx, ecx	; since 25 != 255, eax loads edx. ecx isn't used
	print str$(eax)
	newline
	ret
@cmpxchg endp

@cwd proc
	print "Convert word to doubleword - 5 = "
	mov ax, 5
	cwd
	shl edx, 16
	mov dx, ax
	print str$(edx)
	newline
	ret
@cwd endp

@cwde proc
	print "Convert word to extended doubleword - FFFF8000 = "
	mov ax, 32768
	cwde
	print hex$(eax)
	newline
	ret
@cwde endp

@daa proc
	print "Decimal adjust for addition - 10 : "
	mov eax, 0BAh
	daa	; treats nibble as decimal digits; adds 6 if digit is outside 0-9
	print str$(eax)
	newline
	ret
@daa endp

@das proc
	print "Decimal adjust for subtraction -  10 : "
	mov eax, 0BAh
	das	; treats nibble as decimal digits; subtract 6 if digit is outside 0-9
	print str$(eax)
	newline
	ret
@das endp

@dec proc
	print "Decrement - 91 - 1 = "
	mov eax, 91
	dec eax
	print str$(eax)
	newline
	ret
@dec endp

@div proc
	print "Divide - 187 / 3 = "
	mov eax, 187
	mov dl, 3
	div dl	; 8 bit instruction stores output in AX
	movzx edx, al
	movzx ebx, ah
	print str$(edx)
	print "r"
	print str$(ebx)
	newline
	ret
@div endp

@enter proc
	print "Make stack frame - "
	enter 64, 0
	print "new frame generated "
	leave
	newline
	ret
@enter endp

; @esc - error A2205: ESC instruction is obsolete : ESC ignored

; @hlt - stops executing program

@idiv proc
	print "Signed integer division - -307 / 15 = -"
	mov ax, -307
	mov dl, 15
	idiv dl
	neg al
	neg ah
	movzx edx, al
	movzx ebx, ah
	print str$(edx)
	print "r-"
	print str$(ebx)
	newline
	ret
@idiv endp

@imul proc
	print "Signed mulitply - -11 * 5 = -"
	mov ax, -11
	mov dl, 5
	imul dl
	neg ax
	movzx edx, ax
	print str$(edx)
	newline
	ret
@imul endp

; @in - input byte or word from port. reads network port

@inc proc
	print "Increment - ax + 1 = "
	mov eax, 1000
	inc eax
	print str$(eax)
	newline
	ret
@inc endp

; @ins - input string from port. read network port for string

; @int - interrupt codes are version dependent. intended for DOS

; @into - another interrupt

; @invd - invalidate cache. terminates program, real mode/protected mode

; invlpg - invalidate translation look-aside buffer entry.

; iret/iretd - interrupt return

@ja proc
	print "Jump if above..."
	mov ah, 0
	sahf
	ja ok
	jmp err
@ja endp

@jae proc
	print "Jump if above or equal..."
	mov ah, 0
	sahf
	jae ok
	jmp err
@jae endp

@jb proc
	print "Jump if below..."
	mov ah, cf
	sahf
	jb ok
	jmp err
@jb endp

@jbe proc
	print "Jump if below or equal..."
	mov ah, cf or zf
	sahf
	jbe ok
	jmp err
@jbe endp

@jc proc
	print "Jump if carry..."
	mov ah, cf
	sahf
	jc ok
	jmp err
@jc endp

near_ok:
	jmp ok

@jcxz proc
	print "Jump if cx zero..."
	mov cx, 0
	jcxz near_ok	; explicit near jump, address must be within -127,128 bytes
	jmp err
@jcxz endp

@je proc
	print "Jump if equal..."
	mov ah, zf
	sahf
	je ok
	jmp err
@je endp

@jg proc
	print "Jump if greater (signed)..."
	push 0
	popfd
	jg ok
	jmp err
@jg endp

@jge proc
	print "Jump if greater or equal (signed)..."
	push zf or sf or of
	popfd
	jge ok
	jmp err
@jge endp

@jl proc
	print "Jump if less (signed)..."
	push of
	popfd
	jl ok
	jmp err
@jl endp

@jle proc
	print "Jump if less or equal (signed)..."
	push zf or of
	popfd
	jle ok 
	jmp err
@jle endp

@jmp proc
	print "Unconditional jump..."
	jmp ok
	jmp err
@jmp endp

@jna proc
	print "Jump if not above..."
	mov ah, cf or zf
	sahf
	jna ok
	jmp err
@jna endp

@jnae proc
	print "Jump if not above or equal..."
	mov ah, cf
	sahf
	jnae ok
	jmp err
@jnae endp

@jnb proc
	print "Jump if not below..."
	clc
	jnb ok
	jmp err
@jnb endp

@jnbe proc
	print "Jump if not below or equal..."
	mov ah, 0
	sahf
	jnbe ok
	jmp err
@jnbe endp

@jnc proc
	print "Jump if not carry..."
	clc 
	jnc ok
	jmp err
@jnc endp

@jne proc
	print "Jump if not equal..."
	mov ah, 0
	sahf
	jne ok
	jmp err
@jne endp

@jng proc
	print "Jump if not greater (signed)..."
	mov ah, zf or sf
	sahf
	jng ok
	jmp err
@jng endp

@jnge proc
	print "Jump if not greater or equal (signed)..."
	mov ah, sf
	sahf
	jnge ok
	jmp err
@jnge endp

@jnl proc
	print "Jump if not less (signed)..."
	push sf or of
	popfd
	jnl ok
	jmp err
@jnl endp

@jnle proc
	print "Jump if not less or equal (signed)..."
	push sf or of
	popfd
	jnle ok
	jmp err
@jnle endp

@jno proc
	print "Jump if not overflow..."
	push 0
	popfd
	jno ok
	jmp err
@jno endp

@jnp proc
	print "Jump if no parity..."
	mov ah, 0
	sahf
	jnp ok
	jmp err
@jnp endp

@jns proc
	print "Jump if not signed (signed)..."
	mov ah, 0
	sahf
	jns ok
	jmp err
@jns endp

@jnz proc
	print "Jump if not zero..."
	mov ah, 0
	sahf
	jnz ok 
	jmp err
@jnz endp

@jo proc
	print "Jump if overflow (signed)..."
	push of
	popfd
	jo ok
	jmp err
@jo endp

@jp proc
	print "Jump if parity..."
	mov ah, pf
	sahf
	jp ok
	jmp err
@jp endp

@jpe proc
	print "Jump if parity even..."
	mov ah, pf
	sahf
	jpe ok
	jmp err
@jpe endp

@jpo proc
	print "Jump if parity odd..."
	mov ah, 0
	sahf
	jpo ok
	jmp err
@jpo endp

@js proc
	print "Jump if signed (signed)..."
	mov ah, sf
	sahf
	js ok 
	jmp err
@js endp

@jz proc
	print "Jump is zero..."
	mov ah, zf
	sahf
	jz ok
	jmp err
@jz endp

@lahf proc
	print "Load register AH from flags..."
	mov eax, 0
	lahf
	print hex$(eax)
	newline
	ret
@lahf endp

@lar proc
	print "Load access rights..."
	mov eax, 0
	mov edx, 1000
	lar eax, edx
	print str$(eax)
	newline
	ret
@lar endp

; @lds - load pointer using ds. for far pointers

@lea proc
	LOCAL x:DWORD
	print "Load effective address..."
	mov eax, 0
	lea eax, x
	print hex$(eax)
	newline
	ret
@lea endp

@leave proc
	print "Restore stack for procedure exit..."
	enter 8, 0
	leave
	jmp ok
@leave endp

; @led - load pointer using es. for far pointers

; @lfs - load pointer using fs. for far pointers

; @lgdt - load global descriptor table. for OS

; @lidt - load interrupt descriptor table. for OS

; @lgs - load pointer using gs. for far pointers

; @lldt - load local descriptor table. for OS

; @lmsw - load machine status word. for OS

@lock proc
	LOCAL a:DWORD
	print "Lock bus..."
	mov a, 0
	mov eax, 15
	lock xchg a, eax
	leave	; local allocates stack memory, 'ret' in the procedure clears locals but jmp messes with it
	jmp ok
@lock endp

@lods proc
	print "Load string..."
	mov eax, 0
	mov esi, offset globalString 
	lodsb
	cmp eax, 79
	je ok
	jmp err
@lods endp

@loop proc
	print "Decrement cx and loop if cx not zero..."
	mov ecx, 3
	lcl:
		push ecx
		print str$(ecx),32
		pop ecx
		loop lcl
	jmp ok
@loop endp

@loope proc
	print "Loop while equal/zero..."
	mov ecx, 10
	lcl:
		push ecx
		print str$(ecx),32
		pop ecx
		mov ah, zf	; loope requires zf=1
		sahf
		loope lcl
	jmp ok
@loope endp

@loopnz proc
	print "Loop while not zero/not equal..."
	mov ecx, 5
	lcl:
		push ecx
		print str$(ecx), 32
		pop ecx
		mov ah, 0	; loopnz require zf=0
		sahf
		loopnz lcl
	jmp ok
@loopnz endp

; @lsl - load segment limit. gets size of gdt entry (if successful).

; @lss - for far pointers

; @ltr - load task register. privileged instruction

@mov proc
	print "Move byte or word..."
	mov eax, 1
	print hex$(eax), 32
	jmp ok
@mov endp

@movs proc
	LOCAL dest:DWORD
	print "Move string (byte or word)..."
	mov esi, offset globalString
	lea edi, dest
	mov ecx, 4
	movsd ; reverses order on stack
	cmp dest, 79616b4fh
	leave
	je ok
	jmp err
@movs endp

@movsx proc
	print "Move with sign extend..."
	mov dx, 8000h
	movsx eax, dx
	cmp eax, 0FFFF8000h
	je ok
	jmp err
@movsx endp

@movzx proc
	print "Move with zero extend..."
	mov dx, 8000h
	movzx eax, dx
	cmp eax, 00008000h
	je ok
	jmp err
@movzx endp

@mul proc
	print "Unsigned multiply..."
	mov eax, 3000
	mov dx, 111
	mul dx
	cmp dx, 5
	jne err
	cmp ax, 5320
	je ok
	jmp err
@mul endp

@neg proc
	print "Two's compliment negation..."
	mov eax, 0ffffffffh
	neg eax
	cmp eax, 1
	je ok
	jmp err
@neg endp

@nop proc
	print "No operation..."
	nop
	jmp ok
@nop endp

@not proc
	print "One's compliment negation..."
	mov eax, 0ffffffffh
	not eax
	cmp eax, 0
	je ok
	jmp err
@not endp

@or proc
	print "Inclusive logical..."
	mov eax, 0fh
	or eax, 0f0h
	cmp eax, 0ffh
	je ok
	jmp err
@or endp

; @out - output data to port. transfer data over network

; @outs - output string to port.

@pop proc
	print "Pop word off stack..."
	push word ptr 8
	pop ax
	cmp ax, 8
	je ok
	jmp err
@pop endp

@popa proc
	print "Pop all registers onto stack..."
	push 1
	push 2
	push 3
	push 4
	push 5
	push 6
	push 7
	push 8
	popa
	cmp di, 8
	jne err
	cmp ax, 1
	je ok
	jmp err
@popa endp

@popf proc
	print "Pop flags off stack..."
	push word ptr 80h
	popf
	js ok
	jmp err
@popf endp

@push proc
	print "Push word onto stack..."
	push 15
	pop eax
	cmp ax, 15
	je ok
	jmp err
@push endp

@pusha proc
	LOCAL ldi:dword
	print "Push all registers onto stack..."
	mov ldi, edi
	pushad
	pop edi
	cmp edi, ldi
	leave
	je ok
	jmp err
@pusha endp

@pushf proc
	print "Push flags onto stack..."
	mov ah, 1
	sahf
	pushfd
	pop eax
	cmp al, 3	; 2nd flag is always 1
	je ok
	jmp err
@pushf endp

@rcl proc
	print "Rotate through carry left..."
	mov al, 4	
	rcl al, 7	; bits rotate out (stored in CF), rotate again to re-enter value
	cmp al, 1
	je ok
	jmp err
@rcl endp

@rcr proc
	print "Rotate through carry right..."
	mov al, 1
	rcr al, 2
	cmp al, 128
	je ok
	jmp err
@rcr endp

@rep proc
	print "Repeat string operation..."
	mov esi, offset globalString
	mov ecx, 2
	rep lodsb	; overwrites value in al each repeat
	cmp eax, 6bh
	je ok
	jmp err
@rep endp

@repe proc
	LOCAL localString[4]:byte
	print "Repeat equal..."
	mov esi, offset globalString
	lea edi, localString
	mov ecx, 3	; copy 3 characters
	rep movsb
	mov esi, offset globalString
	lea edi, localString
	mov ecx, 4	; compare 4 characters 
	repz cmpsb	; 4th character was unallocated stack space, shouldn't match global string
	leave
	jne ok
	jmp err
@repe endp

@repne proc
	LOCAL localString[4]:byte
	print "Repeat not equal..."
	mov esi, offset globalString
	lea edi, localString
	mov dword ptr localString, 40302010h
	mov ecx, 4
	repne cmpsb
	leave
	jnz ok
	jmp err
@repne endp

@ret proc
	print "Return from procedure..."
	print "Returning."
	newline
	ret
@ret endp

@rol proc
	print "Rotate left..."
	mov al, 0fh
	rol al, 5
	cmp al, 225
	je ok
	jmp err
@rol endp

@ror proc
	print "Rotate right..."
	mov al, 0f0h
	ror al, 5
	cmp al, 135
	je ok
	jmp err
@ror endp

@sahf proc
	print "Store AH register into flags..."
	mov ah, 4
	sahf
	jp ok
	jmp err
@sahf endp

@sal proc
	print "Shift arithmetic left..."
	mov al, 16
	sal al, 4
	cmp al, 0
	je ok
	jmp err
@sal endp

@sar proc
	print "Shift arithmetic right..."
	mov al, 8
	sar al, 3
	cmp al, 1
	je ok
	jmp err
@sar endp

@sbb proc
	print "Subtract with borrow..."
	stc	; sbb subtracts extra 1 if carry set
	mov eax, 5000
	mov edx, 3000
	sbb eax, edx
	cmp eax, 1999
	je ok
	jmp err
@sbb endp

@scas proc
	print "Scan string..."
	mov edi, offset globalString
	mov al, 33	; '!'
	mov ecx, 5
	repne scasb
	je ok
	jmp err
@scas endp

@setae proc
	print "Set if above or equal..."
	clc
	mov al, 0
	setae al
	cmp al, 1
	je ok
	jmp err
@setae endp

@setb proc
	print "Set if below..."
	stc
	mov al, 3
	setb al
	cmp al, 1
	je ok
	jmp err
@setb endp

@setbe proc
	print "Set if below or equal..."
	mov ah, zf
	sahf
	mov al, 2
	setbe al
	cmp al, 1
	je ok
	jmp err
@setbe endp

@sete proc
	print "Set if equal..."
	mov ah, zf
	sahf
	mov al, 2
	sete al
	cmp al, 1
	je ok
	jmp err
@sete endp

@setne proc
	print "Set if not equal..."
	mov ah, 0
	sahf
	mov al, 2
	setne al
	cmp al, 1
	je ok
	jmp err
@setne endp

@setl proc
	print "Set if less..."
	push sf
	popfd
	mov al, 2
	setl al
	cmp al, 1
	je ok
	jmp err
@setl endp

@setge proc
	print "Set if greater or equal..."
	push sf or of
	popfd
	mov al, 2
	setge al
	cmp al, 1
	je ok
	jmp err
@setge endp

@setle proc
	print "Set if less or equal..."
	mov ah, zf
	sahf
	mov al, 2
	setle al
	cmp al, 1
	je ok
	jmp err
@setle endp

@setg proc
	print "Set if greater..."
	push sf or of
	popfd
	mov al, 2
	setg al
	cmp al, 1
	je ok
	jmp err
@setg endp

@sets proc
	print "Set if signed..."
	mov ah, sf
	sahf
	mov al, 2
	sets al
	cmp al, 1
	je ok
	jmp err
@sets endp

@setns proc
	print "Set if not signed..."
	mov ah, 0
	sahf
	mov al, 2
	setns al
	cmp al, 1
	je ok
	jmp err
@setns endp

@setc proc
	print "Set if carry..."
	stc
	mov al, 2
	setc al
	cmp al, 1
	je ok
	jmp err
@setc endp

@setnc proc
	print "Set if not carry..."
	clc
	mov al, 2
	setnc al
	cmp al, 1
	je ok
	jmp err
@setnc endp

@seto proc
	print "Set if overflow..."
	push of
	popfd
	mov al, 2
	seto al
	cmp al, 1
	je ok
	jmp err
@seto endp

@setno proc
	print "Set if not overflow..."
	push 0
	popfd
	mov al, 2
	setno al
	cmp al, 1
	je ok
	jmp err
@setno endp

@setp proc
	print "Set if parity..."
	mov ah, pf
	sahf
	mov al, 2
	setp al
	cmp al, 1
	je ok
	jmp err
@setp endp

@setnp proc
	print "Set if not parity..."
	mov ah, 0
	sahf
	mov al, 2
	setnp al
	cmp al, 1
	je ok
	jmp err
@setnp endp

;	@sgdt - store global desciptor table
;	@sidt - store interrupt descriptor table

@shl proc
	print "Shift logical left..."
	mov al, 8
	shl al, 4
	cmp al, 128
	je ok
	jmp err
@shl endp

@shr proc
	print "Shift logical right..."
	mov al, 64
	shr al, 5
	cmp al, 2
	je ok
	jmp err
@shr endp

@shld proc
	print "Double precision shift..."
	mov eax, 16
	mov edx, 0E0000000h
	shld eax, edx, 3
	cmp eax, 135
	je ok
	jmp err
@shld endp

;	@sldt - store local descriptor table
;	@smsw - store machine status word

@stc proc
	print "Set carry..."
	clc
	stc
	jc ok
	jmp err
@stc endp

@std proc
	print "Set direction flag..."
	std
	pushfd
	pop edx
	bt edx, 10
	cld
	jc ok
	jmp err
@std endp

;	@sti - set interrupt flag

@stos proc
	LOCAL localString[4]:byte
	print "Store string..."
	lea edi, localString
	mov eax, 596F7368h
	stosd
	cmp dword ptr localString, 596F7368h
	leave
	je ok
	jmp err
@stos endp

; @str - store task register

@sub proc
	print "Subtract..."
	mov eax, 5
	sub eax, 10
	cmp eax, -5
	je ok
	jmp err
@sub endp

@test proc
	print "Test for bit pattern..."
	mov eax, 12
	test eax, 16	; zf=1 when a AND b = 0
	je ok
	jmp err
@test endp

@verr proc
	print "Verify read..."
	mov ax, ds
	verr ax
	jz ok
	jmp err
@verr endp

@verw proc
	print "Verify write..."
	mov ax, ds
	verr ax
	jz ok 
	jmp err
@verw endp

@wait proc
	print "Event wait..."
	wait
	jmp ok
@wait endp

; @wbinvd - Write back and invalidate cache (privileged)

@xchg proc
	print "Exchange..."
	mov eax, 51
	mov edx, 99
	xchg eax, edx
	cmp eax, 99
	jne err
	cmp edx, 51
	je ok
	jmp err
@xchg endp

@xlat proc
	LOCAL array[10]:byte
	print "Translate..."
	lea ebx, array
	mov [array+3], 5
	mov al, 3
	xlatb 
	cmp [array+3], 5
	leave
	je ok
	jmp err
@xlat endp

@xor proc
	print "Exclusive or..."
	mov eax, 0000F000h
	mov edx, 00003001h
	xor eax, edx
	cmp eax, 0000C001h
	je ok
	jmp err
@xor endp

ok:
	print "OK"
	newline
	ret

err:
	print "ERROR"
	exit

end main