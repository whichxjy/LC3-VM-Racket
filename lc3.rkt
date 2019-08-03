#lang racket

(require ffi/unsafe
         ffi/unsafe/define
         ffi/unsafe/port)

;; The LC-3 has 65536 memory locations
(define MEM-LOC-NUM (expt 2 16))

;; The LC-3 has 10 total registers
(define REG-COUNT 10)

;; Is the virtual machine still running?
(define is-running #t)

;; Memory Storage
(define memory (make-vector MEM-LOC-NUM 0))

;; Register Storage
(define reg (make-vector REG-COUNT 0))

;; Registers
(define R-R0 0)
(define R-R1 1)
(define R-R2 2)
(define R-R3 3)
(define R-R4 4)
(define R-R5 5)
(define R-R6 6)
(define R-R7 7)
(define R-PC 8)
(define R-COND 9)

;; Opcodes
(define OP-BR 0)    ;; branch
(define OP-ADD 1)   ;; add
(define OP-LD 2)    ;; load
(define OP-ST 3)    ;; store
(define OP-JSR 4)   ;; jump register
(define OP-AND 5)   ;; bitwise and
(define OP-LDR 6)   ;; load register
(define OP-STR 7)   ;; store register
(define OP-RTI 8)   ;; unused
(define OP-NOT 9)   ;; bitwise not
(define OP-LDI 10)  ;; load indirect
(define OP-STI 11)  ;; store indirect
(define OP-JMP 12)  ;; jump
(define OP-RES 13)  ;; reserved (unused)
(define OP-LEA 14)  ;; load effective address
(define OP-TRAP 15) ;; execute trap

;; Condition Flags
(define FL-POS (arithmetic-shift 1 0)) ;; P
(define FL-ZRO (arithmetic-shift 1 1)) ;; Z
(define FL-NEG (arithmetic-shift 1 2)) ;; N

;; Memory Mapped Registers
(define MR-KBSR #xFE00) ;; keyboard status
(define MR-KBDR #xFE02) ;; keyboard data

;; TRAP Codes
(define TRAP-GETC #x20)  ;; get character from keyboard, not echoed onto the terminal
(define TRAP-OUT #x21)   ;; output a character
(define TRAP-PUTS #x22)  ;; output a word string
(define TRAP-IN #x23)    ;; get character from keyboard, echoed onto the terminal
(define TRAP-PUTSP #x24) ;; output a byte string
(define TRAP-HALT #x25)  ;; halt the program

;; Swap
(define (swap16 two-bytes)
  (define low-byte (bytes-ref two-bytes 1))
  (define high-byte (bytes-ref two-bytes 0))
  (bitwise-ior (arithmetic-shift high-byte 8) low-byte))

;; Sign Extend
(define (sign-extend x bit-count)
  (cond [(positive? (bitwise-and (arithmetic-shift x (- (sub1 bit-count))) #x1))
         (bitwise-and (bitwise-ior x (arithmetic-shift #xFFFF bit-count)) #xFFFF)]
        [else
         (bitwise-and x #xFFFF)]))

;; Write To Register
(define (reg-write reg-idx val)
  (set! val (bitwise-and val #xFFFF))
  (vector-set! reg reg-idx val))

;; Read From Register
(define (reg-read reg-idx)
  (vector-ref reg reg-idx))

;; Write To Memory
(define (mem-write address val)
  (set! address (bitwise-and address #xFFFF))
  (set! val (bitwise-and val #xFFFF))
  (vector-set! memory address val))

;; Read From Memory
(define (mem-read address)
  (set! address (bitwise-and address #xFFFF))
  (when (= address MR-KBSR)
    (cond [(char-ready?)
           (mem-write MR-KBSR (arithmetic-shift 1 15))
           (mem-write MR-KBDR (char->integer (read-char)))]
          [else
           (mem-write MR-KBSR 0)]))
  (vector-ref memory address))

;; Display Memory (Just For Debugging)
(define (display-mem)
  (for ([i (in-range 0 MEM-LOC-NUM)])
    (printf "[address ~a]: ~a" i (mem-read i))
    (if (= (modulo (add1 i) 4) 0)
        (newline)
        (display " "))))

;; Display Registers (Just For Debugging)
(define (display-reg)
  (for ([i (in-range 0 REG-COUNT)])
    (printf "[reg ~a]: ~a" i (reg-read i))
    (if (= (modulo (add1 i) 5) 0)
        (newline)
        (display " "))))

;; Update Flags
(define (update-flags reg-idx)
  (cond [(zero? (reg-read reg-idx))
         (reg-write R-COND FL-ZRO)]
        [(positive? (arithmetic-shift (reg-read reg-idx) -15))
         (reg-write R-COND FL-NEG)]
        [else
         (reg-write R-COND FL-POS)]))

;; Read Two Bytes From The Port
(define (read-two-bytes in-port)
  (define two-bytes (read-bytes 2 in-port))
  (cond
    [(bytes? two-bytes) (swap16 two-bytes)]
    [else eof]))

;; Read Image File
(define (read-image-file in-port)
  ;; the origin tells us where in memory to place the image
  (define origin (read-two-bytes in-port))
  ;; read image with recursion
  (define (read-image-iter in-port address)
    (define two-bytes (read-two-bytes in-port))
    (unless (or (>= address MEM-LOC-NUM) (eof-object? two-bytes))
      (mem-write address two-bytes)
      (read-image-iter in-port (add1 address))))
  (read-image-iter in-port origin))

;; ==================================================================

;; Modified version of the "get-pass" package
;; Thanks to Spencer Mitchell (@smitchell556)

(define-ffi-definer define-libc (ffi-lib #f))

;; C Structs
(define-cstruct _TERMIOS ([c_iflag _uint]
                          [c_oflag _uint]
                          [c_cflag _uint]
                          [c_lflag _uint]
                          [c_cc (_array _uint 32)]
                          [c_ispeed _uint]
                          [c_ospeed _uint]))

;; C Functions
(define-libc tcgetattr (_fun _int (t : (_ptr o _TERMIOS)) -> (i : _int) -> (values i t)))
(define-libc tcsetattr (_fun _int _int (t : (_ptr i _TERMIOS)) -> (i : _int) -> (values i t)))

;; C Defines
(define ECHO 8)
(define TCSANOW 0)
(define TCSADRAIN 1)

;; Accept a single character from the port without echoing it back
(define (getc [in (current-input-port)])
  ;; Utility to get a single character from the command line without echoing input.
  (define fd (unsafe-port->file-descriptor in))
  (define-values (ret-val termios) (tcgetattr fd))

  ;; Set ECHO flag to 0 in TERMIOS struct.
  (set-TERMIOS-c_lflag! termios (bitwise-and (TERMIOS-c_lflag termios)
                                             (bitwise-not ECHO)))
  ;; Use TCSADRAIN to ensure all output to the output port has been transmitted
  ;; before turning off ECHO.
  (tcsetattr fd TCSADRAIN termios)

  ;; Read a character
  (define ch (read-char))

  ;; Set ECHO flag to 1 in TERMIOS struct.
  (set-TERMIOS-c_lflag! termios (bitwise-ior (TERMIOS-c_lflag termios) ECHO))
  ;; Use TCSANOW to ensure ECHO is turned on immediately.
  (tcsetattr fd TCSANOW termios)

  ch)

;; ====================== Traps Implementation ======================

;; TRAP GETC
(define (handle-trap-getc)
  ;; read a single ASCII character
  ;; (the character is not echoed onto the console)
  (reg-write R-R0 (char->integer (getc))))

;; TRAP OUT
(define (handle-trap-out)
  ;; write a ASCII character to the console display
  (display (integer->char (reg-read R-R0)))
  (flush-output))

;; TRAP PUTS
(define (handle-trap-puts)
  ;; write a string of ASCII characters to the console display
  (define (write-char-iter addr)
    (unless (zero? (mem-read addr))
      (display (integer->char (mem-read addr)))
      (write-char-iter (add1 addr))))
  (write-char-iter (reg-read R-R0))
  (flush-output))

;; TRAP IN
(define (handle-trap-in)
  ;; print a prompt on the screen
  (display "Enter a character: ")
  (flush-output)
  ;; read a single character from the keyboard
  (define ch (read-char))
  (display ch)
  (reg-write R-R0 (char->integer ch)))

;; TRAP PUTSP
(define (handle-trap-putsp)
  ;; write a string of ASCII characters to the console display
  ;; (two characters per memory location)
  (define (write-char-iter addr)
    (unless (zero? (mem-read addr))
      (define low-byte (bitwise-and (mem-read addr) #xFF))
      (define high-byte (arithmetic-shift (mem-read addr) -8))
      (display (integer->char low-byte))
      (when (positive? high-byte)
        (display (integer->char high-byte)))
      (write-char-iter (add1 addr))))
  (write-char-iter (reg-read R-R0))
  (flush-output))

;; TRAP HALT
(define (handle-trap-halt)
  ;; halt execution
  (set! is-running #f)
  ;; print a message on the console
  (displayln "HALT")
  (flush-output))

;; =================== Instructions Implementation ==================

;; ADD
(define (do-add instr)
  ;; destination register (DR)
  (define r0 (bitwise-and (arithmetic-shift instr -9) #x7))
  ;; first operand (SR1)
  (define r1 (bitwise-and (arithmetic-shift instr -6) #x7))
  ;; whether we are in immediate mode
  (define imm-flag (bitwise-and (arithmetic-shift instr -5) #x1))
  (cond [(positive? imm-flag)
         (define imm5 (sign-extend (bitwise-and instr #x1F) 5))
         (reg-write r0 (+ (reg-read r1) imm5))]
        [else
         (define r2 (bitwise-and instr #x7))
         (reg-write r0 (+ (reg-read r1) (reg-read r2)))])
  (update-flags r0))

;; AND
(define (do-and instr)
  (define r0 (bitwise-and (arithmetic-shift instr -9) #x7))
  (define r1 (bitwise-and (arithmetic-shift instr -6) #x7))
  (define imm-flag (bitwise-and (arithmetic-shift instr -5) #x1))
  (cond [(positive? imm-flag)
         (define imm5 (sign-extend (bitwise-and instr #x1F) 5))
         (reg-write r0 (bitwise-and (reg-read r1) imm5))]
        [else
         (define r2 (bitwise-and instr #x7))
         (reg-write r0 (bitwise-and (reg-read r1) (reg-read r2)))])
  (update-flags r0))

;; NOT
(define (do-not instr)
  (define r0 (bitwise-and (arithmetic-shift instr -9) #x7))
  (define r1 (bitwise-and (arithmetic-shift instr -6) #x7))
  (reg-write r0 (bitwise-not (reg-read r1)))
  (update-flags r0))

;; BR
(define (do-br instr)
  (define pc-offset (sign-extend (bitwise-and instr #x1FF) 9))
  (define cond-flag (bitwise-and (arithmetic-shift instr -9) #x7))
  (cond [(positive? (bitwise-and cond-flag (reg-read R-COND)))
         (reg-write R-PC (+ (reg-read R-PC) pc-offset))]))

;; JMP
(define (do-jmp instr)
  ;; Also handles RET
  (define r1 (bitwise-and (arithmetic-shift instr -6) #x7))
  (reg-write R-PC (reg-read r1)))

;; JSR
(define (do-jsr instr)
  (reg-write R-R7 (reg-read R-PC))
  (define long-flag (bitwise-and (arithmetic-shift instr -11) #x1))
  (cond [(positive? long-flag)
         ;; JSR
         (define long-pc-offset (sign-extend (bitwise-and instr #x7FF) 11))
         (reg-write R-PC (+ (reg-read R-PC) long-pc-offset))]
        [else
         ;; JSRR
         (define r1 (bitwise-and (arithmetic-shift instr -6) #x7))
         (reg-write R-PC (reg-read r1))]))

;; LD
(define (do-ld instr)
  (define r0 (bitwise-and (arithmetic-shift instr -9) #x7))
  (define pc-offset (sign-extend (bitwise-and instr #x1FF) 9))
  (reg-write r0 (mem-read (+ (reg-read R-PC) pc-offset)))
  (update-flags r0))

;; LDI
(define (do-ldi instr)
  (define r0 (bitwise-and (arithmetic-shift instr -9) #x7))
  (define pc-offset (sign-extend (bitwise-and instr #x1FF) 9))
  (reg-write r0 (mem-read (mem-read (+ (reg-read R-PC) pc-offset))))
  (update-flags r0))

;; LDR
(define (do-ldr instr)
  (define r0 (bitwise-and (arithmetic-shift instr -9) #x7))
  (define r1 (bitwise-and (arithmetic-shift instr -6) #x7))
  (define offset (sign-extend (bitwise-and instr #x3F) 6))
  (reg-write r0 (mem-read (+ (reg-read r1) offset)))
  (update-flags r0))

;; LEA
(define (do-lea instr)
  (define r0 (bitwise-and (arithmetic-shift instr -9) #x7))
  (define pc-offset (sign-extend (bitwise-and instr #x1FF) 9))
  (reg-write r0 (+ (reg-read R-PC) pc-offset))
  (update-flags r0))

;; ST
(define (do-st instr)
  (define r0 (bitwise-and (arithmetic-shift instr -9) #x7))
  (define pc-offset (sign-extend (bitwise-and instr #x1FF) 9))
  (mem-write (+ (reg-read R-PC) pc-offset) (reg-read r0)))

;; STI
(define (do-sti instr)
  (define r0 (bitwise-and (arithmetic-shift instr -9) #x7))
  (define pc-offset (sign-extend (bitwise-and instr #x1FF) 9))
  (mem-write (mem-read (+ (reg-read R-PC) pc-offset)) (reg-read r0)))

;; STR
(define (do-str instr)
  (define r0 (bitwise-and (arithmetic-shift instr -9) #x7))
  (define r1 (bitwise-and (arithmetic-shift instr -6) #x7))
  (define offset (sign-extend (bitwise-and instr #x3F) 6))
  (mem-write (+ (reg-read r1) offset) (reg-read r0)))

;; TRAP
(define (do-trap instr)
  (define trap-code (bitwise-and instr #xFF))
  (cond
    [(= trap-code TRAP-GETC) (handle-trap-getc)]
    [(= trap-code TRAP-OUT) (handle-trap-out)]
    [(= trap-code TRAP-PUTS) (handle-trap-puts)]
    [(= trap-code TRAP-IN) (handle-trap-in)]
    [(= trap-code TRAP-PUTSP) (handle-trap-putsp)]
    [(= trap-code TRAP-HALT) (handle-trap-halt)]))

;; ==================================================================

;; Main Loop
(define (main)
  (call-with-input-file "2048.obj" #:mode 'binary read-image-file)
  ;; set the PC to starting position
  ;; 0x3000 is the default
  (define PC-START #x3000)
  (reg-write R-PC PC-START)
  ;; fetch & execute instructions with recursion
  (define (fetch-exec-iter)
    (when is-running
      ;; fetch
      (define instr (mem-read (reg-read R-PC)))
      (reg-write R-PC (add1 (reg-read R-PC)))
      (define op (arithmetic-shift instr -12))
      ;; execute
      (cond
        [(= op OP-ADD) (do-add instr)]
        [(= op OP-AND) (do-and instr)]
        [(= op OP-NOT) (do-not instr)]
        [(= op OP-BR) (do-br instr)]
        [(= op OP-JMP) (do-jmp instr)]
        [(= op OP-JSR) (do-jsr instr)]
        [(= op OP-LD) (do-ld instr)]
        [(= op OP-LDI) (do-ldi instr)]
        [(= op OP-LDR) (do-ldr instr)]
        [(= op OP-LEA) (do-lea instr)]
        [(= op OP-ST) (do-st instr)]
        [(= op OP-STI) (do-sti instr)]
        [(= op OP-STR) (do-str instr)]
        [(= op OP-TRAP) (do-trap instr)]
        [else (error "BAD OPCODE")])
      (fetch-exec-iter)))
  (fetch-exec-iter))

(main)
