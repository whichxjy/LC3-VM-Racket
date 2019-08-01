#lang racket

(define UINT16-MAX #xFFFF)

(define REG-COUNT 10)

(define is-running #t)

;; Memory Storage
(define memory (make-vector UINT16-MAX 0))

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

;; Swap
(define (swap16 two-bytes)
  (define high-byte (bytes-ref two-bytes 0))
  (define low-byte (bytes-ref two-bytes 1))
  (bitwise-ior (arithmetic-shift high-byte 8) low-byte))

;; Sign Extend
(define (sign-extend x bit-count)
  (cond [(positive? (bitwise-and (arithmetic-shift x (- (sub1 bit-count))) 1))
         (bitwise-ior x (arithmetic-shift #xFFFF bit-count))]
        [else x]))

;; Write To Register
(define (reg-write reg-idx val)
  (vector-set! reg reg-idx val))

;; Read From Register
(define (reg-read reg-idx)
  (vector-ref reg reg-idx))

;; Write To Memory
(define (mem-write address val)
  (vector-set! memory address val))

;; Read From Memory
(define (mem-read address)
  (cond [(= address MR-KBSR)
         (cond [(char-ready?)
                (mem-write MR-KBSR (arithmetic-shift 1 15))
                (mem-write MR-KBDR (char->integer (read-char)))]
               [else
                (mem-write MR-KBSR 0)])])
  (vector-ref memory address))

;; Print Memory
(define (print-memory)
  (for ([i (in-range 0 UINT16-MAX)])
    (if (= (modulo (add1 i) 5) 0)
        (printf "[index ~a]: ~a\n" i (mem-read i))
        (printf "[index ~a]: ~a  " i (mem-read i)))))

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
    (let ([two-bytes (read-two-bytes in-port)])
      (unless (or (>= address UINT16-MAX) (eof-object? two-bytes))
        (mem-write address two-bytes)
        (read-image-iter in-port (add1 address)))))
  (read-image-iter in-port origin))


;; =================== Instructions Implementation ==================

;; ADD
(define (do-add instr)
  ;; destination register (DR)
  (define r0 (bitwise-and (arithmetic-shift instr -9) #x7))
  ;; first operand (SR1)
  (define r1 (bitwise-and (arithmetic-shift instr -6) #x7))
  ;; whether we are in immediate mode
  (define imm-flag (positive? (bitwise-and (arithmetic-shift instr -5) #x1)))
  (cond [(imm-flag)
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
  (define imm-flag (positive? (bitwise-and (arithmetic-shift instr -5) #x1)))
  (cond [(imm-flag)
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
      (define op (arithmetic-shift instr -12))
      ;; execute
      (case op
        [(OP_ADD) (do-add instr)]
        [(OP-AND) (do-and instr)]
        [(OP-NOT) (do-not instr)]
        [(OP-BR) (do-br instr)]
        [(OP-JMP) (do-jmp)])
      ;; update program counter
      (reg-write R-PC (add1 (reg-read R-PC)))
      (fetch-exec-iter)))
  fetch-exec-iter)
