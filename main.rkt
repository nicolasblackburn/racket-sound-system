#lang racket

(provide (all-defined-out))

(random-seed 0)

(define bpm 125)
(define bytes-depth 1)
(define sample-rate 8000)
(define signed? #f)
(define big-endian? #f)

(define (compose-call . args)
  (let-values ([(lst-f x) (split-at-right args 1)])
    ((apply compose lst-f) (car x))))

(define (fun-op op)
  (lambda lst-f
    (lambda (t) (apply op (map (lambda (f) (f t)) lst-f)))))

(define add (fun-op +))
(define sub (fun-op -))
(define mult (fun-op *))
(define div (fun-op /))
(define mod (fun-op modulo))

(define (shift f x) 
  (lambda (t) (f (- t x))))

(define (lerp t0 x0 t1 x1 t)
  (+ (/ (* x0 (- t t1)) (- t0 t1)) (/ (* x1 (- t t0)) (- t1 t0))))

(define (adsr-envelope attack decay sustain release duration t)
  (cond [(< t 0) 0]
        [(< t attack) (/ t attack)]
        [(< t (+ attack decay)) (lerp attack 1 (+ attack decay) sustain t)]
        [(< t (- duration release)) sustain]
        [(< t duration) (lerp (- duration release) sustain duration 0 t)]
        [else 0]))

(define step-envelope (curry adsr-envelope 0.08 0 1 0.08))

;(define sample-rate 44100) ;Hz
;(define bit-depth 8) ;must be a power of 2
;(define playback-duration (* 60 2.5)) ;s

(define (make-noise sample-rate duration)
  (let* ([samples-count (* sample-rate duration)]
         [samples (make-vector samples-count)])
    (for ([i (in-range samples-count)])
      (vector-set! samples i (/ (- (* (random 2) 2) 1) 2)))
    (lambda (t)
      (vector-ref samples (modulo (exact-floor t) samples-count)))))

(struct note (tone octave) #:transparent)
(struct instrument (function envelope) #:transparent)
(struct frame (instrument note duration volume) #:transparent)
(struct clip (function duration) #:transparent)

(define c 0)
(define c# 1)
(define d 2)
(define d# 3)
(define e 4)
(define f 5)
(define f# 6)
(define g 7)
(define g# 8)
(define a 9)
(define a# 10)
(define b 11)

(define (pitch tone octave)
  (* (expt 2 (/ (+ (* octave 12) tone (- 57)) 12)) 440))

(define (instrument-value function envelope pitch duration t)
  (* (function (* pitch t)) (envelope duration t)))

(define (frames-function frames)
  (define (function-helper frames)
    (match frames
      ['() (lambda (t) 0)]
      [(list-rest (frame (instrument function envelope) (note tone octave) duration volume) rest)
         (lambda (t)
           (cond [(< t 0) 0]
                 [(< t duration) (* volume (instrument-value function envelope (pitch tone octave) duration t))]
                 [else ((function-helper rest) (- t duration))]))]))
  (function-helper frames))

(define (duration dur) (* bpm dur))

(define (frames-duration frames)
  (foldl (lambda (frame duration) (+ duration (frame-duration frame))) 0 frames))

(define (quantize bytes signed? big-endian? n)
  (integer->integer-bytes (- (exact-floor (* (- (expt 2 (* 8 bytes)) 1)
                                             (/ (+ 1 n) 2)))
                             (if signed? (expt 2 (- (* 8 bytes) 1))
                                 0))
                          bytes
                          signed?
                          big-endian?))

(define (write-samples quantize sample-rate duration fn)
  (for ([t (in-range (* sample-rate duration))])
    (display (compose-call quantize fn (curryr / sample-rate) t))))

(define sine (instrument (compose cos (curry * 2 pi)) step-envelope))
(define noise (instrument (compose (make-noise 8000 1) (curry * 2 pi)) step-envelope))

(define frames
  (list
   (frame sine (note a 1) 0.25 1)
   (frame sine (note a 1) 0.25 1)
   (frame sine (note c 2) 0.25 1)
   (frame sine (note a 1) 0.25 1)
   (frame sine (note d 2) 0.25 1)
   (frame sine (note c 2) 0.25 1)
   (frame sine (note a 1) 0.25 1)
   (frame sine (note g 1) 0.25 1)
   (frame sine (note a 1) 0.25 1)))

(write-samples (curry quantize 2 #t #f) 44100 (frames-duration frames) (frames-function frames))
;(require plot)
;(plot (function (frames-function frames) 0 (frames-duration frames)))