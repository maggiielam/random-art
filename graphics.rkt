#lang racket

; This file contains a function called draw that can be used to produce random
; art.  It assumes that the file hw5.rkt includes a working definition of
; all-pairs and generate.

(require "hw6.rkt")
(require racket/gui/base)

; produces a drawing of given height and width by generating three functions of
; given expression depth and using them to compute rgb values for each pixel
(define (draw width height depth)
  (define (to-intensity n) (inexact->exact (round (+ 127.5 (* n 127.5)))))
  (define (to-x i) (- (/ (* 2.0 i) width) 1.0))
  (define (to-y i) (- (/ (* 2.0 i) height) 1.0))
  (define (generate-function) (eval (list 'lambda '(x y) (generate depth))))
  (let* ((f1 (generate-function))
         (f2 (generate-function))
         (f3 (generate-function))
         (bitmap (make-object bitmap% width height))
         (bm-dc (make-object bitmap-dc% bitmap))
         (frame (new frame% (label "341 fun") (width width)
                     (height (+ height 20))))
         (canvas (new canvas% (parent frame)
                      (paint-callback
                       (lambda (canvas dc) 
                         (send dc draw-bitmap bitmap 0 0))))))
    (send bm-dc clear)
    (map (lambda (lst)
           (let* ((x1 (car lst))
                  (y1 (cadr lst))
                  (x2 (to-x x1))
                  (y2 (to-y y1))
                  (rgb (lambda (f) (to-intensity (f x2 y2)))))
             (send bm-dc set-pixel x1 y1
                   (make-object color% (rgb f1) (rgb f2) (rgb f3)))))
         (all-pairs (range 0 width) (range 0 height)))
    (send frame show #t)))

; variation of draw that keeps drawing new pictures
(define (draw2 width height depth)
  (define (to-intensity n) (inexact->exact (round (+ 127.5 (* n 127.5)))))
  (define (to-x i) (- (/ (* 2.0 i) width) 1.0))
  (define (to-y i) (- (/ (* 2.0 i) height) 1.0))
  (define (generate-function) (eval (list 'lambda '(x y) (generate depth))))
  (letrec ((bitmap (make-object bitmap% width height))
           (bm-dc (make-object bitmap-dc% bitmap))
           (frame (new frame% (label "341 fun") (width width)
                       (height (+ height 20))))
           (canvas (new canvas% (parent frame)
                        (paint-callback (lambda (canvas dc) 
                                          (send dc draw-bitmap bitmap 0 0)))))
           (loop (lambda ()
                   (let* ((f1 (generate-function))
                          (f2 (generate-function))
                          (f3 (generate-function)))
                     (send bm-dc clear)
                     (map (lambda (lst)
                            (let* ((x1 (car lst))
                                   (y1 (cadr lst))
                                   (x2 (to-x x1))
                                   (y2 (to-y y1))
                                   (rgb (lambda (f) (to-intensity (f x2 y2)))))
                              (send bm-dc set-pixel x1 y1
                                    (make-object color% (rgb f1) (rgb f2) (rgb f3)))))
                          (all-pairs (range 0 width) (range 0 height)))
                                          (send frame show #f)
                     (send frame show #f)
                     (send frame show #t)
                     (sleep/yield 1)
                     (loop)))))
    (loop)))
