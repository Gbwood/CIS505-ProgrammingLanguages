#lang racket

(provide lookup)
(provide Leaf)
(provide Node)
(provide average)

(struct Leaf (num))
(struct Node (left right))

(define (lookup ls key) 
	(cond
		[(eq? key "") (display "Error\n")]
		[(eq? key (car (first ls))) 
			(cdr (first ls))]
		[else (lookup (rest ls) key)]))

(define (average t)
	(cond 
		[(null?) ("null")]
		[(Leaf? t)
			(Leaf-num t)]
		[(Node? t)
			(/ (+ (average (Node-left t)) 
				   (average (Node-right t))
				   )
			   2 
			   )]))
		
		
		