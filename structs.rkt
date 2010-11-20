#lang racket
(provide (struct-out turn)
         (struct-out player)
         (struct-out state))

(struct turn (player position) #:transparent)
(struct player (name func) #:transparent)
(struct state (unused moves-list user computer winner) #:transparent)

