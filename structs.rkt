#lang racket
(provide (struct-out turn)
         (struct-out player)
         (struct-out state))

(struct turn (player position message) #:prefab)
(struct player (name func) #:prefab)
(struct state (unused moves-list x o winner) #:prefab)

