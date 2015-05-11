;;;; init.scm - Initializes the inverted-index
;;;; depends on: inverted-index.scm

;-------------------------------------------------------------------------------

(load "parser")
(load "inverted-index-maker")
(load "search-basic")

;-------------------------------------------------------------------------------

;;; Initializing the inverted index
(define *ii* (make-inverted-index))

(define (reset)
  (set! *ii* (make-inverted-index)))

(reset)

#|
; Alice in Wonderland, by Lewis Carroll
(ii/add-doc! *ii* (create-plain-doc "alice.txt"))
; Bible (King James Version)
;(ii/add-doc! *ii* (create-plain-doc "bible.txt"))
; Paradise Lost, by John Milton
(ii/add-doc! *ii* (create-plain-doc "paradise.txt"))
; The Republic, by Plato
(ii/add-doc! *ii* (create-plain-doc "republic.txt"))
; The Adventures of Sherlock Holmes, by Arthur Conan Doyle
(ii/add-doc! *ii* (create-plain-doc "sherlock.txt"))
; The Time Machine, by H. G. (Herbert George) Wells
(ii/add-doc! *ii* (create-plain-doc "time.txt"))
|#


(ii/add-doc! *ii* (create-ling-doc "english-na-mor/Bates/Free20/amy20.cha"))
(ii/add-doc! *ii* (create-ling-doc "english-na-mor/Bates/Free20/betty20.cha"))
(ii/add-doc! *ii* (create-ling-doc "english-na-mor/Bates/Free20/chuck20.cha"))


;(define (init type)
;  (let ((docs (parser type)))
;    (inverted-index-maker *ii* docs)))
