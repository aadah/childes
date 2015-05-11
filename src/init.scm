;;;; init.scm - Initializes the inverted-index
;;;; depends on: inverted-index.scm

;-------------------------------------------------------------------------------

(load "inverted-index")

;-------------------------------------------------------------------------------

;;; Initializing the inverted index
(define *ii* (make-inverted-index))

; Alice in Wonderland, by Lewis Carroll
(ii/add-doc! *ii* (create-plain-doc "alice.txt"))
; Bible (King James Version)
;(ii/add-doc! *ii* (create-plain-doc "bible.txt"))
; Paradise Lost, by John Milton
;(ii/add-doc! *ii* (create-plain-doc "paradise.txt"))
; The Republic, by Plato
;(ii/add-doc! *ii* (create-plain-doc "republic.txt"))
; The Adventures of Sherlock Holmes, by Arthur Conan Doyle
;(ii/add-doc! *ii* (create-plain-doc "sherlock.txt"))
; The Time Machine, by H. G. (Herbert George) Wells
(ii/add-doc! *ii* (create-plain-doc "time.txt"))

