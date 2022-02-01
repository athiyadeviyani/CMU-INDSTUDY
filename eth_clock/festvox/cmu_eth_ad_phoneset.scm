;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                     ;;;
;;;                     Carnegie Mellon University                      ;;;
;;;                  and Alan W Black and Kevin Lenzo                   ;;;
;;;                      Copyright (c) 1998-2000                        ;;;
;;;                        All Rights Reserved.                         ;;;
;;;                                                                     ;;;
;;; Permission is hereby granted, free of charge, to use and distribute ;;;
;;; this software and its documentation without restriction, including  ;;;
;;; without limitation the rights to use, copy, modify, merge, publish, ;;;
;;; distribute, sublicense, and/or sell copies of this work, and to     ;;;
;;; permit persons to whom this work is furnished to do so, subject to  ;;;
;;; the following conditions:                                           ;;;
;;;  1. The code must retain the above copyright notice, this list of   ;;;
;;;     conditions and the following disclaimer.                        ;;;
;;;  2. Any modifications must be clearly marked as such.               ;;;
;;;  3. Original authors' names are not deleted.                        ;;;
;;;  4. The authors' names are not used to endorse or promote products  ;;;
;;;     derived from this software without specific prior written       ;;;
;;;     permission.                                                     ;;;
;;;                                                                     ;;;
;;; CARNEGIE MELLON UNIVERSITY AND THE CONTRIBUTORS TO THIS WORK        ;;;
;;; DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING     ;;;
;;; ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT  ;;;
;;; SHALL CARNEGIE MELLON UNIVERSITY NOR THE CONTRIBUTORS BE LIABLE     ;;;
;;; FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES   ;;;
;;; WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN  ;;;
;;; AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,         ;;;
;;; ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF      ;;;
;;; THIS SOFTWARE.                                                      ;;;
;;;                                                                     ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Phonset for cmu_eth
;;;

;;;  Feeel free to add new feature values, or new features to this
;;;  list to make it more appropriate to your language

;; This is where it'll fall over if you haven't defined a 
;; a phoneset yet, if you have, delete this, if you haven't
;; define one then delete this error message
;; (error "You have not yet defined a phoneset for eth (and others things ?)\n            Define it in festvox/cmu_eth_ad_phoneset.scm\n")

(defPhoneSet
  cmu_eth
  ;;;  Phone Features
  (;; vowel or consonant
   (clst + - 0)
   (vc + - 0)
   ;; vowel length: short long dipthong schwa
   (vlng s l d a 0)
   ;; vowel height: high mid low
   (vheight 1 2 3 0 -)
   ;; vowel frontness: front mid back
   (vfront 1 2 3 0 -)
   ;; lip rounding
   (vrnd + - 0)
   ;; consonant type: stop fricative affricative nasal liquid approximant
   (ctype s f a n l r 0)
   ;; place of articulation: labial alveolar palatal
   ;; labio-dental dental velar glottal
   (cplace l a p b d v g 0)
   ;; consonant voicing
   (cvox + - 0)
   (asp  + - 0)
   (nuk + - 0)
   )
  (
   (    pau   -   -   0   0   0   0   0   0   -   -   -   ) 

   ;; insert the phones here, see examples in 
   ;; festival/lib/*_phones.scm

( A  - + l 3 3 - 0 0 0 - - ) ;; aa
( AI - + d 3 2 - 0 0 0 - - ) ;; ay
( CH - - 0 0 0 0 a p - - - ) ;; ch
( E  - + l 2 1 0 0 0 0 - - ) ;; eh
( D  - - 0 0 0 0 s a + - - ) ;; d
( G  - - 0 0 0 0 s v + - - ) ;; g
( H  - - 0 0 0 0 f g - - - ) ;; hh
( I  - - l 1 1 - 0 0 0 - - ) ;; iy
( JH - - 0 0 0 0 a p + - - ) ;; jh
( K  - - 0 0 0 0 s v - - - ) ;; k
( M  - - 0 0 0 0 n l + - - ) ;; m
( N  - - 0 0 0 0 n a + - - ) ;; n
( O  - + l 2 3 + 0 0 0 - - ) ;; ow 
( P  - - 0 0 0 0 s l + - - ) ;; p
( R  - - 0 0 0 0 r a + - - ) ;; r
( S  - - 0 0 0 0 f a + - - ) ;; s
( T  - - 0 0 0 0 s a - - - ) ;; t
( U  - + l 1 3 + 0 0 0 - - ) ;; uw
( Y  - - 0 0 0 0 r p + - - ) ;; y
( Z  - - 0 0 0 0 f a - - - ) ;; z

  )
)

(PhoneSet.silences '(pau))

(define (cmu_eth_ad::select_phoneset)
  "(cmu_eth_ad::select_phoneset)
Set up phone set for cmu_eth."
  (Parameter.set 'PhoneSet 'cmu_eth)
  (PhoneSet.select 'cmu_eth)
)

(define (cmu_eth_ad::reset_phoneset)
  "(cmu_eth_ad::reset_phoneset)
Reset phone set for cmu_eth."
  t
)

(provide 'cmu_eth_ad_phoneset)
