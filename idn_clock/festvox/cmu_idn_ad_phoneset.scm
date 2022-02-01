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
;;; Phonset for cmu_idn
;;;

;;;  Feeel free to add new feature values, or new features to this
;;;  list to make it more appropriate to your language

;; This is where it'll fall over if you haven't defined a 
;; a phoneset yet, if you have, delete this, if you haven't
;; define one then delete this error message
;; (error "You have not yet defined a phoneset for idn (and others things ?)\n            Define it in festvox/cmu_idn_ad_phoneset.scm\n")

(defPhoneSet
  cmu_idn
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

   (A   -   +   l   3   3   -   0   0   0   -   -) ;; a like father 
   (AH  -   +   s   2   2   -   0   0   0   -   -) ;; ah like but
   (AI  -   +   d   3   2   -   0   0   0   -   -) ;; ay 
   (AW  -   +   d   3   2   -   0   0   0   -   -) ;; how
   (B   -   -   0   0   0   0   s   l   +   -   -)
   (C   -   -   0   0   0   0   a   p   -   -   -)
   (D   -   -   0   0   0   0   s   a   +   -   -)
   (E   -   +   s   2   1   -   0   0   0   -   -) ;; eh
   (F   -   -   0   0   0   0   f   b   -   -   -)
   (G   -   -   0   0   0   0   s   v   +   -   -)
   (H   -   -   0   0   0   0   f   g   -   -   -)
   (I   -   -   l   1   1   -   0   0   0   -   -) ;; iy
   (JH  -   -   0   0   0   0   a   p   +   -   -)
   (K   -   -   0   0   0   0   s   v   -   -   -)
   (L   -   -   0   0   0   0   l   a   +   -   -)
   (M   -   -   0   0   0   0   n   l   +   -   -)
   (N   -   -   0   0   0   0   n   a   +   -   -)
   (NG  -   -   0   0   0   0   n   v   +   -   -)
   (O   -   +   d   2   3   +   0   0   0   -   -) ;; ow
   (P   -   -   0   0   0   0   s   l   -   -   -)
   (R   -   -   0   0   0   0   r   a   +   -   -)
   (S   -   -   0   0   0   0   f   a   -   -   -)
   (T   -   -   0   0   0   0   s   a   -   -   -)
   (U   -   +   s   1   3   +   0   0   0   -   -) ;; uh
   (V   -   -   0   0   0   0   f   b   +   -   -)
   (W   -   -   0   0   0   0   r   l   +   -   -)
   (Y   -   -   0   0   0   0   r   p   +   -   -)
   (Z   -   -   0   0   0   0   f   a   +   -   -)

  )
)

(PhoneSet.silences '(pau))

(define (cmu_idn_ad::select_phoneset)
  "(cmu_idn_ad::select_phoneset)
Set up phone set for cmu_idn."
  (Parameter.set 'PhoneSet 'cmu_idn)
  (PhoneSet.select 'cmu_idn)
)

(define (cmu_idn_ad::reset_phoneset)
  "(cmu_idn_ad::reset_phoneset)
Reset phone set for cmu_idn."
  t
)

(provide 'cmu_idn_ad_phoneset)
