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
;;; Tokenizer for idn
;;;
;;;  To share this among voices you need to promote this file to
;;;  to say festival/lib/cmu_idn/ so others can use it.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Load any other required files

;; Punctuation for the particular language
(set! cmu_idn_ad::token.punctuation "\"'`.,:;!?(){}[]")
(set! cmu_idn_ad::token.prepunctuation "\"'`({[")
(set! cmu_idn_ad::token.whitespace " \t\n\r")
(set! cmu_idn_ad::token.singlecharsymbols "")

;;; Voice/idn token_to_word rules 
(define (cmu_idn_ad::token_to_words token name)
  "(cmu_idn_ad::token_to_words token name)
Specific token to word rules for the voice cmu_idn_ad.  Returns a list
of words that expand given token with name."
  (cond
   ((string-matches name "[1-9][0-9]+")
    (cmu_idn::number token name))
   (t ;; when no specific rules apply do the general ones
    (list name))))

(define (cmu_idn::number token name)
  "(cmu_idn::number token name)
Return list of words that pronounce this number in idn."

  (error "cmu_idn::number to be written\n")

)

(define (cmu_idn_ad::select_tokenizer)
  "(cmu_idn_ad::select_tokenizer)
Set up tokenizer for idn."
  (Parameter.set 'Language 'cmu_idn)
  (set! token.punctuation cmu_idn_ad::token.punctuation)
  (set! token.prepunctuation cmu_idn_ad::token.prepunctuation)
  (set! token.whitespace cmu_idn_ad::token.whitespace)
  (set! token.singlecharsymbols cmu_idn_ad::token.singlecharsymbols)

  (set! token_to_words cmu_idn_ad::token_to_words)
)

(define (cmu_idn_ad::reset_tokenizer)
  "(cmu_idn_ad::reset_tokenizer)
Reset any globals modified for this voice.  Called by 
(cmu_idn_ad::voice_reset)."
  ;; None

  t
)

(provide 'cmu_idn_ad_tokenizer)
