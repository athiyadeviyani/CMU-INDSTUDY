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
;;; Lexicon, LTS and Postlexical rules for cmu_idn
;;;

;;; Load any necessary files here

(define (cmu_idn_addenda)
  "(cmu_idn_addenda)
Basic lexicon should (must ?) have basic letters, symbols and punctuation."

;;; Pronunciation of letters in the alphabet
;(lex.add.entry '("a" nn (((a) 0))))
;(lex.add.entry '("b" nn (((b e) 0))))
;(lex.add.entry '("c" nn (((th e) 0))))
;(lex.add.entry '("d" nn (((d e) 0))))
;(lex.add.entry '("e" nn (((e) 0))))
; ...

;;; Symbols ...
;(lex.add.entry 
; '("*" n (((a s) 0) ((t e) 0) ((r i1 s) 1)  ((k o) 0))))
;(lex.add.entry 
; '("%" n (((p o r) 0) ((th i e1 n) 1) ((t o) 0))))

;; Basic punctuation must be in with nil pronunciation
(lex.add.entry '("." punc nil))
;(lex.add.entry '("." nn (((p u1 n) 1) ((t o) 0))))
(lex.add.entry '("'" punc nil))
(lex.add.entry '(":" punc nil))
(lex.add.entry '(";" punc nil))
(lex.add.entry '("," punc nil))
;(lex.add.entry '("," nn (((k o1) 1) ((m a) 0))))
(lex.add.entry '("-" punc nil))
(lex.add.entry '("\"" punc nil))
(lex.add.entry '("`" punc nil))
(lex.add.entry '("?" punc nil))
(lex.add.entry '("!" punc nil))

(lex.add.entry '( "tepat" nil (((T E) 0) ((P A T) 0))))
(lex.add.entry '( "tiga" nil (((T I) 0) ((G A) 0))))
(lex.add.entry '( "sebelas" nil (((S E) 0) ((B E) 0) ((L A S) 0))))
(lex.add.entry '( "tengah" nil (((T E) 0) ((NG AH) 0))))
(lex.add.entry '( "malam" nil (((M A) 0) ((L A M) 0))))
(lex.add.entry '( "lewat" nil (((L E) 0) ((W A T) 0))))
(lex.add.entry '( "satu" nil (((S A) 0) ((T U) 0))))
(lex.add.entry '( "pagi" nil (((P A) 0) ((G I) 0))))
(lex.add.entry '( "sekarang" nil (((S E) 0) ((K A) 0) ((R A NG) 0))))
(lex.add.entry '( "hampir" nil (((H A M) 0) ((P I R) 0))))
(lex.add.entry '( "pukul" nil (((P U) 0) ((K U L) 0))))
(lex.add.entry '( "sepuluh" nil (((S E) 0) ((P U) 0) ((L U H) 0))))
(lex.add.entry '( "sampai" nil (((S A M) 0) ((P AI) 0))))
(lex.add.entry '( "puluh" nil (((P U) 0) ((L U H) 0))))
(lex.add.entry '( "delapan" nil (((D E) 0) ((L A) 0) ((P A N) 0))))
(lex.add.entry '( "di" nil (((D I) 0) )))
(lex.add.entry '( "enam" nil (((E) 0) ((N A M) 0))))
(lex.add.entry '( "belas" nil (((B E) 0) ((L A S) 0))))
(lex.add.entry '( "siang" nil (((S I) 0) ((A N G) 0))))
(lex.add.entry '( "setengah" nil (((S E) 0) ((T E) 0) ((NG AH) 0))))
(lex.add.entry '( "empat" nil (((E M) 0) ((P A T) 0))))
(lex.add.entry '( "lima" nil (((L I) 0) ((M A) 0))))
(lex.add.entry '( "sembilan" nil (((S E M) 0) ((B I) 0) ((L A N) 0))))
(lex.add.entry '( "hingga" nil (((H I NG) 0) ((G A) 0))))
(lex.add.entry '( "hari" nil (((H A) 0) ((R I) 0))))
(lex.add.entry '( "seperempat" nil (((S E) 0) ((P E R) 0) ((E M) 0) ((P A T) 0))))
(lex.add.entry '( "waktunya" nil (((W A K) 0) ((T U) 0) ((N Y A) 0))))
(lex.add.entry '( "sore" nil (((S O) 0) ((R E) 0))))
(lex.add.entry '( "dua" nil (((D U) 0) ((A) 0) )))
(lex.add.entry '( "tujuh" nil (((T U) 0) ((JH U H) 0))))
(lex.add.entry '( "sedikit" nil (((S E) 0) ((D I) 0) ((K I T) 0))))
(lex.add.entry '( "kurang" nil (((K U) 0) ((R A NG) 0))))
(lex.add.entry '( "atau" nil (((A) 0) ((T AW) 0))))
(lex.add.entry '( "setelah" nil (((S E) 0) ((T E) 0) ((L A H) 0))))

)

(set! eng_map
'(
(A aa)
(AH ah)
(AW aw)
(AI ay)
(B b)
(C ch)
(D d)
(E eh)
(F f)
(G g)
(H hh)
(I iy)
(JH jh)
(K k)
(L l)
(M m)
(N n)
(NG ng)
(O ow)
(P p)
(R r)
(S s)
(T t)
(U uw)
(V v)
(W w)
(Y y)
(Z x)
(pau pau)
)) 

(define (do_eng_map utt)
  (mapcar
   (lambda (s)
     (set! x (assoc_string (item.name s) eng_map))
     (if x 
         (item.set_feat s "us_diphone" (cadr x))
         (format t "no eng_map for %s\n" (item.name s))
         )
     )
   (utt.relation.items utt 'Segment))
  utt)

(require 'lts)

;;;  Function called when word not found in lexicon
;;;  and you've trained letter to sound rules
(define (cmu_idn_lts_function word features)
  "(cmu_idn_lts_function WORD FEATURES)
Return pronunciation of word not in lexicon."

  ;; If you have nothing ...
  (format t "Unknown word %s\n" word)
  (list word features nil)

  ;; If you have lts rules (trained or otherwise)
;  (if (not boundp 'cmu_idn_lts_rules)
;      (require 'cmu_idn_lts_rules))
;  (let ((dword (downcase word)) (phones) (syls))
;    (set! phones (lts_predict dword cmu_idn_lts_rules))
;    (set! syls (cmu_idn_lex_syllabify_phstress phones))
;    (list word features syls))
  )

(define (cmu_idn_map_modify ps)
  (cond
   ((null ps) nil)
   ((null (cdr ps)) ps)
   ((assoc_string (string-append (car ps) (cadr ps))
                   cmu_idn_ad_char_phone_map)
    (cons
     (string-append (car ps) (cadr ps))
     (cmu_idn_map_modify (cddr ps))))
   (t
    (cons
     (car ps)
     (cmu_idn_map_modify (cdr ps))))))

(define (cmu_idn_map_phones p)
  (cond
   ((null p) nil)
   (t
    (let ((a (assoc_string (car p) cmu_idn_ad_char_phone_map)))
      (cond
       (a (cons (cadr a) (cmu_idn_map_phones (cdr p))))
       (t (cmu_idn_map_phones (cdr p))))))))

(define (cmu_idn_is_vowel x)
  (string-equal "+" (phone_feature x "vc")))

(define (cmu_idn_contains_vowel l)
  (member_string
   t
   (mapcar (lambda (x) (cmu_idn_is_vowel x)) l)))

(define (cmu_idn_lex_sylbreak currentsyl remainder)
  "(cmu_idn_lex_sylbreak currentsyl remainder)
t if this is a syl break, nil otherwise."
  (cond
   ((not (cmu_idn_contains_vowel remainder))
    nil)
   ((not (cmu_idn_contains_vowel currentsyl))
    nil)
   (t
    ;; overly naive, I mean wrong
    t))
)

(define (cmu_idn_lex_syllabify_phstress phones)
 (let ((syl nil) (syls nil) (p phones) (stress 0))
    (while p
     (set! syl nil)
     (set! stress 0)
     (while (and p (not (cmu_idn_lex_sylbreak syl p)))
       (if (string-matches (car p) "xxxx")
           (begin
             ;; whatever you do to identify stress
             (set! stress 1)
             (set syl (cons (car p-stress) syl)))
           (set! syl (cons (car p) syl)))
       (set! p (cdr p)))
     (set! syls (cons (list (reverse syl) stress) syls)))
    (reverse syls)))

(if (probe_file (path-append cmu_idn_ad::dir "festvox/lex_lts_rules.scm"))
    (begin
      (load (path-append cmu_idn_ad::dir "festvox/lex_lts_rules.scm"))
      (set! cmu_idn_lts_rules lex_lts_rules)))

    ;; utf8-sampa map based on unitran 
(if (probe_file (path-append cmu_idn_ad::dir "festvox/cmu_idn_ad_char_phone_map.scm"))
    (begin
      (set! cmu_idn_ad_char_phone_map
            (load (path-append cmu_idn_ad::dir 
                               "festvox/cmu_idn_ad_char_phone_map.scm") t))
	(load (path-append cmu_idn_ad::dir 
                           "festvox/unicode_sampa_mapping.scm"))

    ;; utf8-indic-sampa letter based one
    (define (cmu_idn_lts_function word features)
      "(cmu_idn_lts_function WORD FEATURES)
Return pronunciation of word not in lexicon."
      (let ((dword word) (phones) (syls) (aphones))
        (if (boundp 'cmu_idn_lts_rules)
            (set! phones (lts_predict (utf8explode dword) cmu_idn_lts_rules))
            (begin
              (set! aphones (cmu_idn_map_modify (utf8explode dword)))
              (set! phones (cmu_idn_map_phones aphones))
              (set! phones (sampa_lookup phones))))
;        (set! phones (indic_unicode_lts sphones))
        (set! syls (cmu_idn_lex_syllabify_phstress phones))
        (list word features syls)))
    ))

(define (sampa_lookup gphones)
  (let ((phlist nil) (sp nil))
    (mapcar 
     (lambda (gg)
       (set! sp (assoc_string gg unicode_sampa_mapping))
       (if sp
           (set! phlist (append (reverse (car (cadr sp))) phlist))
           (set! phlist (cons gg phlist))))
     gphones)
    (reverse phlist)))

(define (indic_unicode_lts phlist)
	(set! finallist (list))
	(set! graphemecount 0)
	(set! prevgrapheme (list))
	(set! totgcnt (- (length phlist) 1))
	(mapcar (lambda (ggg)
		(if (symbol? (car ggg))
		(begin
		(cond
			;; schwa deletion for the last consonant
			((equal? graphemecount totgcnt)
			(begin
				(if (string-equal (phone_feature (car ggg) 'vc) "-")
				(begin 
					(if (string-equal (phone_feature (car prevgrapheme) 'vc) "-") 
					(set! finallist (append  finallist prevgrapheme)))
					;(set! finallist (append finallist (list (car ggg)))) ;appropriate for hindi
					(set! finallist (append finallist  ggg)) ; for generic (non-schwa final) indic
				)
				(begin 
					(if (string-equal (phone_feature (car prevgrapheme) 'vc) "-") 
					(set! finallist (append finallist (list (car prevgrapheme)))))
					(set! finallist (append finallist (list (car ggg))))
				))
			))
			;; generic treatment for an intermediate grapheme
			((and (> graphemecount 0) (< graphemecount totgcnt))
			(begin
				(cond 
					;; If current is vowel, remove the previous schwa
					((and (string-equal (phone_feature (car ggg) 'vc) "+") (string-equal (phone_feature (car prevgrapheme) 'vc) "-"))
					(begin 
						(set! finallist (append finallist (list (car prevgrapheme))))
						(set! finallist (append finallist (list (car ggg))))
					))
					;; If current is consonant and previous is consonant, dump all of previous 
					((and  (string-equal (phone_feature (car ggg) 'vc) "-") (string-equal (phone_feature (car prevgrapheme) 'vc) "-"))
					(set! finallist (append finallist prevgrapheme)))
					(t 
					 t)
				)
			))
			((and (eq? graphemecount 0) (string-equal (phone_feature (car ggg) 'vc) "+"))
				(set! finallist (list (car ggg)))
			)
			(t 
			t)
		)
		(set! graphemecount (+ 1 graphemecount))
		(set! prevgrapheme ggg)
		)
		(begin 
			(cond
				((equal? (car ggg) '(P))
					(set! finallist (append finallist (list (car prevgrapheme))))
					(set! prevgrapheme (list))
				)
				((equal? (car ggg) '(M))
					(if (string-equal (phone_feature (car prevgrapheme) 'vc) "-") (set! finallist (append finallist prevgrapheme)))
					(set! finallist (append finallist (list "nB")))
					(set! prevgrapheme (list))
				)
				((equal? (car ggg) '(CD))
					(if (string-equal (phone_feature (car prevgrapheme) 'vc) "-") (set! finallist (append finallist prevgrapheme)))
					(set! finallist (append finallist (list "nB")))
					(set! prevgrapheme (list))
				)
				(t
				t)
				;(format t "debug: todo \n")
			)
			(set! graphemecount (+ 1 graphemecount))
		)
	)
	) phlist)
finallist)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; OR: Hand written letter to sound rules
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; ;;;  Function called when word not found in lexicon
; (define (cmu_idn_lts_function word features)
;   "(cmu_idn_lts_function WORD FEATURES)
; Return pronunciation of word not in lexicon."
;   (format stderr "failed to find pronunciation for %s\n" word)
;   (let ((dword (downcase word)))
;     ;; Note you may need to use a letter to sound rule set to do
;     ;; casing if the language has non-ascii characters in it.
;     (if (lts.in.alphabet word 'cmu_idn)
; 	(list
; 	 word
; 	 features
; 	 ;; This syllabification is almost certainly wrong for
; 	 ;; this language (its not even very good for English)
; 	 ;; but it will give you something to start off with
; 	 (lex.syllabify.phstress
; 	   (lts.apply word 'cmu_idn)))
; 	(begin
; 	  (format stderr "unpronouncable word %s\n" word)
; 	  ;; Put in a word that means "unknown" with its pronunciation
; 	  '("nepoznat" nil (((N EH P) 0) ((AO Z) 0) ((N AA T) 0))))))
; )

; ;; You may or may not be able to write a letter to sound rule set for
; ;; your language.  If its largely lexicon based learning a rule
; ;; set will be better and easier that writing one (probably).
; (lts.ruleset
;  cmu_idn
;  (  (Vowel WHATEVER) )
;  (
;   ;; LTS rules 
;   ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Postlexical Rules 
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (cmu_idn::postlex_rule1 utt)
  "(cmu_idn::postlex_rule1 utt)
A postlexical rule form correcting phenomena over word boundaries."
  (mapcar
   (lambda (s)
     ;; do something
     )
   (utt.relation.items utt 'Segment))
   utt)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Lexicon definition
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(lex.create "cmu_idn")
(lex.set.phoneset "cmu_idn")
(lex.set.lts.method 'cmu_idn_lts_function)
(if (probe_file (path-append cmu_idn_ad::dir "festvox/cmu_idn_lex.out"))
    (lex.set.compile.file (path-append cmu_idn_ad::dir 
                                       "festvox/cmu_idn_lex.out")))
(cmu_idn_addenda)
(if (probe_file (path-append cmu_idn_ad::dir "festvox/cmu_idn_addenda.scm"))
    (load (path-append cmu_idn_ad::dir "festvox/cmu_idn_addenda.scm")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Lexicon setup
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (cmu_idn_ad::select_lexicon)
  "(cmu_idn_ad::select_lexicon)
Set up the lexicon for cmu_idn."
  (lex.select "cmu_idn")

  ;; Post lexical rules
  ;; (set! postlex_rules_hooks (list cmu_idn::postlex_rule1))
  (set! postlex_rules_hooks (list 
                             do_eng_map
                             cmu_idn::postlex_rule1))
)

(define (cmu_idn_ad::reset_lexicon)
  "(cmu_idn_ad::reset_lexicon)
Reset lexicon information."
  t
)

(provide 'cmu_idn_ad_lexicon)
