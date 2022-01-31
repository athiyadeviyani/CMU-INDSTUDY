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
;;;                                                                       ;;
;;;  A generic voice definition file for a limited domain synthesizer     ;;
;;;  Cutomsized for: time                                                 ;;
;;;                                                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require_module 'clunits)
(require 'clunits)

(if (assoc 'cmu_time_ad_ldom voice-locations)
    (defvar cmu_time_ad::ldom_dir 
      (cdr (assoc 'cmu_time_ad_ldom voice-locations)))
    (defvar cmu_time_ad::ldom_dir (string-append (pwd) "/")))

(if (not (probe_file (path-append cmu_time_ad::ldom_dir "festvox/")))
    (begin
     (format stderr "cmu_time_ad::ldom: Can't find voice scm files they are not in\n")
     (format stderr "   %s\n" (path-append  cmu_time_ad::ldom_dir "festvox/"))
     (format stderr "   Either the voice isn't linked in Festival library\n")
     (format stderr "   or you are starting festival in the wrong directory\n")
     (error)))

;;;  Add the directory contains general voice stuff to load-path
(set! load-path (cons (path-append cmu_time_ad::ldom_dir "festvox/") 
		      load-path))

;;; Flag to save multiple loading of db
(defvar cmu_time_ad::ldom_loaded nil)
;;; When set to non-nil ldom voices *always* use their closest voice
;;; this is used when generating the prompts
(defvar cmu_time_ad::ldom_prompting_stage nil)
;;; Flag to allow new lexical items to be added only once
(defvar cmu_time_ad::added_extra_lex_items nil)

;;; Use backup voice, or use excuse phrase, if this is non-nill
;;; it will use a backup phrase to speak instead of using the
;;; backup voice
(defvar cmu_time_ad::backup_phrase nil)
;;; Show time to synthesize
(defvar cmu_time_ad::show_times nil)
;;; Show unit selection
(defvar cmu_time_ad::show_unit_selection nil)

;;; The front end to the limited domain
(require 'cmu_time_ad)

;;; You may wish to change this 
(set! cmu_time_ad::closest_voice 'voice_kal_diphone)

;;;  These are the parameters which are needed at run time
;;;  build time parameters are added to his list in cmu_time_ad_build.scm
(set! cmu_time_ad::dt_params
      (list
       (list 'db_dir cmu_time_ad::ldom_dir)
       '(name cmu_time_ad)
       '(index_name cmu_time_ad)
       '(f0_join_weight 0.0)
       '(join_weights
         (0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5))
       '(trees_dir "festival/trees/")
       '(catalogue_dir "festival/clunits/")
       '(coeffs_dir "mcep/")
       '(coeffs_ext ".mcep")
       '(clunit_name_feat lisp_cmu_time_ad::clunit_name)
       ;;  Run time parameters 
;       '(join_method windowed)
       ;; if pitch mark extraction is bad this is better than the above
       '(join_method smoothedjoin)
       '(continuity_weight 100)
       '(optimal_coupling 2)
       '(extend_selections 10)
       '(pm_coeffs_dir "mcep/")
       '(pm_coeffs_ext ".mcep")
       '(sig_dir "wav/")
       '(sig_ext ".wav")
;       '(clunits_debug 1)
))

(define (cmu_time_ad::clunit_name i)
  "(cmu_time_ad::clunit_name i)
Defines the unit name for unit selection for time.  The can be modified
changes the basic claissifncation of unit for the clustering.  By default
this does segment plus word, which is reasonable for many ldom domains."
  (let ((name (item.name i)))
    (if (string-equal 
	 name (car (cadr (car (PhoneSet.description '(silences))))))
	;; Do something simpler around pauses
	(string-append
	 name
	 "_"
	 (item.feat i "p.name"))
	;; For all other phones do phone plus word
	;; You could add other things here (like word position)
	(let ((downcasedword 
	       (downcase 
		(item.feat i "R:SylStructure.parent.parent.name"))))
	  (string-append
	   name   ;; segment name
	   "_"
	   (if (string-matches downcasedword ".*'.*")
	       (format nil "%s%s" (string-before downcasedword "'")
		       (string-after downcasedword "'"))
	       (format nil "%s" downcasedword)))))))

(define (cmu_time_ad::ldom_load)
  "(cmu_time_ad::ldom_load)
Function that actual loads in the databases and selection trees.
SHould only be called once per session."
  (set! dt_params cmu_time_ad::dt_params)
  (set! clunits_params cmu_time_ad::dt_params)
  (clunits:load_db clunits_params)
  (load (string-append
	 (string-append 
	  cmu_time_ad::ldom_dir "/"
	  (get_param 'trees_dir dt_params "trees/")
	  (get_param 'index_name dt_params "all")
	  ".tree")))
  (set! cmu_time_ad::ldom_clunit_selection_trees clunits_selection_trees)
  (set! cmu_time_ad::ldom_loaded t))

(set! cmu_time_ad::phrase_cart_tree
'
((lisp_token_end_punc in ("'" "\"" "?" "." "," ":" ";"))
  ((B))
  ((n.name is 0)
   ((B))
   ((n.name in ("." "..." "...."))
    ((name is ".")
     ((NB))
     ((B)))
    ((NB))))))

(define (cmu_time_ad::token_to_words token name)
  (cond
   ;; Domain specific token to word rules
   (t
    (cmu_time_ad::old_token_to_words token name))))

(define (cmu_time_ad::domain_specific_voice_setup)
  "(cmu_time_ad::domain_specific_voice_setup)
Setups an token to word rules, lexical entries, prosody mopdels specific
to this domain.  This function is also called at prompt generation time
to ensure the prompts follow the same rules."
  ;; Let the cluster stuff decide about vowel reduction
  (set! cmu_time_ad::old_postlex_vowel_reduce_table 
	           postlex_vowel_reduce_table)
  (set! postlex_vowel_reduce_table nil)

  ;; Seems things work better with explicit punctuation phrase breaks
  ;; in limited domain (as things will be more consistent
  (set! phrase_cart_tree cmu_time_ad::phrase_cart_tree)
  (Parameter.set 'Phrase_Method 'cart_tree)

  ;; If you want to have domain specific text processing this will
  ;; cause cmu_time_ad::token_to_word to be called
  (set! cmu_time_ad::old_token_to_words english_token_to_words)

  (set! english_token_to_words cmu_time_ad::token_to_words)
  (set! token_to_words cmu_time_ad::token_to_words)

  (if (not cmu_time_ad::added_extra_lex_items)
      (begin
	;; (lex.add.entry '("dadada" nn (((d aa) 1) (( d aa) 1) ((d aa) 1))))
	(set! cmu_time_ad::added_extra_lex_items t)
	))
)

(define (cmu_time_ad::voice_reset)
  "(cmu_time_ad::voice_reset)
Reset global variables back to previous voice."
  (set! english_token_to_words cmu_time_ad::old_token_to_words)
  (set! token_to_words cmu_time_ad::old_token_to_words)
  (set! utt.synth cmu_time_ad::real_utt.synth)
  (set! tts_hooks (delq cmu_time_ad::utt.synth tts_hooks))
  (set! tts_hooks (cons utt.synth tts_hooks))
  (set! postlex_vowel_reduce_table 
	cmu_time_ad::old_postlex_vowel_reduce_table)
  (set! cluster_synth_pre_hooks nil)
  (set! cluster_synth_post_hooks nil)
  t
)

(define (cmu_time_ad::utt.synth utt)
  "(cmu_time_ad::utt.synth utt)
Uses the time voice to synthesize utt, but if that fails 
back off to the defined closest voice (probably a diphone synthesizer)."
  (let ((starttime (clunits::time)))
  (if cmu_time_ad::show_times
      (format t "START: %s\n"
	      (read-from-string (utt.feat utt "iform"))))
  (unwind-protect 
   (begin
     (cmu_time_ad::real_utt.synth utt)
     ;; Successful synthesis
     )
   (begin
     ;; The above failed, so resynthesize with the backup voice
;     (format stderr "ldom/clunits failed\n")
     (if cmu_time_ad::backup_phrase
	 (begin
            (set! utt (cmu_time_ad::real_utt.synth 
		(eval
		 (list 'Utterance 'Text
		       cmu_time_ad::backup_phrase))))
	   )
	 (begin  ;; else use the backup voices
	   (eval (list cmu_time_ad::closest_voice))  ;; call backup voice
	   (set! utt2 (cmu_time_ad::copy_tokens utt))
	   (set! utt (utt.synth utt2))
	   (voice_cmu_time_ad_ldom)                 ;; return to ldom voice
	   ))
     ))
  (if cmu_time_ad::show_unit_selection
      (cmu_time_ad::clunits_units_selected utt "-"))
  (if cmu_time_ad::show_times
      (let ((synthtime (- (clunits::time) starttime))
	    (wavelength 
	     (/ (cadr (assoc 'num_samples (wave.info (utt.wave utt))))
		(cadr (assoc 'sample_rate (wave.info (utt.wave utt)))))))
	(format t "END: synthtime %2.3f wavelength %2.3f (%2.3f)\n"
		synthtime
		wavelength
		(/ wavelength synthtime))))
  utt)
)

(define (cmu_time_ad::fix_pauses utt)
  "(cmu_time_ad::fix_pauses utt)
Remove double pauses at start and end of utterance."
  (mapcar
   (lambda (s)
     (cond
      ;; remove final pause
      ((and (not (item.next s))
	    (string-equal (item.name s)
		  (car (cadr (car (PhoneSet.description '(silences)))))))
       (item.delete s))
      ;; remove inital pause
      ((and (not (item.prev s))
	    (string-equal (item.name s)
	     (car (cadr (car (PhoneSet.description '(silences)))))))
       (item.delete s))))
   (utt.relation.items utt 'Segment))
  utt
)

(define (cmu_time_ad::copy_tokens utt)
  "(cmu_time_ad::copy_tokens utt)
This should be standard library, of in fact not even needed.  It constructs
a new utterance from the tokens in utt so it may be safely resynthesized."
  (let ((utt2 (Utterance Tokens nil))
	(oldtok (utt.relation.first utt 'Token)))
    (utt.relation.create utt2 'Token)
    (while oldtok
      (let ((ntok (utt.relation.append utt2 'Token)))
	(mapcar
	 (lambda (fp)
	   (item.set_feat ntok (car fp) (car (cdr fp))))
	 (cdr (item.features oldtok)))
	(set! oldtok (item.next oldtok))))
    utt2))

(define (voice_cmu_time_ad_ldom)
  "(voice_cmu_time_ad_ldom)
Define voice for limited domain: time."
  ;; Blindly use basic parameters of closest voice
  (eval (list cmu_time_ad::closest_voice))

  (cmu_time_ad::domain_specific_voice_setup)

  ;; When using a backup voice for out of domain utts
  ;; we redefine utt.synth to our own (if we haven't already)
  (if (not (equal? utt.synth cmu_time_ad::utt.synth))
      (begin
	;; This actually isn't general enough, but it will do.
	(set! tts_hooks (delq utt.synth tts_hooks))
	(set! tts_hooks (cons cmu_time_ad::utt.synth tts_hooks))
	(set! cmu_time_ad::real_utt.synth utt.synth)
	(set! utt.synth cmu_time_ad::utt.synth)))

  ;; Load in the clunits databases (or select it if its already loaded)
  (if (not cmu_time_ad::ldom_prompting_stage)
      (begin
	(if (not cmu_time_ad::ldom_loaded)
	    (cmu_time_ad::ldom_load)
	    (clunits:select 'cmu_time_ad))
	(set! clunits_selection_trees 
	      cmu_time_ad::ldom_clunit_selection_trees)
	(Parameter.set 'Synth_Method 'Cluster)))
  

;  (set! cluster_synth_pre_hooks (list cmu_time_ad::fix_pauses))
  (set! cluster_synth_pre_hooks nil)

  ;; This is where you can modify power (and sampling rate) if desired
  (set! after_synth_hooks nil)
;  (set! after_synth_hooks
;      (list
;        (lambda (utt)
;          (utt.wave.rescale utt 2.1))))

  (set! current_voice_reset cmu_time_ad::voice_reset)

  (set! current-voice 'cmu_time_ad_ldom)
)

(if (not (boundp 'clunits::time))
    ;;; Its an older version of clunits and doesn't have this so fake it
    (define (clunits::time)
      "(clunits::time)
      An improper version of clunits::time. upgrade your festopt_clunits
      to get a more accurate one."
      (let ((tmpfile (make_tmp_filename))
	    (hms))
	(system (format nil "date %s >%s" "+%H%t%M%t%S" tmpfile))
	(set! hms (load tmpfile t))
	(delete-file tmpfile)
	(+ (* 12 (car hms))
	   (* 60 (cadr hms))
	   (caddr hms)))))

(define (cmu_time_ad::clunits_units_selected utt filename)
  "(cmu_time_ad::clunits_units_selected utt filename)
Output selected unitsfile indexes for each unit in the given utterance.
Results saved in given file name, or stdout if filename is \"-\"."
  (let ((fd (if (string-equal filename "-")
		t
		(fopen filename "w")))
	(sample_rate
	 (cadr (assoc 'sample_rate (wave.info (utt.wave utt))))))
    (mapcar
     (lambda (s)
       (format fd "%s\t%s\t%10s\t%f\t%f\n"
	       (string-before (item.name s) "_")
	       (item.name s)
	       (item.feat s "fileid")
	       (item.feat s "middle")
	       (+ 
		(item.feat s "middle")
		(/ (- (item.feat s "samp_end")
		      (item.feat s "samp_start"))
		   sample_rate)))
       )
     (utt.relation.items utt 'Unit))
    (if (not (string-equal filename "-"))
	(fclose fd))
    t))

(provide 'cmu_time_ad_ldom)

