;;; -----------------
;;; GENERAL UTILITIES
;;; -----------------

(defun flatten (obj)
  (do* ((result (list obj))
        (node result))
       ((null node) (delete nil result))
    (cond ((consp (car node))
           (when (cdar node) (push (cdar node) (cdr node)))
           (setf (car node) (caar node)))
          (t (setf node (cdr node))))))

(defun sublistp (input)
  ;; Returns T if <input> is a list of lists.
  (when (and (listp input) (loop for i in input thereis (listp i))) t))

(defun remove-duplicate-sublists (list)
  (remove-duplicates list :test #'equal))

(defun list< (a b)
  ;; Returns true when the first element of list <a> is less than the
  ;; first element of list <b>
  (cond ((null a) (not (null b)))
        ((null b) nil)
        ((= (first a) (first b)) nil)
        (t (< (first a) (first b))) ))

(defun scale-value (value orig-min orig-max dest-min dest-max)
   (+ (/ (* (- value orig-min)
            (- dest-max dest-min))
         orig-max)
      dest-min))

(defun binary-list (n &optional acc)
  ;; http://stackoverflow.com/questions/22668217/decimal-to-binary-in-lisp-make-a-non-nested-list
  (cond ((zerop n) (or acc (list 0)))
        ((plusp n)
         (binary-list (ash n -1) (cons (logand 1 n) acc)))
        (t (error "~S: non-negative argument required, got ~s" 'binary-list n))))

(defun rotate (lst n)
  ;; https://github.com/bbatsov/cl-99-problems/blob/master/p119.lisp
  (let ((n-int (if (plusp n) n (- (length lst) (abs n)))))
    (labels ((rotate* (lst index result)
               (cond
                 ((null lst) result)
                 ((< index n-int) (rotate* (rest lst) (1+ index) (cons (first lst) result)))
                 (t (append lst result)))))
      (rotate* lst 0 nil))))

;;; -----------------
;;; MUSICAL UTILITIES
;;; -----------------

(defun p->i (pitch-list)
  ;; Converts a list of pitches into a list of sequential intervals.
  (loop :for (p q) :on pitch-list :while q :collect (- q p)))

(defun freq-to-midi (freq)
  ;; Converts a pitch with frequency <freq> in Hz to midi cents.
  (round (* 100 (+ 69 (* 12 (log (/ freq 440) 2))))))

(defun midi-to-freq (note)
  ;; Converts a note in midi cents to frequency in Hz.
  (* 440 (expt 2 (/ (- (/ note 100) 69) 12))))

(defun durations-to-offsets (duration-list)
  ;; Given a list of durations, returns a list of the corresponding offsets in ms.
  (loop :for d :in duration-list
     :sum d into total
     :collect total into results
     :finally (return (butlast (push 0 results)))))

(defun offsets-to-durations (offset-list)
  ;; Given a list of offsets, returns a list of the corresponding durations in ms.
  (loop :for (o1 o2) :on offset-list :while o2
     :collect (- o2 o1)))

(defun harmonic-series (fundamental n-partials)
  ;; Returns <n-partials> of the harmonic series starting on <fundamental> note in midi cents.
  (let ((fundamental-freq (midi-to-freq fundamental)))
    (loop :for p :from 1 :upto n-partials
       :collect (freq-to-midi (* fundamental-freq p)))))

;;; ---------------
;;; TRANSFORMATIONS
;;; ---------------

(defun one-rotation (chord &optional (interval 1200))
  ;; Repeatedly transposes the lowest note of <chord>
  ;; up by an <interval> until it's the highest.
  (let ((lowest (apply #'min chord))
	(highest (apply #'max chord)))
    (loop :for a := lowest :then (+ a interval)
       :maximizing a :until (> a  highest)
	 :finally (return (subst a lowest chord)))))

(defun chord-rotations (chord &optional (interval 1200))
  ;; Returns a list of all chord rotations
  (let ((sorted-chord (sort (copy-seq chord) #'<)))
    (labels ((upa (a b)
             (cond ((> a b) a)
                   (t (upa (+ a interval) b)))))
    (append (list sorted-chord)
            (loop :for note :in (butlast sorted-chord)
	       :for achord := (subst (upa note (apply #'max sorted-chord))
				     note sorted-chord) 
	       :then (subst (upa note (apply #'max achord))
			    note achord) 
                :collect achord)))))

(defun many-rotations (chord interval iterations multiplier)
  ;; Rotates the chord; in each rotation the note that goes to the top of the chord
  ;; is transposed by a increasing amout, multiplied by a <multiplier> interval.
  (loop :for i :from 0 :upto iterations
     :for a := chord :then (one-rotation a (+ interval (* i multiplier 100)))
     :collect a))
     
;(defun rotation-matrix (chord-list)
;  (setq sorted-list (loop :for a :in chord-list :collect (sort (copy-seq a) #'<)))
;  (loop :for h :from 0 :upto (length sorted-list)
;	:for pivot :in (first sorted-list)
;        :append 
;        (loop :for i :in sorted-list
;              :for tquot = (- (nth h i) pivot)
					;              :collect (mapcar (lambda (x) (- x tquot)) i))))

(defun expand-chord-up (chord &optional (multiplier 1))
  ;; Increases the interval between notes, consecutively from bottom to top,
  ;; a semitone between the first and second notes, two semitones between the
  ;; second and third, etc. For intervals other than semitones use <multipler>.
  (let ((sorted-chord (sort (copy-seq chord) #'<)))
    (mapcar
     (lambda (x) (+ x (* (position x sorted-chord) 100 multiplier)))
     sorted-chord)))

(defun expand-chord-down (chord &optional (multiplier 1))
  ;; Same as above, but from top to bottom.
  (let ((sorted-chord (sort (copy-seq chord) #'>)))
    (mapcar
     (lambda (x) (- x (* (position x sorted-chord) 100 multiplier)))
     sorted-chord)))

(defun expand-chord-pivot (chord &optional (multiplier 1))
  ;; Same as above, expand both up and down around a pivot middle note.
  (let* ((sorted-chord (sort (copy-seq chord) #'<))
	 (average-elt (elt sorted-chord (- (round (/ (length sorted-chord) 2)) 1))))
    (loop :for n :in sorted-chord
       :if (> n average-elt) :collect n :into over
       :if (< n average-elt) :collect n :into under
       :if (= n average-elt) :collect n :into over :and :collect n :into under
       :finally (return (rest (append (expand-chord-up over multiplier)
			      (expand-chord-down under multiplier)))))))
    
(defun many-expansions (chord multiplier direction iterations)
  ;; Performs one of the previous operarions several times, returning a
  ;; sequence of chords.
  (loop :for new-chord := chord
     :then (cond ((eq direction 'down) (expand-chord-down new-chord multiplier))
		 ((eq direction 'up) (expand-chord-up new-chord multiplier))
		 ((eq direction 'pivot) (expand-chord-pivot new-chord multiplier)))
     :repeat iterations :collect new-chord))

;;; -------
;;; VOICING
;;; -------

(defun transpoct (chord bottom top)
  ;; Transposes each note of <chord> to the nearest octave that's between <bottom> and <top>.
  (labels ((up (note bottom)
	     (if (> note bottom)
		 note
		 (up (+ note 1200) bottom)))
	   (down (note top)
	     (if (< note top)
		 note
		 (down (- note 1200) top))))
    (loop :for note :in chord
       :if (< note bottom) :collect (up note bottom)
       :else :if (> note top) :collect (down note top)
       :else :collect note)))

(defun transpoct-seq (seq bottom top)
  ;; Applies the function transpoct to a series of chords.
  (mapcar #'(lambda (x) (transpoct x bottom top)) seq))

(defun top-limit (chord-or-sequence top)
  ;; transposes chords one or more octaves down until all the notes are below <top>
  (labels ((range (chord top)
             (if (< (apply #'max chord) top)
		 chord
		 (range (mapcar (lambda (x) (- x 1200)) chord) top))))
    (if (sublistp chord-or-sequence) 
          (loop :for chord :in chord-or-sequence
                :collect (range chord top))
	  (range chord-or-sequence top))))

(defun bottom-limit (chord-or-sequence bottom)
  ;; transposes chords one or more octaves up until all the notes are above <bottom>
  (labels ((range (chord bottom)
             (if (> (apply #'min chord) bottom)
		 chord
		 (range (mapcar (lambda (x) (+ x 1200)) chord) bottom))))
    (if (sublistp chord-or-sequence) 
          (loop :for chord :in chord-or-sequence
                :collect (range chord bottom))
	  (range chord-or-sequence bottom))))

;;; --------
;;; ANALYSIS
;;; --------

(defun unique-p (l)
  ;; Checks if a list only has unique elements.
  (or (null l)
      (and (not (member (car l) (cdr l)))
	   (unique-p (cdr l)))))

(defun mod12 (chord)
  (mapcar (lambda (x) (mod (/ x 100) 12)) chord))

(defun mod12-unique-p (chord)
  ;; Checks if a list only has unique mod12 elements.
  (unique-p (mod12 chord)))

(defun count-non-uniques (chord-list)
  ;; Counts the number of chords in <chord-list> that have repeated pitch classes.
  (loop :for chord :in chord-list
     :counting (not (mod12-unique-p chord))))

(defun count-unique-chords (chord-list)
  ;; Counts the number of different mod12 chords in <chord-list>.
  (let ((mod12-sorted (loop :for chord :in (mapcar #'mod12 chord-list)
			:collect (copy-seq (sort chord #'<)))))
    (length (remove-duplicate-sublists mod12-sorted))))


;; (defun find-best-expansion (chord direction iterations)
;;   ;; Finds the multipler for function "many-expansions" that generates the least number of chords with repeated pitch classes.
;;   ;; Returns a dotted pair (best multiplier . number of chords with repeated pitch classes)
;;   (let* ((results (loop :for m :from 1 :upto 11
;; 		     :collect (count-non-uniques (many-expansions chord m direction iterations))))
;; 	 (best (+ 1 (position (apply #'min results) results))))
;;     (print results)
;;     (cons best (elt results (- best 1)))))

(defun find-best-expansion (chord direction iterations)
  ;; Finds the multipler for function "many-expansions" that generates the largest number of different chords.
  ;; Returns a dotted pair (best multiplier . number of different chords).
  (let* ((results (loop :for m :from 1 :upto 11
		     :collect (count-unique-chords (many-expansions chord m direction iterations))))
	 (best (+ 1 (position (apply #'max results) results))))
    (print results)
    (cons best (elt results (- best 1)))))

(defun all-intervals (chord)
  ;; Returns a list of all intervals present in <chord>.
  (loop :for achord := chord :then (rest achord)
     :while (> (length achord) 1)
     :append (loop :for n :in achord
		:for a := (first achord)
		:when (not (eql n a)) :collect (- n a))))

(defun count-intervals (chord)
  ;; Returns a list of dotted pairs (interval . its ocurrences in <chord>).
  (let* ((intervals (all-intervals chord))
	 (k (remove-duplicates intervals)))
    (loop :for i :in k
       :collect (cons i (count i intervals)))))

(defun chord-score (chord &optional (score '(0 20 16 8 8 4 20 4 12 12 16 20)))
  ;; Attributes a value to each of the mod 12 intervals present in
  ;; <chord> according to the optional list <score>, in which the first
  ;; element is the value of interval class 0, the second element
  ;; the value of interval class 1, etc. Sums all the values and
  ;; returns a total score for the <chord>.
  (loop :for n :in (count-intervals chord)
     :sum (loop :for s :from 0 :upto 11
	     :when (eql (mod (/ (car n) 100) 12) s)
	     :sum (* (elt score s) (cdr n))))) 

(defun simpsons-index (chord)
  ;; Attributes a score to <chord> according to the diversity of its
  ;; intervals, calculated using the formula for Simpson's index.
  (loop :for n :in (count-intervals chord)
     :summing (* (cdr n) (- (cdr n) 1)) into r
     :counting n :into q
     :finally (return (- 1 (/ r (* q (- q 1)))))))

(defun harmonic-score (chord fundamental)
  ;; Compares <chord> with the harmonic series starting on <fundamental>.
  ;; Returns the degree of coincidence. Higher values mean less coincidence.
  (let ((spectrum (harmonic-series fundamental 40)))
    (loop :for note :in chord
       :sum (loop :for p :in spectrum
	       :minimize (abs (- note p))))))

;;; ---------------
;;; SORT AND SEARCH
;;; ---------------

(defun sort-sequences (list-of-chord-sequences func)
  ;; Sorts <list-of-chord-sequences> acording to the scoring function <func>.
  (loop :for sequence :in list-of-chord-sequences
     :collect (cons (loop :for ch :in sequence
		       :summing (funcall func ch))
		    sequence)
     :into results
     :finally (return (sort (copy-seq results) #'list<))))

(defun find-best-sequence (list-of-chord-sequences)
  (loop :for sequence :in list-of-chord-sequences
     :collect (cons (+
		     (* 2 (count-unique-chords sequence)) 
		     (* 5 (/ 1 (+ (count-non-uniques sequence) 1)))
		     (/ (loop :for ch :in sequence
			   :summing (chord-score ch)) 100))
		    sequence)
     :into results
     :finally (return (sort (copy-seq results) #'list<))))

  
;;; --------------
;;; RHYTHM GRAVITY
;;; --------------

(defun gravity-force (m1 m2 r)
  (let ((g (* 6.67398 0.00000000001)))
    (/ (* g m1 m2) (expt r 2))))

(defun center-of-mass (body-list)
  (loop
     :with total-mass = (loop :for a :in body-list :sum (getf a :mass))
     :for b :in body-list
     :sum (* (getf b :mass) (getf b :pos)) :into s
     :finally (return (* (/ 1 total-mass) s))))

(defun create-body-list (positions masses)
  (loop :for p :in positions
     :for m :in masses
     :collect (list ':pos p ':mass m ':vel 0 ':f 0)))

(defun gravity (body-list)
  (loop
     :for body :in body-list :collect
     (loop :for other :in body-list
	:when (not (eq body other))
	:sum (* (gravity-force
		 (getf body :mass)
		 (getf other :mass)
		 (- (getf other :pos) (getf body :pos)))
		(if (< (getf body :pos) (getf other :pos)) 1 -1)))))
	
(defun update (body-list dt scale)
  (let ((f-list (gravity body-list)))
    (loop :for f :in f-list ;Update gravity-force
       :for body :in body-list
       :do (setf (getf body :f) f))
    (loop :for body :in body-list ;Calculate new positions and velocites
       :for x := (getf body :pos)
       :for v := (getf body :vel)
       :for new-pos := (+ x (* v dt))
       :for f := (getf body :f)
       :for m := (getf body :mass)
       :for new-vel := (+ v (* (/ f m) dt))
       :do (progn
	     (setf (getf body :pos) new-pos)
	     (setf (getf body :vel) new-vel))
       :finally (return (collision-control body-list scale)))))

(defun collision-control (body-list scale)
  (loop :with scale-factor = (/ (* scale 3) 100)
     :for (a b) :on body-list :while b
     :do
     (when (eql (round (/ (getf a :pos) scale-factor))
		(round (/ (getf b :pos) scale-factor)))
       (progn
	 (setf (getf a :pos) -1)
	 (setf (getf b :vel) (/
			      (+
			       (* (getf a :mass) (getf a :vel))
			       (* (getf b :mass) (getf b :vel)))
			      (+ (getf a :mass) (getf b :mass))))))
     :finally (return (remove-if (lambda (x) (eql (getf x :pos) -1))
				 body-list))))
       
(defun get-offsets (body-list)
  (loop :for body :in body-list
     :collect (getf body :pos)))

(defun rhythm-gravity (positions masses time &optional (step 20))
  "Calculates a list of offsets after <time>"
  (let ((scale (- (apply #'max positions) (apply #'min positions))))
    (loop :for i :upto time
       :for bl = (create-body-list positions masses) :then (update bl step scale)
       :finally (return (get-offsets bl)))))
  
(defun rg (positions masses time &optional (step 20))
  "Outputs a visualisation of the gravity function."
  (loop :with scale = (- (apply #'max positions) (apply #'min positions))
     :for i :upto time
     :for bl = (create-body-list positions masses) :then (update bl step scale)
     :while (> (list-length bl) 1)
     :do (let ((line (make-string (+ 1 (apply #'max positions))
				  :initial-element #\_)))
	   (loop :for b :in bl
	      :for position = (round (getf b :pos))
	      :for c = (cond
			 ((< (getf b :mass) 300000) ".")
			 ((and (>= (getf b :mass) 300000)
			       (< (getf b :mass) 600000)) "o")
			 ((>= (getf b :mass)) 600000 "O"))
	      :do (replace line c :start1 position :end1 (+ 1 position))
	      :finally (format t "~a ~a~%" line i)))))

(defun make-body-list (magnitudes)
  (mapcar (lambda (x) (* x 100000)) magnitudes))

(defun time-series (positions masses iterations step)
  (let ((scale (- (apply #'max positions) (apply #'min positions))))
    (loop :for i :upto iterations
       :for bl = (create-body-list positions masses) :then (update bl step scale)
       :collect (get-offsets bl))))


;;; ---------
;;; NECKLACES
;;; ---------

(defun all-necklaces (limit &optional (fun nil fun-supplied-p))
  "Because a necklace can be expressed as a series of zeros (rests) 
and ones (onsets), converts all numbers from 1 up to <limit> into base-2, 
and returns the corresponding binary lists. The results can be filtered
to only include the necklaces for which the function <fun> returns T."
  (loop :for i :from 1 :upto limit
     :for b := (binary-list i)
     :when (or (not fun-supplied-p) (funcall fun b)) ;(rhythmic-oddity-p (binary->interonset b))
     :collect b))

(defun binary->interonset (l)
  "Accepts a list <l> of binary digits and returns a list
of inter-onset intervals. For example (1 1 0 0 0 1) -> (1 4 1)."
  (when (member 1 l)
    (let ((normal-l
	   (loop :for ll := l :then (rotate ll 1)
	      :while (zerop (first ll))
	      :finally (return ll))))
      (loop :for o :in normal-l
	 :for c := 0 :then (incf c)
	 :when (and (plusp o) (plusp c)) :collect c :into r :and :do (setf c 0)
	 :finally (return (append r (list (+ c 1))))))))

;;; ---------------
;;; RHYTHMIC ODDITY
;;; ---------------

(defun rhythmic-oddity-p (input &key (binary-list nil))
  "Checks if <input> has the 'rhythmic oddity' property.
Accepts a list of inter-onset intervals by default. If <input> is
a binary list then the function must be called with :binary-list t"
  (let ((word (if binary-list
		  (binary->interonset input)
		  input)))
    (when (and (listp word) (evenp (reduce #'+ word)))
      (let ((all-cycles (loop :for u :in word
			   :for rotated-word := word
			   :then (append (rest rotated-word)
					 (list (first rotated-word)))
			   :collect rotated-word)))
	(loop :for w :in all-cycles
	   :unless
	   (loop :for i :from 1 :upto (- (length w) 1)
	      :for hu := (reduce #'+ (subseq w 0 i))
	      :for hv := (reduce #'+ (subseq w i))
	      :when (= hu hv)
	      :return (not :it)
	      :finally (return t))
	   :return nil
	   :finally (return t))))))

;;; --------
;;; EVENNESS
;;; --------

(defun evenness (ioi)
  (let* ((n (reduce #'+ ioi))
	 (k (length ioi))
	 (h (/ n k))
	 (m (apply #'max ioi)))
    (/ 1 (1+ (- m h)))))
		    

;; (defun hop-and-jump (onsets pulses &optional hop)
;;   (when (evenp pulses)
;;     (let* ((necklace (make-array pulses :element-type 'bit))
;; 	  (available (copy-seq necklace)))
;;       (print necklace)
;;       (print available))))

(defun do-it (v &key (min-length 8))
  (let* ((a (all-necklaces v))
	 (results (loop :for i :in a
		     :for s := (binary->interonset i)
		     :for p := 1 :then (incf p)
		     :do (when (= (mod p 1000) 0) (format t "-"))
		     :when (and
			    (> (length s) min-length)
			    (< (count 1 s) 3)
			    (rhythmic-oddity-p s)
			    (> (evenness s) 1/3))
		     :collect (mapcar #'(lambda (x) (* 100 x)) s)))
	   (chords (mapcar #'(lambda (l) (necklace-chord 4800 l)) results)))
    (loop :for c :in chords
       :for p := 1 :then (incf p)
       :do (when (= (mod p 100) 0) (format t "="))
       :when ;(and
	      (mod12-unique-p c)
	     ; (> (length c) min-length))
       :collect c)))

(defun necklace-chord (root inter-onsets)
  (loop :for n :in inter-onsets
     :for r := (+ root n) :then (+ r n)
     :collect r :into results
     :finally (return (push root results))))

;(all-necklaces 65536 #'(and (alexandria:rcurry #'rhythmic-oddity-p :binary-list t) t))

;;; -----------------
;;; TRICHORD ANALISYS
;;; -----------------

;(defun prime-form (l)
  

(defun first-trichord (l)
  (loop :for a :in l
     :for c := 1 :then (if (not (member a r)) (incf c) c)
     :collect a :into r
     :until (= c 3)
     :finally (return r)))

