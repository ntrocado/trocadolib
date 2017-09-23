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
  "Returns T if <input> is a list of lists."
  (when (and (listp input) (loop for i in input thereis (listp i))) t))

(defun remove-duplicate-sublists (list)
  (remove-duplicates list :test #'equal))

(defun list< (a b)
  "Returns true when the first element of list <a> is lower than the
  first element of list <b>"
  (cond ((null a) (not (null b)))
        ((null b) nil)
        ((= (first a) (first b)) nil)
        (t (< (first a) (first b))) ))

(defun list> (a b)
  "Returns true when the first element of list <a> is higher than the
  first element of list <b>"
  (cond ((null a) (not (null b)))
        ((null b) nil)
        ((= (first a) (first b)) nil)
        (t (> (first a) (first b)))))


(defun scale-value (value orig-min orig-max dest-min dest-max)
  "Scales <value> from an original to a destination range. If <value>, <orig-min> and <orig-max> are all the same, returns the lowest value of the destination bracket."
  (when (and (>= value orig-min)
	     (<= value orig-max))
    (if (= value orig-min orig-max)
	dest-min
	(+ (/ (* (- value orig-min)
		 (- dest-max dest-min))
	      (- orig-max orig-min))
	   dest-min))))

(defun binary-list (n &optional acc)
  "Accepts a non-negative integer, returns its binary representation in list form."
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

(defun random-no-repeats (bottom top size no-repeat-size)
  "Returns a list of random numbers between <bottom> and <top>, with the length <size>.
Numbers will be unique in each subsequence of length <no-repeat-size>."
  (when (> size no-repeat-size)
    (loop :repeat size
       :with new-last-x
       :for last-x := nil :then new-last-x
       :collect (loop :for nn := (+ (random (- top (- bottom 1))) bottom)
		   :do (setf new-last-x (if (< (length last-x) no-repeat-size)
					    (cons nn last-x)
					    (butlast (cons nn last-x))))
		   :until (unique-p new-last-x)
		   :finally (return nn)))))

;;; -----------------
;;; PWGL or OpenMusic
;;; -----------------

(defun midi-cents (n)
  "Converts a midi note number to the midicents format used by OpenMusic if this is being
run in that environment."
  (if (find :om *features*)
      (* n 100)
      n))

;;; -----------------
;;; MUSICAL UTILITIES
;;; -----------------

(defun p->i (pitch-list)
  "Converts a list of pitches into a list of sequential intervals."
  (loop :for (p q) :on pitch-list :while q :collect (- q p)))

(defun i->p (interval-list start)
  "Converts a list of intervals into a list of pitches."
  (loop :for i :in (push start interval-list) :sum i :into z :collect z))

(defun freq-to-midi (freq)
  "Converts a pitch with frequency <freq> in Hz to midi cents."
  (* (midi-cents 1) (+ 69 (* 12 (log (/ freq 440) 2)))))

(defun midi-to-freq (note)
  "Converts a note in midi cents to frequency in Hz."
  (* 440 (expt 2 (/ (- (/ note (midi-cents 1)) 69) 12))))

(defun durations-to-offsets (duration-list)
  "Given a list of durations, returns a list of the corresponding offsets in ms."
  (loop :for d :in duration-list
     :sum d into total
     :collect total into results
     :finally (return (butlast (push 0 results)))))

(defun offsets-to-durations (offset-list)
  "Given a list of offsets, returns a list of the corresponding durations in ms."
  (loop :for (o1 o2) :on offset-list :while o2
     :collect (- o2 o1)))

(defun harmonic-series (fundamental n-partials)
  "Returns <n-partials> of the harmonic series starting on <fundamental> note in midi cents."
  (let ((fundamental-freq (midi-to-freq fundamental)))
    (loop :for p :from 1 :upto n-partials
       :collect (freq-to-midi (* fundamental-freq p)))))

;;; ---------------
;;; TRANSFORMATIONS
;;; ---------------

(defun one-rotation (chord &optional (interval (midi-cents 12)))
  "Repeatedly transposes the lowest note of <chord>
up by an <interval> until it's the highest."
  (let ((lowest (apply #'min chord))
	(highest (apply #'max chord)))
    (loop :for a := lowest :then (+ a interval)
       :maximizing a :until (> a  highest)
       :finally (return (subst a lowest chord)))))

(defun all-rotations (chord &optional (interval (midi-cents 12)))
  "Returns a list of all <chord> rotations."
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
  "Rotates the <chord> using <interval> (see above);
in each one of the <iterations> the note that goes to the top of the chord 
is transposed by a increasing amount, multiplied by a <multiplier>."
  (loop :for i :from 0 :below iterations
     :for a := chord :then
     (let ((rotated-chord (sort (copy-seq (one-rotation a interval)) #'<)))
       (append (list (+ (car (last rotated-chord))
			(* i (midi-cents multiplier))))
	       (butlast rotated-chord)))
     :collect a))

(defun many-many-rotations (chord interval iterations
			    multiplier-min multiplier-max multiplier-step
			    &key (rounded t))
  "Returns a list of lists with all the 'many-rotations' for a range of multipliers,
between <multiplier-min> and <multiplier-max>, progressing by <multiplier-step>.
The :rounded keyword returns results in 12TET when set to T, and microtonal when
set to NIL."
  (loop :for m :from multiplier-min :upto multiplier-max :by multiplier-step 
     :collect (if rounded
		  (mapcar (lambda (x) (mapcar #'round x))
			  (many-rotations chord interval iterations m))
		  (many-rotations chord interval iterations m))))

(defun rotation-matrix (chord &key (interval (midi-cents 12)) (rem-dups t))
  "Returns a matrix of rotations, where for each note in <chord> there's a set of
rotations, transposed to keep that pivot note constant. The :rem-dups keyword
removes ocurring duplicate chords."
  (let* ((rotations (all-rotations chord interval))
	 (sorted-list (mapcar (lambda (x) (sort (copy-seq x) #'<)) rotations))
	 (r (loop :for h :from 0 :upto (length rotations)
	       :for pivot :in (first sorted-list)
	       :append 
	       (loop :for i :in sorted-list
		  :for tquot := (- (nth h i) pivot)
		  :collect (mapcar (lambda (x) (- x tquot)) i)))))
    (if rem-dups
	(remove-duplicates r :test #'equal :from-end t)
	r)))

(defun expand-chord-up (chord &optional (multiplier 1))
  "Increases the interval between notes, consecutively from bottom to top,
a semitone between the first and second notes, two semitones between the
 second and third, etc. For intervals other than semitones use <multipler>."
  (let ((sorted-chord (sort (copy-seq chord) #'<)))
    (mapcar
     (lambda (x) (+ x (* (position x sorted-chord) (midi-cents 1) multiplier)))
     sorted-chord)))

(defun expand-chord-down (chord &optional (multiplier 1))
  "Same as above, but from top to bottom."
  (let ((sorted-chord (sort (copy-seq chord) #'>)))
    (mapcar
     (lambda (x) (- x (* (position x sorted-chord) (midi-cents 1) multiplier)))
     sorted-chord)))

(defun expand-chord-pivot (chord &optional (multiplier 1))
  "Same as above, expand both up and down around a pivot middle note."
  (let* ((sorted-chord (sort (copy-seq chord) #'<))
	 (average-elt (elt sorted-chord (- (round (/ (length sorted-chord) 2)) 1))))
    (loop :for n :in sorted-chord
       :if (> n average-elt) :collect n :into over
       :if (< n average-elt) :collect n :into under
       :if (= n average-elt) :collect n :into over :and :collect n :into under
       :finally (return (rest (append (expand-chord-up over multiplier)
				      (expand-chord-down under multiplier)))))))
    
(defun many-expansions (chord multiplier direction iterations)
  "Performs one of the previous operarions several times, returning a sequence of chords."
  (loop :for new-chord := chord
     :then (cond ((eq direction 'down) (expand-chord-down new-chord multiplier))
		 ((eq direction 'up) (expand-chord-up new-chord multiplier))
		 ((eq direction 'pivot) (expand-chord-pivot new-chord multiplier)))
     :repeat iterations :collect new-chord))

;;; -------
;;; VOICING
;;; -------

(defun transpoct (chord bottom top)
  "Transposes each note of <chord> to the nearest octave between <bottom> and <top>."
  (labels ((up (note bottom)
	     (if (> note bottom)
		 note
		 (up (+ note (midi-cents 12)) bottom)))
	   (down (note top)
	     (if (< note top)
		 note
		 (down (- note (midi-cents 12)) top))))
    (loop :for note :in chord
       :if (< note bottom) :collect (up note bottom)
       :else :if (> note top) :collect (down note top)
       :else :collect note)))

(defun transpoct-seq (seq bottom top)
  "Applies the function transpoct to a series of chords."
  (mapcar #'(lambda (x) (transpoct x bottom top)) seq))

(defun top-limit (chord-or-sequence top)
  "Transposes chords one or more octaves down until all the notes are below <top>."
  (labels ((range (chord top)
             (if (< (apply #'max chord) top)
		 chord
		 (range (mapcar (lambda (x) (- x (midi-cents 12))) chord) top))))
    (if (sublistp chord-or-sequence) 
	(loop :for chord :in chord-or-sequence
	   :collect (range chord top))
	(range chord-or-sequence top))))

(defun bottom-limit (chord-or-sequence bottom)
  "Transposes chords one or more octaves up until all the notes are above <bottom>."
  (labels ((range (chord bottom)
             (if (> (apply #'min chord) bottom)
		 chord
		 (range (mapcar (lambda (x) (+ x (midi-cents 12))) chord) bottom))))
    (if (sublistp chord-or-sequence) 
	(loop :for chord :in chord-or-sequence
	   :collect (range chord bottom))
	(range chord-or-sequence bottom))))

(defun closed-voicing (chord)
  "Puts <chord> in closed voicing."
  (let* ((rotations (all-rotations chord))
	 (range (loop :for ch :in rotations
		   :collect (cons (- (apply #'max ch) (apply #'min ch))
				  ch))))
    (cdr (first (sort (copy-seq range) #'list<))))) 

;;; --------
;;; ANALYSIS
;;; --------

(defun unique-p (l)
  "Checks if a list only has unique elements."
  (or (null l)
      (and (not (member (car l) (cdr l)))
	   (unique-p (cdr l)))))

(defun mod12 (chord)
  (mapcar (lambda (x) (mod (/ x (midi-cents 1)) 12)) chord))

(defun mod12-unique-p (chord)
  "Checks if a list only has unique mod12 elements."
  (unique-p (mod12 chord)))

(defun count-chords-with-repeated-pcs (chord-list)
  "Counts the number of chords in <chord-list> that have repeated pitch classes."
  (loop :for chord :in chord-list
	:counting (not (mod12-unique-p chord))))

(defun count-unique-chords (chord-list)
  "Counts the number of different mod12 chords in <chord-list>."
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

(defun find-best-expansion (chord direction iterations &optional (decimals 0))
  "Finds the multipler for function <many-expansions> that generates the largest number of different mod12 chords. Returns a dotted pair (best multiplier . number of different chords)."
  (let* ((results (loop :for m :from 1 :upto 11 :by (/ 1
						       (expt 10 decimals))
		     :collect (count-unique-chords (mapcar (lambda (x) (mapcar #'round x))
							   (many-expansions chord
									    m
									    direction
									    iterations)))))
	 (best (+ 1 (position (apply #'max results) results)))) 
    (values best (elt results (- best 1)) results)))

(defun all-intervals (chord)
  "Returns a list of all intervals present in <chord>."
  (loop :for achord := chord :then (rest achord)
     :while (> (length achord) 1)
     :append (loop :for n :in achord
		:for a := (first achord)
		:when (not (eql n a)) :collect (- n a))))

(defun count-intervals (chord)
  "Returns a list of dotted pairs (interval . its ocurrences in <chord>)."
  (let* ((intervals (all-intervals chord))
	 (k (remove-duplicates intervals)))
    (loop :for i :in k
	  :collect (cons i (count i intervals)))))

(defun duplicate-pcs (chord)
  "Returns the number of duplicate pitch classes."
  (loop :with mod12-chord := (mod12 chord)
	:for n :in mod12-chord
	:count (> (count n mod12-chord) 1)))

(defun duplicate-pcs-relative (chord)
  "Returns the number of duplicate pitch classes, in proportion to the number of notes in <chord>."
  (/ (duplicate-pcs chord)
     (length chord)))

(defun interval-score (chord &optional (score '(0 20 16 8 8 4 20 4 12 12 16 20)))
  "Attributes a value to each of the mod 12 intervals present in <chord> according to the 
optional list <score>, in which the first element is the value of interval class 0, the second 
element the value of interval class 1, etc. Sums all the values and returns a total score for the <chord>."
  (let ((sorted-chord (copy-seq (sort chord #'<))))
    (loop :for n :in (count-intervals sorted-chord)
	  :sum (loop :for s :from 0 :upto 11
		     :when (eql (mod (/ (car n) (midi-cents 1)) 12) s)
		       :sum (* (elt score s) (cdr n))))))

(defun simpsons-index (chord)
  "Attributes a score to the ordered set <chord> according to the diversity of its intervals, calculated using the formula for Simpson's index of diversity. There must be at least two different intervals."
  (let ((intervals (count-intervals chord)))
    (when (> (length intervals) 1)
      (loop :for n :in (count-intervals chord)
	 :summing (* (cdr n) (- (cdr n) 1)) into r
	 :summing (cdr n) :into q
	 :finally (return (- 1 (/ r (* q (- q 1)))))))))

(defun harmonic-coincidence (chord fundamental &key (inverse nil) (compare-spectra nil))
  "Compares <chord> with the harmonic series starting on <fundamental>.
Returns the degree of coincidence. Higher values mean more coincidence."
  (let* ((spectrum (harmonic-series fundamental 100))
	 (new-chord (if compare-spectra
			(flatten (mapcar (lambda (x) (harmonic-series x 14)) chord))
			chord))
	 (degree (/ 1 (1+ (/ (loop :for note :in new-chord
				:sum (loop :for p :in spectrum
					:minimize (abs (- note p))))
			     (length new-chord))))))
    (if inverse
	(- 1 degree)
	degree)))

;;; ---------------
;;; SORT AND SEARCH
;;; ---------------

(defun sort-chords (list-of-chords functions
		    &key (weights (make-list (length functions) :initial-element 1)))
  "Scores <list-of-chords> according to <functions> and <weights>, returning a sorted tree of
relative values in the form ((<score1> (<chord1>)) (<score2> (<chord2>)) ... (<scoren> (<chordn>))), where <score1> is the highest."
  (labels ((scaled-score (s) (mapcar (lambda (x) (scale-value x
							      (apply #'min s)
							      (apply #'max s)
							      0
							      1))
				     s)))
    (let* ((weights-sum (reduce #'+ weights))
	   (relative-weights (mapcar (lambda (x) (/ x weights-sum)) weights))
	   (scores (loop :for f :in functions
		      :for w :in relative-weights
		      :collect (mapcar (lambda (x) (* x w))
				       (scaled-score (mapcar f list-of-chords)))
		      :into r
		      :do (print r)
		      :finally (return (loop :for i :from 0 :below (length (first r))
					  :collect (reduce #'+ (mapcar (lambda (x) (nth i x))
								       r)))))))
      (sort (copy-seq (mapcar #'list scores list-of-chords)) #'list>))))

(defun sort-sequences (list-of-chord-sequences functions
		       &key (weights (make-list (length functions) :initial-element 1)))
  "Scores <list-of-chord-sequences> according to <functions> and <weights>, returning a sorted tree
in the form ((<score1> ((chord 1a) (chord 1b) ... (chord 1n)))
             (<score2> ((chord 2a) (chord 2b) ... (chord 2n)))
             ... 
             (<scoren> ((chord na) (chord nb) ... (chord nn)))), where <score1> is the highest."
  (sort (copy-seq (mapcar (lambda (chord-sequence)
			    (cons
			     (reduce #'+ (mapcar #'car
						 (sort-chords chord-sequence
							      functions
							      :weights weights)))
			     chord-sequence))
			  list-of-chord-sequences))
	#'list>))

  ;; (defun sort-sequences (list-of-chord-sequences func)
;;   ;; Sorts <list-of-chord-sequences> acording to the scoring function <func>.
;;   (loop :for sequence :in list-of-chord-sequences
;;      :collect (cons (loop :for ch :in sequence
;; 		       :summing (funcall func ch))
;; 		    sequence)
;;      :into results
;;      :finally (return (sort (copy-seq results) #'list<))))

;; (defun sort-sequences (list-of-chord-sequences &rest weight-functions)
;;   ;; Sorts <list-of-chord-sequences> acording to the scoring function <func>.
;;   (loop :for sequence :in list-of-chord-sequences
;;      :collect (cons (loop :for ch :in sequence))
;;      :summing (funcall func ch)
;;      sequence
;;      :into results
;;      :finally (return (sort (copy-seq results) #'list<))))

;; (defun find-best-sequence (list-of-chord-sequences)
;;   (loop :for sequence :in list-of-chord-sequences
;;      :collect (cons (+
;; 		     (* 2 (count-unique-chords sequence)) 
;; 		     (* 5 (/ 1 (+ (count-non-uniques sequence) 1)))
;; 		     (/ (loop :for ch :in sequence
;; 			   :summing (chord-score ch))
;; 			100))
;; 		    sequence)
;;      :into results
;;      :finally (return (sort (copy-seq results) #'list<))))


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
       :do (progn)
       (setf (getf body :pos) new-pos)
       (setf (getf body :vel) new-vel)
       :finally (return (collision-control body-list scale)))))

(defun collision-control (body-list scale) 
  (loop :with scale-factor = (/ (* scale 3) 100)
     :for (a b) :on body-list :while b
     :do
     (when (or (eql (round (/ (getf a :pos) scale-factor))
		    (round (/ (getf b :pos) scale-factor)))
	       (>= (getf a :pos) (getf b :pos))) 
       (progn
	 (setf (getf a :pos) -1)
	 (setf (getf b :vel) (/
			      (+
			       (* (getf a :mass) (getf a :vel))
			       (* (getf b :mass) (getf b :vel)))
			      (+ (getf a :mass) (getf b :mass))))))
     :finally (return (remove-if (lambda (x) (eql (getf x :pos) -1)) body-list))))
     
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
  "Returns a list of offsets for a number of <iterations>."
  (let ((scale (- (apply #'max positions) (apply #'min positions))))
    (loop :for i :upto iterations
       :for bl = (update (create-body-list positions masses) step scale)
       :then (update bl step scale)
       :collect (get-offsets bl))))

;;; ---------
;;; NECKLACES
;;; ---------

(defun all-necklaces (limit &rest filters)
  "Because a necklace can be expressed as a series of zeros (rests) 
and ones (onsets), converts all numbers from 1 up to (2^<limit>)-1 into base-2, 
and returns the corresponding binary lists. The results can be filtered
to only include the necklaces for which all the functions <filters> return T.
For example: (all-necklaces 6 #'rhythmic-oddity-p (lambda (x) (< (count '1 (binary->interonset x)) 2))) -> all necklaces with total length up to 6, with the rhythmic oddity property and no more than two consecutive attacks."
  (loop :for i :from 1 :upto (1- (expt 2 limit))
	:for b := (binary-list i)
	:when (or (not filters)
		  (loop :for fun :in filters
			:always (funcall fun b)))
	  :collect b))

(defun count-necklaces (limit &rest filters)
  "Same as all-necklaces, but more efficiently just counts how many solutions there are, without returning them all."
  (loop :for i :from 1 :upto (1- (expt 2 limit))
	:for b := (binary-list i)
	:when (or (not filters)
		  (loop :for fun :in filters
			:always (funcall fun b)))
	  :count b))

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
	      :when (and (plusp o) (plusp c))
		:collect c :into r :and :do (setf c 0)
	      :finally (return (append r (list (+ c 1))))))))

(defun interonset->binary (l)
  "Accepts a list <l> of inter-onset intervals and returns a list
of binary digits. For example (1 4 1) -> (1 1 0 0 0 1)."
  (unless (member-if-not #'plusp l)
    (flatten (mapcar (lambda (x) (if (eq x '1)
				     '1
				     (list '1
					   (make-list (1- x)
						      :initial-element '0))))
		     l))))

(defun lyndon-words (n a M)
  "Generates all Lyndon words of length <= <n> over an alphabet <a>..<M>. The algorithm is an adaptation of the one by Jean-Paul Duval, Génération d'une section des classes de conjugaison et arbre des mots de Lyndon de longueur bornée, in Theoretical Computer Science, 60, 1988, pp. 255-283."
  (let ((w (make-list (1+ n)))
	(i 1))
    (setf (elt w 1) a)
    (loop
      :do
	 (loop :for j :from 1 :to (- n i)
	       :do (setf (elt w (+ i j)) (elt w j)))
      :collect (subseq w 1 (1+ i))
      :do
	 (setf i n)
	 (loop :while (and (> i 0)
			   (eq (elt w i) M))
	       :do (decf i))
	 (when (> i 0)
	   (setf (elt w i) (1+ (elt w i))))
      :until (or (= i 0)))))

(defun lyndon-words-with-duration (n a M duration)
  "Generates all Lyndon words of length <= <n> over an alphabet <a>..<M>, where <a> and <M> are numbers and <a> < <M>, and where the sum of all numbers is <= <duration>."
  (remove-if-not (lambda (x) (<= (reduce #'+ x) duration))
		 (lyndon-words n a M)))

;;; ------------------
;;; GEOMETRY OF RHYTHM
;;; ------------------

(defun rhythmic-oddity-p (input &key (interonset-intervals nil))
  "Checks if <input> has the rhythmic oddity property.
Accepts a list of binary digits by default. If <input> is a list of inter-onset intervals
then the function must be called with :interonset-intervals t."
  (let* ((necklace (if interonset-intervals
		       (interonset->binary input)
		       input)) 
	 (w (length necklace)))
    (when (evenp w)
      (loop :for pulse :in necklace
	    :for i :from 0
	    :never (and (plusp pulse)
			(plusp (elt necklace
				    (mod (+ i (/ w 2))
					 w))))))))

(defun count-necklaces-with-rhythmic-oddity (length)
  (count-necklaces (expt 2 length) :filter #'rhythmic-oddity-p))




;;; --------
;;; EVENNESS
;;; --------

(defun evenness (ioi)
  (let* ((n (reduce #'+ ioi))
	 (k (length ioi))
	 (h (/ n k))
	 (m (apply #'max ioi)))
    (/ 1 (1+ (- m h)))))

;;; ---------------
;;; SPECIFIC SEARCH
;;; ---------------

;; (defun necklace-specific-search (v &key (min-length 8))
;;   "What are all necklaces with lenght higher than <min-length>, less than three attacks with the
;; duration of a single pulse, possessing the rhythmic oddity property and with an evenness degree 
;; higher than 1/3?"
;;   (let* ((a (all-necklaces v))
;; 	 (results (loop :for i :in a
;; 			:for s := (binary->interonset i)
;; 			:for p := 1 :then (incf p)
;; 			:do (when (= (mod p 1000) 0) (format t "-"))
;; 			:when (and (>= (length s) min-length)
;; 				   (< (count 1 s) 3)
;; 				   (rhythmic-oddityc-p i)
;; 				   (> (evenness s) 1/3))
;; 			  :collect (mapcar #'(lambda (x) (* (midi-cents 1) x)) s)))
;; 	 (chords (mapcar #'(lambda (l) (necklace-chord (midi-cents 48) l))
;; 			 results)))
;;     (loop :for c :in chords
;; 	  :for p := 1 :then (incf p)
;; 	  :do (when (zerop (mod p 100)) (format t "="))
;; 	  :when (mod12-unique-p c)
;; 	    :collect c)))

(defun necklace-specific-search (min-length max-length consecutive min-evenness)
  (remove-if-not (and #'rhythmic-oddity-p
		      (lambda (x) (let ((ioi (binary->interonset x)))
				    (and (>= (length ioi) min-length)
					 (< (count 1 ioi) consecutive)
					 (> (evenness x) min-evenness)
					 (mod12-unique-p (i->p ioi 0))))))
		 (lyndon-words max-length 1 4)))



(defun do-it-lyndon (max-length)
  "What are all necklaces with lenght higher than <min-length>, at least three onsets, possessing
the rhythmic oddity property and with an evenness degree higher than 1/3?"
  (let* ((a (lyndon-words max-length 2 5))
	 (results (loop :for i :in a
			:for p := 1 :then (incf p)
			:do (when (= (mod p 1000) 0) (format t "-"))
			:when (and
			       (< (count 1 i) 3)
			       (rhythmic-oddity-p i :interonset-intervals t)
			       (> (evenness i) 1/3))
			  :collect (mapcar #'(lambda (x) (* (midi-cents 1) x)) i))) 
	 (chords (mapcar #'(lambda (l) (necklace-chord (midi-cents 48) l))
			 results)))
    (print (length results))
    (loop :for c :in chords
	  :for p := 1 :then (incf p)
	  :do (when (= (mod p 1000))
		0)
	      (format t "=")
	  :when (mod12-unique-p c)
	    :collect c)))

(remove-if-not (lambda (x) (let ((l (reduce #'+ x)))
			     (and (< 19 l 22)
				  (rhythmic-oddity-p x :interonset-intervals t)
				  (> (evenness x) 1/3))))
	       (lyndon-words 7 2 4))

(defun necklace-chord (root inter-onsets)
  "Builds a pitch collection starting on <root> and following the <inter-onsets> intervals."
  (loop :for n :in inter-onsets
	:for r := (+ root n) :then (+ r n)
	:collect r :into results
	:finally (return (push root results))))

;;(all-necklaces 65536 #'(and (alexandria:rcurry #'rhythmic-oddity-p :binary-list t) t))
;;(do-it (expt 2 20) :min-length 9)
;;(mapcar #'p->i (do-it (expt 2 20) :min-length 9))

;;; -----------------
;;; TRICHORD ANALISYS
;;; -----------------

;;; TODO

;;;(defun prime-form (l)

(defun first-trichord (l)
  (loop :for a :in l
     :for c := 1 :then (if (not (member a r)) (incf c) c)
     :collect a :into r
     :until (= c 3)
     :finally (return r)))
