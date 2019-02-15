;;;; package.lisp

(defpackage #:trocadolib
  (:use #:cl)
  (:export

   :flatten
   :sublistp
   :remove-duplicate-sublists
   :list<
   :list>
   :maptree
   :round-to
   :scale-value
   :binary-list
   :rotate
   :exp-rand
   :rrand
   :exp-rrand
   :random-no-repeats
   :fraction-gcd

   ;; PWGL or OpenMusic
   :midi-cents

   ;; Musical Utilities
   :p->i
   :i->p
   :freq-to-midi
   :midi-to-freq
   :durations-to-offsets
   :offsets-to-durations
   :midi->string
   :notename->midicents
   :midi-seq->string
   :harmonic-series

   ;; Pitch Spelling
   :spell-ok

   ;; Lilypond Output
   :midi->ly-note
   :grand-staff-split
   :ly-harmony

   ;; Transformations
   :one-rotation
   :all-rotations
   :rotations-esmae
   :many-rotations
   :many-many-rotations
   :rotation-matrix
   :expand-chord-up
   :expand-chord-down
   :expand-chord-pivot
   :many-expansions

   ;; Voicing
   :transpoct
   :transpoct-seq
   :top-limit
   :bottom-limit
   :closed-position

   ;; Analysis
   :unique-p
   :mod12
   :mod12-unique-p
   :count-chords-with-repeated-pcs
   :count-unqiue-chords
   :find-best-expansion
   :all-intervals
   :count-intervals
   :duplicate-pcs
   :duplicate-pcs-relative
   :interval-score
   :simpsons-index
   :harmonic-coincidence

   ;; Sort and Search
   :sort-chords
   :sort-sequences

   ;; Rhythm Gravity
   ;; TODO

   ;; Necklaces
   :all-necklaces
   :count-necklaces
   :binary->interonset
   :interonset->binary
   :lyndon-words
   :lyndon-words-with-duration
   :necklace-chord

   ;; Geometry of Rhythm
   :rhythmic-oddity-p
   :count-necklaces-with-rhythmic-oddity
   :evenness
   :evenness-weight
   :evenness-weight-index

   ;; Specific search
   :necklace-specific-search
   :necklace-specific-search-with-lyndon

   ;; Spectral analysis
   :make-matrix
   :find-common
   :inharmonicity
   :spectrum))
