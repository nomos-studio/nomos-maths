; SPDX-License-Identifier: EPL-2.0
(ns nomos.maths.harmonic
  "Just Intonation pitch space — Tenney harmonic distance, ratio arithmetic,
  and harmonic gravity.

  JI ratios are represented as two-element vectors [m n] (the fraction m/n).
  All ratio functions reduce to lowest terms and accept positive integers only.

  ## The metric
    (tenney-h [3 2])          ; → log₂(6) ≈ 2.58  — perfect fifth
    (tenney-h 5)              ; → log₂(5) ≈ 2.32  — 5th harmonic
    (tenney-h [5 4] [3 2])    ; → H of [5 4] relative to attractor [3 2]

  ## Ratio arithmetic
    (ratio-reduce   [6 4])        ; → [3 2]
    (ratio-normalize [3 1])       ; → [3 2]   — octave-reduce to [1, 2)
    (ratio-inverse  [3 2])        ; → [4 3]   — Utonal reciprocal, normalized
    (ratio-interval [5 4] [3 2])  ; → [5 3]   — interval between two ratios

  ## Pitch conversion
    (ratio->cents    [3 2])       ; → 701.96  — cents above root
    (ratio->voct     [3 2])       ; → 0.585   — V/oct offset from root
    (ratio->midi-offset [5 4])    ; → 3.86    — semitones above root (float)

  ## Gravity
    (gravity-field [9 8])           ; → attraction strength toward [1 1]
    (gravity-weight 5)              ; → selection weight for harmonic 5
    (gravity-accept? 4.32 5.58)     ; → accept a mutation toward higher H?

  The Tenney field is the shared metric for harmonic gravity in deflattice
  (restoring force), defbook (selection probability), and defbook :cell
  (mutation acceptance). Each primitive applies the field via its own
  mechanism; this namespace provides the field itself.")

;; ---------------------------------------------------------------------------
;; Internal constants and helpers
;; ---------------------------------------------------------------------------

(def ^:private LOG2 (Math/log 2.0))

(defn- gcd ^long [^long a ^long b]
  (loop [a (Math/abs a) b (Math/abs b)]
    (if (zero? b) a (recur b (mod a b)))))

;; ---------------------------------------------------------------------------
;; Ratio arithmetic
;; ---------------------------------------------------------------------------

(defn ratio-reduce
  "Reduce ratio [m n] to lowest terms. Both m and n must be positive integers."
  [[m n]]
  (let [g (gcd (long m) (long n))]
    [(/ (long m) g) (/ (long n) g)]))

(defn ratio-normalize
  "Octave-reduce ratio [m n] to the range [1, 2).
  Multiplies or divides the numerator by 2 until 1 ≤ m/n < 2, then reduces."
  [[m n]]
  (loop [m (long m) n (long n)]
    (let [r (/ (double m) (double n))]
      (cond
        (< r 1.0) (recur (* 2 m) n)
        (>= r 2.0) (recur m (* 2 n))
        :else (ratio-reduce [m n])))))

(defn ratio-interval
  "Return the ratio representing the interval from [a b] to [m n].
  Computed as (m/n) ÷ (a/b) = [m×b, n×a], then octave-normalized and reduced."
  [[m n] [a b]]
  (ratio-normalize [(* (long m) (long b))
                    (* (long n) (long a))]))

(defn ratio-inverse
  "Return the Utonal (reciprocal) of ratio [m n], octave-normalized.
  (ratio-inverse [3 2]) → [4 3]"
  [[m n]]
  (ratio-normalize [n m]))

;; ---------------------------------------------------------------------------
;; Tenney harmonic distance
;; ---------------------------------------------------------------------------

(defn tenney-h
  "Tenney harmonic distance.

  (tenney-h [m n])         — log₂(m × n) for ratio [m n] in lowest terms
  (tenney-h n)             — log₂(n) for single harmonic integer N
  (tenney-h pos attractor) — H of pos relative to attractor (movable attractor)

  The attractor defaults to [1 1]. Passing an explicit attractor allows
  measuring distance from any tonal center, not only the mathematical origin.
  This is required for defexcursion's tonal center management: when the form
  sets a non-trivial ground state, gravity pulls toward that, not toward 1/1."
  ([x]
   (if (vector? x)
     (let [[m n] (ratio-reduce x)]
       (/ (Math/log (* (double m) (double n))) LOG2))
     (/ (Math/log (double x)) LOG2)))
  ([position attractor]
   (tenney-h (ratio-interval position attractor))))

;; ---------------------------------------------------------------------------
;; Pitch conversion
;; ---------------------------------------------------------------------------

(defn ratio->cents
  "Convert JI ratio [m n] to cents above the root (1200 × log₂(m/n))."
  ^double [[m n]]
  (* 1200.0 (/ (Math/log (/ (double m) (double n))) LOG2)))

(defn ratio->voct
  "Convert JI ratio [m n] to a V/oct offset from the root (log₂(m/n)).
  Add to the root voltage to produce the absolute CV pitch."
  ^double [[m n]]
  (/ (Math/log (/ (double m) (double n))) LOG2))

(defn ratio->midi-offset
  "Convert JI ratio [m n] to a semitone offset from the root (float).
  12 × log₂(m/n). Not rounded — use Math/round if an integer MIDI offset is needed."
  ^double [ratio]
  (* 12.0 (ratio->voct ratio)))

(defn ratio->freq
  "Convert JI ratio [m n] to an absolute frequency in Hz.
  `root-hz` is the root pitch frequency (default 440.0 for A4)."
  (^double [ratio] (ratio->freq ratio 440.0))
  (^double [[m n] root-hz]
   (* (double root-hz) (/ (double m) (double n)))))

;; ---------------------------------------------------------------------------
;; 12-TET to 5-limit JI approximation
;; ---------------------------------------------------------------------------

(def semitone->ji-ratio
  "5-limit JI ratio for each semitone interval (0–12).
  Used for approximating 12-TET pitch distances in JI space, e.g. when
  analyzing corpus data notated in equal temperament.
  Misses septimal intervals (7/4, 7/6); adequate for Palestrina-style analysis."
  {0  [1  1]    ; unison
   1  [16 15]   ; minor second
   2  [9  8]    ; major second
   3  [6  5]    ; minor third
   4  [5  4]    ; major third
   5  [4  3]    ; perfect fourth
   6  [45 32]   ; tritone
   7  [3  2]    ; perfect fifth
   8  [8  5]    ; minor sixth
   9  [5  3]    ; major sixth
   10 [9  5]    ; minor seventh
   11 [15 8]    ; major seventh
   12 [2  1]})  ; octave

(defn midi->tenney-h
  "Approximate Tenney H for a MIDI semitone interval (0–12) using
  the 5-limit JI mapping. For inter-voice analysis of 12-TET data."
  ^double [semitones]
  (if-let [ratio (get semitone->ji-ratio (Math/abs (int semitones)))]
    (tenney-h ratio)
    (tenney-h [(Math/round (Math/pow 2.0 (/ (Math/abs (double semitones)) 12.0)))
               1])))

;; ---------------------------------------------------------------------------
;; Harmonic gravity field
;; ---------------------------------------------------------------------------

(defn gravity-field
  "Harmonic gravity field: attraction strength of position toward attractor.

  Returns a value in [0, 1]:
    0.0  — position is at the attractor (no pull)
    1.0  — position is at or beyond the horizon (maximum pull)

  Options:
    :attractor — target ratio, default [1 1]
    :horizon   — Tenney H at which field strength reaches 1.0, default 8.0

  Consumers interpret the strength according to their own mechanism:
  deflattice uses it as a bias on step direction; defbook uses it to
  weight selection probability; defbook :cell uses it in the acceptance test."
  [position & {:keys [attractor horizon] :or {attractor [1 1] horizon 8.0}}]
  (let [h (tenney-h position attractor)]
    (min 1.0 (/ h (double horizon)))))

(defn gravity-weight
  "Selection weight for a ratio or harmonic integer under Tenney gravity.

  Returns a weight in (0, 1] inversely proportional to Tenney distance from
  the attractor. Used by defbook to bias next-note selection toward harmonically
  simpler positions.

  `strength` ∈ [0, ∞): 0.0 = flat (no bias), higher = sharper preference.
  1.0 gives linear falloff; 2.0 gives quadratic."
  [position & {:keys [attractor strength horizon]
               :or   {attractor [1 1] strength 1.0 horizon 8.0}}]
  (Math/pow (- 1.0 (gravity-field position
                                   :attractor attractor
                                   :horizon horizon))
            (double strength)))

(defn gravity-force
  "Restoring-force magnitude for a continuous position in Tenney space.

  Returns a pull fraction in [0, 1]: how strongly to nudge toward the attractor
  in this time step. Used by deflattice to bias the probability of taking a
  lattice step toward the attractor.

  `dt`  — elapsed time in beats or steps
  `tau` — gravity time constant; larger = weaker/slower restoring force"
  [position & {:keys [attractor tau dt horizon]
               :or   {attractor [1 1] tau 8.0 dt 1.0 horizon 8.0}}]
  (let [strength (gravity-field position :attractor attractor :horizon horizon)]
    (* strength (/ (double dt) (double tau)))))

(defn gravity-accept?
  "Metropolis-Hastings acceptance criterion for a harmonic shift.

  If new-h < current-h (moving toward attractor): always accept.
  If new-h ≥ current-h (moving away): accept with probability
    exp(-(new-h − current-h) / temp)

  `temp` — annealing temperature.
    High (e.g. 2.0): accept most mutations (exploration).
    Low  (e.g. 0.1): accept only downhill mutations (convergence).
  Decrease temp over the session arc as [:session :harmonic-distance]
  accumulates to crystallize the cell material toward resolution."
  [current-h new-h & {:keys [temp] :or {temp 1.0}}]
  (if (< (double new-h) (double current-h))
    true
    (< (rand) (Math/exp (/ (- (double current-h) (double new-h))
                           (double temp))))))
