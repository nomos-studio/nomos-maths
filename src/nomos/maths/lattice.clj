; SPDX-License-Identifier: EPL-2.0
(ns nomos.maths.lattice
  "The harmonic lattice — JI pitch space as a 2D integer grid.

  A lattice point is a pair [m n] where m/n is the pitch interval above
  the fundamental. m is the overtone index, n is the undertone index.
  Both are positive integers in lowest terms (gcd = 1).

  Points are stored in their natural reduced form (not octave-normalized).
  Use `nomos.maths.harmonic/ratio-normalize` when computing pitch output.

  ## Building regions
    (lattice-region {:otonal-limit 11 :utonal-limit 7 :tenney-limit 6.5})
    ; → sorted vector of [m n] pairs, ordered by Tenney H ascending

  ## Querying
    (in-region? [5 4] region)     ; → true/false
    (region-points region)        ; → sorted [m n] vector

  ## Navigation support
    (sort-by-h-from [9 8] points) ; → points sorted by Tenney distance from [9 8]
    (gravity-steps [9 8] region)  ; → points with lower H than [9 8], nearest first
    (expansion-steps [3 2] region); → points with higher H than [3 2], nearest first
    (origin-point region)         ; → [1 1] or simplest point

  ## Chord structures
    (otonal-points [3 2] region)  ; → all region points sharing denominator 2
    (utonal-points [5 4] region)  ; → all region points sharing numerator 5

  ## Interval and prime analysis
    (prime-limit [7 4])           ; → 7
    (prime-factors n)             ; → sorted list of prime factors of n"
  (:require [nomos.maths.harmonic :as h]))

;; ---------------------------------------------------------------------------
;; Internal helpers
;; ---------------------------------------------------------------------------

(defn- gcd ^long [^long a ^long b]
  (loop [a (Math/abs a) b (Math/abs b)]
    (if (zero? b) a (recur b (mod a b)))))

(defn- coprime? [m n]
  (= 1 (gcd (long m) (long n))))

;; ---------------------------------------------------------------------------
;; Prime factorization
;; ---------------------------------------------------------------------------

(defn prime-factors
  "Return the sorted list of distinct prime factors of positive integer n."
  [n]
  (loop [n   (long n)
         d   2
         acc []]
    (cond
      (> (* d d) n) (if (> n 1) (if (some #{n} acc) acc (conj acc n)) acc)
      (zero? (mod n d)) (recur (/ n d) d (if (some #{d} acc) acc (conj acc d)))
      :else (recur n (inc d) acc))))

(defn prime-limit
  "Return the highest prime appearing in either m or n of lattice point [m n]."
  [[m n]]
  (let [primes (distinct (concat (prime-factors m) (prime-factors n)))]
    (if (seq primes) (apply max primes) 1)))

;; ---------------------------------------------------------------------------
;; Region
;; ---------------------------------------------------------------------------

(defrecord LatticeRegion [points point-set])

(defn lattice-region
  "Build the set of valid lattice points within the given limits.

  Options:
    :otonal-limit  — maximum overtone index m (default 11)
    :utonal-limit  — maximum undertone index n (default 7)
    :tenney-limit  — maximum Tenney H (default 6.5)
    :prime-limit   — if set, exclude points whose prime-limit exceeds this
    :octave-fold   — if true (default), include only points where m/n ∈ [1, 2);
                     if false, include all m/n > 0 within the index limits

  Returns a LatticeRegion with points sorted by Tenney H ascending."
  ([] (lattice-region {}))
  ([{:keys [otonal-limit utonal-limit tenney-limit prime-lim octave-fold]
     :or   {otonal-limit 11 utonal-limit 7 tenney-limit 6.5 octave-fold true}}]
   (let [points
         (->> (for [m (range 1 (inc (long otonal-limit)))
                    n (range 1 (inc (long utonal-limit)))
                    :when (coprime? m n)
                    :let  [r (/ (double m) (double n))
                           th (h/tenney-h [m n])]
                    :when (<= th (double tenney-limit))
                    :when (or (not octave-fold) (and (>= r 1.0) (< r 2.0)))
                    :when (or (nil? prime-lim) (<= (prime-limit [m n]) (long prime-lim)))]
                [m n])
              (sort-by #(h/tenney-h %))
              vec)]
     (->LatticeRegion points (set points)))))

(defn region-points
  "Return the sorted vector of [m n] points in region."
  [^LatticeRegion region]
  (:points region))

(defn in-region?
  "Return true if lattice point [m n] is in region."
  [point ^LatticeRegion region]
  (contains? (:point-set region) point))

(defn origin-point
  "Return the simplest point in region (lowest Tenney H, typically [1 1])."
  [^LatticeRegion region]
  (first (:points region)))

;; ---------------------------------------------------------------------------
;; Distance and ordering
;; ---------------------------------------------------------------------------

(defn sort-by-h-from
  "Sort a collection of lattice points by Tenney distance from `position`.
  Nearest points (smallest H-distance) come first."
  [position points]
  (sort-by #(h/tenney-h % position) points))

(defn points-by-distance
  "Return all region points sorted by Tenney distance from `position`.
  Each entry is a map {:point [m n] :h-from-here D :h-from-origin O}."
  [position ^LatticeRegion region]
  (->> (:points region)
       (map (fn [p]
              {:point          p
               :h-from-here    (h/tenney-h p position)
               :h-from-origin  (h/tenney-h p)}))
       (sort-by :h-from-here)))

(defn gravity-steps
  "Return region points that are closer to the origin than `position`,
  sorted nearest-first (smallest H-distance from position).

  These are the candidate steps under harmonic gravity — moving to any of
  these reduces tension. deflattice selects among them using gravity-weight."
  [position ^LatticeRegion region]
  (let [current-h (h/tenney-h position)]
    (->> (:points region)
         (filter #(< (h/tenney-h %) current-h))
         (sort-by #(h/tenney-h % position)))))

(defn expansion-steps
  "Return region points that are farther from the origin than `position`,
  sorted nearest-first (smallest H-distance from position).

  These are the candidate steps for harmonic expansion — moving to any of
  these increases tension. Used in the departure phase of defexcursion."
  [position ^LatticeRegion region]
  (let [current-h (h/tenney-h position)]
    (->> (:points region)
         (filter #(> (h/tenney-h %) current-h))
         (sort-by #(h/tenney-h % position)))))

(defn nearest-points
  "Return the n nearest region points to `position` by Tenney distance,
  excluding `position` itself."
  [position ^LatticeRegion region n]
  (->> (:points region)
       (remove #{position})
       (sort-by #(h/tenney-h % position))
       (take n)))

;; ---------------------------------------------------------------------------
;; Chord structures
;; ---------------------------------------------------------------------------

(defn otonal-points
  "Return all region points that share the denominator of `point`.
  These form the Otonal chord on that denominator — all points [m n']
  where n' = (second point), drawn from the overtone series above a common
  undertone root.

  Example: otonal-points [5 4] → all [m 4] in region
           → [1 4]? No — not in [1,2). → [5 4], [7 4], [9 4]... within limits."
  [[_ n :as _point] ^LatticeRegion region]
  (filter #(= n (second %)) (:points region)))

(defn utonal-points
  "Return all region points that share the numerator of `point`.
  These form the Utonal chord on that numerator — all points [m' n]
  where m' = (first point), drawn from the undertone series below a common
  overtone root.

  Example: utonal-points [5 4] → all [5 n] in region
           → [5 3], [5 4], [5 6]... within limits."
  [[m _ :as _point] ^LatticeRegion region]
  (filter #(= m (first %)) (:points region)))

(defn otonal-chord
  "Return the Otonal hexad within `region` built on undertone index n.
  An Otonal chord has all members sharing the same denominator — they are
  drawn from consecutive members of the overtone series above a common root.
  Sorted by Tenney H ascending (simplest first)."
  [n ^LatticeRegion region]
  (->> (:points region)
       (filter #(= (long n) (second %)))
       (sort-by #(h/tenney-h %))))

(defn utonal-chord
  "Return the Utonal hexad within `region` built on overtone index m.
  A Utonal chord has all members sharing the same numerator — they are
  drawn from consecutive members of the undertone series below a common root.
  Sorted by Tenney H ascending (simplest first)."
  [m ^LatticeRegion region]
  (->> (:points region)
       (filter #(= (long m) (first %)))
       (sort-by #(h/tenney-h %))))

;; ---------------------------------------------------------------------------
;; Lattice statistics
;; ---------------------------------------------------------------------------

(defn region-stats
  "Return a summary map for a region:
    :count          — number of points
    :tenney-range   — [min-H max-H]
    :prime-limits   — sorted vector of distinct prime limits present
    :origin?        — whether [1 1] is in the region"
  [^LatticeRegion region]
  (let [pts (:points region)
        hs  (map #(h/tenney-h %) pts)]
    {:count        (count pts)
     :tenney-range [(apply min hs) (apply max hs)]
     :prime-limits (sort (distinct (map prime-limit pts)))
     :origin?      (contains? (:point-set region) [1 1])}))
