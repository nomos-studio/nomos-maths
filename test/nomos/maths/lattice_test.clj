; SPDX-License-Identifier: EPL-2.0
(ns nomos.maths.lattice-test
  "Unit tests for nomos.maths.lattice — region generation, neighbor queries, chords."
  (:require [clojure.test :refer [deftest is testing]]
            [nomos.maths.harmonic :as h]
            [nomos.maths.lattice :as l]))

;; Standard test region: 5-limit, tenney ≤ 5.5 — covers common JI intervals
(def r5 (l/lattice-region {:otonal-limit 9 :utonal-limit 9 :tenney-limit 5.5}))

(defn- gcd-test [a b]
  (loop [a (Math/abs (long a)) b (Math/abs (long b))]
    (if (zero? b) a (recur b (mod a b)))))

;; ---------------------------------------------------------------------------
;; prime-factors
;; ---------------------------------------------------------------------------

(deftest prime-factors-test
  (testing "primes return themselves"
    (is (= [2]    (l/prime-factors 2)))
    (is (= [3]    (l/prime-factors 3)))
    (is (= [7]    (l/prime-factors 7))))
  (testing "composite numbers"
    (is (= [2 3]  (l/prime-factors 6)))
    (is (= [2 5]  (l/prime-factors 10)))
    (is (= [3 5]  (l/prime-factors 15)))
    (is (= [2 3 5] (l/prime-factors 30))))
  (testing "prime powers — distinct factors only"
    (is (= [2] (l/prime-factors 4)))
    (is (= [2] (l/prime-factors 8)))
    (is (= [3] (l/prime-factors 9))))
  (testing "1 has no prime factors"
    (is (= [] (l/prime-factors 1)))))

;; ---------------------------------------------------------------------------
;; prime-limit
;; ---------------------------------------------------------------------------

(deftest prime-limit-test
  (testing "3-limit intervals"
    (is (= 3 (l/prime-limit [3 2])))
    (is (= 3 (l/prime-limit [4 3])))
    (is (= 2 (l/prime-limit [2 1]))))
  (testing "5-limit intervals"
    (is (= 5 (l/prime-limit [5 4])))
    (is (= 5 (l/prime-limit [6 5])))
    (is (= 5 (l/prime-limit [5 3]))))
  (testing "7-limit intervals"
    (is (= 7 (l/prime-limit [7 4])))
    (is (= 7 (l/prime-limit [7 6]))))
  (testing "origin"
    (is (= 1 (l/prime-limit [1 1])))))

;; ---------------------------------------------------------------------------
;; lattice-region
;; ---------------------------------------------------------------------------

(deftest lattice-region-test
  (testing "origin [1 1] is always present (H=0, always within limits)"
    (is (l/in-region? [1 1] r5)))
  (testing "points are sorted by Tenney H ascending"
    (let [pts (l/region-points r5)
          hs  (map #(h/tenney-h %) pts)]
      (is (= hs (sort hs)))))
  (testing "no duplicates"
    (let [pts (l/region-points r5)]
      (is (= (count pts) (count (set pts))))))
  (testing "all points are coprime (gcd = 1)"
    (doseq [[m n] (l/region-points r5)]
      (is (= 1 (gcd-test m n)) (str "not coprime: " [m n]))))
  (testing "all points within tenney-limit"
    (doseq [p (l/region-points r5)]
      (is (<= (h/tenney-h p) 5.5))))
  (testing "known 5-limit intervals are present"
    (is (l/in-region? [3 2] r5))   ; perfect fifth
    (is (l/in-region? [4 3] r5))   ; perfect fourth
    (is (l/in-region? [5 4] r5))   ; major third
    (is (l/in-region? [6 5] r5))   ; minor third
    (is (l/in-region? [5 3] r5)))  ; major sixth
  (testing "7-limit intervals excluded from 5-limit region with prime-limit filter"
    (let [r5-prime (l/lattice-region {:otonal-limit 9 :utonal-limit 9
                                      :tenney-limit 5.5 :prime-lim 5})]
      (is (not (l/in-region? [7 4] r5-prime)))
      (is (not (l/in-region? [7 6] r5-prime)))))
  (testing "octave-fold false includes multi-octave intervals"
    (let [r-wide (l/lattice-region {:otonal-limit 5 :utonal-limit 1
                                    :tenney-limit 4.0 :octave-fold false})]
      (is (l/in-region? [3 1] r-wide))   ; perfect twelfth
      (is (l/in-region? [5 1] r-wide)))) ; major seventeenth
  (testing "octave-fold true excludes multi-octave intervals"
    (is (not (l/in-region? [3 1] r5)))   ; 3/1 = 3.0 — outside [1, 2)
    (is (not (l/in-region? [5 1] r5)))))

;; ---------------------------------------------------------------------------
;; gravity-steps and expansion-steps
;; ---------------------------------------------------------------------------

(deftest gravity-steps-test
  (testing "from origin — no gravity steps (nowhere lower)"
    (is (empty? (l/gravity-steps [1 1] r5))))
  (testing "from major second [9 8] — has steps toward lower H"
    (let [steps (l/gravity-steps [9 8] r5)]
      (is (seq steps))
      (is (every? #(< (h/tenney-h %) (h/tenney-h [9 8])) steps))))
  (testing "gravity steps are sorted nearest-first"
    (let [steps (l/gravity-steps [9 8] r5)
          dists (map #(h/tenney-h % [9 8]) steps)]
      (is (= dists (sort dists)))))
  (testing "gravity steps all have lower H than the current position"
    (doseq [pos (l/region-points r5)]
      (let [current-h (h/tenney-h pos)
            steps     (l/gravity-steps pos r5)]
        (is (every? #(< (h/tenney-h %) current-h) steps))))))

(deftest expansion-steps-test
  (testing "from most complex point — no expansion steps"
    (let [most-complex (last (l/region-points r5))]
      (is (empty? (l/expansion-steps most-complex r5)))))
  (testing "from origin — all non-origin points are expansion steps"
    (let [steps (l/expansion-steps [1 1] r5)
          non-origin (filter #(not= [1 1] %) (l/region-points r5))]
      (is (= (count non-origin) (count steps)))))
  (testing "expansion steps are sorted nearest-first"
    (let [steps (l/expansion-steps [1 1] r5)
          dists (map #(h/tenney-h % [1 1]) steps)]
      (is (= dists (sort dists)))))
  (testing "expansion steps all have higher H than current position"
    (doseq [pos (l/region-points r5)]
      (let [current-h (h/tenney-h pos)
            steps     (l/expansion-steps pos r5)]
        (is (every? #(> (h/tenney-h %) current-h) steps))))))

;; ---------------------------------------------------------------------------
;; gravity-steps and expansion-steps — movable attractor (3-arity)
;; ---------------------------------------------------------------------------

(deftest gravity-steps-attractor-backward-compat-test
  (testing "3-arity with [1 1] matches 2-arity for all region points"
    (doseq [pos (l/region-points r5)]
      (is (= (l/gravity-steps pos r5)
             (l/gravity-steps pos [1 1] r5))))))

(deftest gravity-steps-attractor-semantics-test
  (testing "at the attractor — no gravity steps (H=0 from attractor, nothing lower)"
    (is (empty? (l/gravity-steps [3 2] [3 2] r5))))
  (testing "from [5 4] toward [3 2] — includes [3 2] and [1 1]"
    ;; H([5 4], [3 2]) = H([5 3]) ≈ 3.91
    ;; H([3 2], [3 2]) = 0    → included
    ;; H([1 1], [3 2]) = H([4 3]) ≈ 3.58 < 3.91 → included
    (let [steps (set (l/gravity-steps [5 4] [3 2] r5))]
      (is (contains? steps [3 2]))
      (is (contains? steps [1 1]))))
  (testing "from [5 4] toward [3 2] — excludes [4 3] (further from [3 2] than [5 4] is)"
    ;; H([4 3], [3 2]) = H([16 9]) ≈ 7.17 > H([5 4], [3 2]) ≈ 3.91
    (let [steps (set (l/gravity-steps [5 4] [3 2] r5))]
      (is (not (contains? steps [4 3])))))
  (testing "[4 3] IS a gravity step from [5 4] with default attractor [1 1]"
    ;; Confirms the attractor choice makes a real difference
    (let [steps (set (l/gravity-steps [5 4] r5))]
      (is (contains? steps [4 3]))))
  (testing "all gravity steps are closer to attractor than position is"
    (doseq [pos (l/region-points r5)]
      (let [attractor [3 2]
            current-h (h/tenney-h pos attractor)
            steps     (l/gravity-steps pos attractor r5)]
        (is (every? #(< (h/tenney-h % attractor) current-h) steps)))))
  (testing "gravity steps sorted nearest-first from position"
    (let [steps (l/gravity-steps [5 4] [3 2] r5)
          dists (map #(h/tenney-h % [5 4]) steps)]
      (is (= dists (sort dists))))))

(deftest expansion-steps-attractor-backward-compat-test
  (testing "3-arity with [1 1] matches 2-arity for all region points"
    (doseq [pos (l/region-points r5)]
      (is (= (l/expansion-steps pos r5)
             (l/expansion-steps pos [1 1] r5))))))

(deftest expansion-steps-attractor-semantics-test
  (testing "at attractor [3 2] — all other region points are expansion steps"
    ;; Analogous to expansion-steps([1 1], r5) returning all non-origin points
    (let [steps    (l/expansion-steps [3 2] [3 2] r5)
          non-attr (filter #(not= [3 2] %) (l/region-points r5))]
      (is (= (count non-attr) (count steps)))))
  (testing "from [5 4] toward [3 2] — [4 3] is an expansion step"
    ;; H([4 3], [3 2]) ≈ 7.17 > H([5 4], [3 2]) ≈ 3.91
    (let [steps (set (l/expansion-steps [5 4] [3 2] r5))]
      (is (contains? steps [4 3]))))
  (testing "from [5 4] toward [3 2] — [1 1] is NOT an expansion step"
    ;; H([1 1], [3 2]) ≈ 3.58 < 3.91 — [1 1] is a gravity step, not expansion
    (let [steps (set (l/expansion-steps [5 4] [3 2] r5))]
      (is (not (contains? steps [1 1])))))
  (testing "all expansion steps are farther from attractor than position is"
    (doseq [pos (l/region-points r5)]
      (let [attractor [3 2]
            current-h (h/tenney-h pos attractor)
            steps     (l/expansion-steps pos attractor r5)]
        (is (every? #(> (h/tenney-h % attractor) current-h) steps)))))
  (testing "expansion steps sorted nearest-first from position"
    (let [steps (l/expansion-steps [3 2] [3 2] r5)
          dists (map #(h/tenney-h % [3 2]) steps)]
      (is (= dists (sort dists))))))

;; ---------------------------------------------------------------------------
;; nearest-points
;; ---------------------------------------------------------------------------

(deftest nearest-points-test
  (testing "returns n nearest excluding self"
    (let [nn (l/nearest-points [3 2] r5 3)]
      (is (= 3 (count nn)))
      (is (not (some #{[3 2]} nn)))))
  (testing "nearest to origin are the simplest intervals in the region"
    ;; With octave-fold=true, [2 1] (the octave) is excluded since 2/1 = 2.0 ≥ 2.
    ;; The nearest are [3 2] (H ≈ 2.58) and [4 3] (H ≈ 3.58).
    (let [nn (l/nearest-points [1 1] r5 2)
          hs (map #(h/tenney-h %) nn)]
      (is (every? #(< % 4.0) hs))))
  (testing "sorted by distance"
    (let [nn   (l/nearest-points [9 8] r5 4)
          dists (map #(h/tenney-h % [9 8]) nn)]
      (is (= dists (sort dists))))))

;; ---------------------------------------------------------------------------
;; Chord structures
;; ---------------------------------------------------------------------------

(deftest otonal-chord-test
  (testing "all points share the given denominator"
    (let [chord (l/otonal-chord 4 r5)]
      (is (every? #(= 4 (second %)) chord))))
  (testing "includes expected Otonal members on n=4"
    ;; [5 4] has n=4 ✓, [3 2] has n=2 (NOT included), [9 4] outside limits
    (let [chord (set (l/otonal-chord 4 r5))]
      (is (contains? chord [5 4]))       ; major third — n=4 ✓
      (is (not (contains? chord [3 2]))) ; fifth has n=2, not n=4
      (is (not (contains? chord [9 8])))))
  (testing "[5 4] and [7 4] both in Otonal chord on n=4 (if 7 is in region)"
    (let [r7    (l/lattice-region {:otonal-limit 11 :utonal-limit 7 :tenney-limit 6.5})
          chord (set (l/otonal-chord 4 r7))]
      (is (contains? chord [5 4]))
      (is (contains? chord [7 4]))))
  (testing "sorted by Tenney H"
    (let [r7    (l/lattice-region {:otonal-limit 11 :utonal-limit 7 :tenney-limit 6.5})
          chord (l/otonal-chord 4 r7)
          hs    (map #(h/tenney-h %) chord)]
      (is (= hs (sort hs))))))

(deftest utonal-chord-test
  (testing "all points share the given numerator"
    (let [chord (l/utonal-chord 5 r5)]
      (is (every? #(= 5 (first %)) chord))))
  (testing "includes expected Utonal members"
    (let [chord (set (l/utonal-chord 5 r5))]
      (is (contains? chord [5 4]))   ; major third
      (is (contains? chord [5 3])))) ; major sixth
  (testing "sorted by Tenney H"
    (let [chord (l/utonal-chord 5 r5)
          hs    (map #(h/tenney-h %) chord)]
      (is (= hs (sort hs))))))

;; ---------------------------------------------------------------------------
;; region-stats
;; ---------------------------------------------------------------------------

(deftest region-stats-test
  (testing "origin is present"
    (is (:origin? (l/region-stats r5))))
  (testing "count > 0"
    (is (pos? (:count (l/region-stats r5)))))
  (testing "tenney-range: min is 0 (origin), max <= tenney-limit"
    (let [{[lo hi] :tenney-range} (l/region-stats r5)]
      (is (< lo 0.001))   ; origin has H = 0
      (is (<= hi 5.5))))
  (testing "prime-limits only includes primes actually present"
    (let [r3 (l/lattice-region {:otonal-limit 9 :utonal-limit 9
                                :tenney-limit 5.5 :prime-lim 3})
          stats (l/region-stats r3)]
      (is (every? #(<= % 3) (:prime-limits stats))))))
