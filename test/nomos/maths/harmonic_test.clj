; SPDX-License-Identifier: EPL-2.0
(ns nomos.maths.harmonic-test
  "Unit tests for nomos.maths.harmonic — JI arithmetic, Tenney distance, gravity."
  (:require [clojure.test :refer [deftest is testing]]
            [nomos.maths.harmonic :as h]))

(def eps 1e-6)
(defn ≈ [a b] (< (Math/abs (- (double a) (double b))) eps))

;; ---------------------------------------------------------------------------
;; ratio-reduce
;; ---------------------------------------------------------------------------

(deftest ratio-reduce-test
  (testing "already reduced"
    (is (= [1 1]  (h/ratio-reduce [1 1])))
    (is (= [3 2]  (h/ratio-reduce [3 2])))
    (is (= [15 8] (h/ratio-reduce [15 8]))))
  (testing "reduces to lowest terms"
    (is (= [3 2] (h/ratio-reduce [6 4])))
    (is (= [2 1] (h/ratio-reduce [4 2])))
    (is (= [5 3] (h/ratio-reduce [10 6])))
    (is (= [1 1] (h/ratio-reduce [7 7])))))

;; ---------------------------------------------------------------------------
;; ratio-normalize
;; ---------------------------------------------------------------------------

(deftest ratio-normalize-test
  (testing "already in [1, 2) — unchanged"
    (is (= [3 2]  (h/ratio-normalize [3 2])))
    (is (= [5 4]  (h/ratio-normalize [5 4])))
    (is (= [1 1]  (h/ratio-normalize [1 1]))))
  (testing "above 2 — divide down"
    (is (= [3 2] (h/ratio-normalize [3 1])))   ; 3/1 → 3/2
    (is (= [5 4] (h/ratio-normalize [5 2])))   ; 5/2 → 5/4
    (is (= [9 8] (h/ratio-normalize [9 4]))))  ; 9/4 → 9/8
  (testing "below 1 — multiply up"
    (is (= [4 3] (h/ratio-normalize [1 3])))   ; 1/3 → 4/3
    (is (= [8 5] (h/ratio-normalize [4 5]))))  ; 4/5 → 8/5
  (testing "octave reduces to unison"
    (is (= [1 1] (h/ratio-normalize [2 1])))   ; octave class = unison
    (is (= [1 1] (h/ratio-normalize [4 1]))))  ; double octave = unison
  (testing "reduces after normalizing"
    (is (= [3 2] (h/ratio-normalize [6 4])))))  ; 6/4 → reduce → 3/2

;; ---------------------------------------------------------------------------
;; ratio-inverse
;; ---------------------------------------------------------------------------

(deftest ratio-inverse-test
  (testing "reciprocal, normalized to [1, 2)"
    (is (= [4 3] (h/ratio-inverse [3 2])))    ; 2/3 → 4/3
    (is (= [8 5] (h/ratio-inverse [5 4])))    ; 4/5 → 8/5
    (is (= [5 3] (h/ratio-inverse [6 5])))    ; 5/6 → 5/3 wait: 5/6 < 1 → 10/6 → [5 3] ✓
    (is (= [1 1] (h/ratio-inverse [1 1])))))  ; 1/1 → 1/1

;; ---------------------------------------------------------------------------
;; ratio-interval
;; ---------------------------------------------------------------------------

(deftest ratio-interval-test
  (testing "interval from [1 1] — identity"
    (is (= [3 2] (h/ratio-interval [3 2] [1 1])))
    (is (= [5 4] (h/ratio-interval [5 4] [1 1]))))
  (testing "interval from self — unison"
    (is (= [1 1] (h/ratio-interval [3 2] [3 2])))
    (is (= [1 1] (h/ratio-interval [5 4] [5 4]))))
  (testing "major sixth above the fifth is a major third below the octave"
    ;; (5/4) / (3/2) = 10/12 = 5/6 → normalized → 5/3
    (is (= [5 3] (h/ratio-interval [5 4] [3 2]))))
  (testing "minor third above major third is perfect fifth"
    ;; (3/2) / (5/4) = 12/10 = 6/5 → normalized → 6/5
    (is (= [6 5] (h/ratio-interval [3 2] [5 4])))))

;; ---------------------------------------------------------------------------
;; tenney-h
;; ---------------------------------------------------------------------------

(deftest tenney-h-single-arity-test
  (testing "unison has zero distance"
    (is (≈ 0.0 (h/tenney-h [1 1]))))
  (testing "octave"
    (is (≈ 1.0 (h/tenney-h [2 1]))))
  (testing "perfect fifth — log₂(6)"
    (is (≈ (/ (Math/log 6.0) (Math/log 2.0))
           (h/tenney-h [3 2]))))
  (testing "perfect fourth — log₂(12)"
    (is (≈ (/ (Math/log 12.0) (Math/log 2.0))
           (h/tenney-h [4 3]))))
  (testing "major third — log₂(20)"
    (is (≈ (/ (Math/log 20.0) (Math/log 2.0))
           (h/tenney-h [5 4]))))
  (testing "major sixth — log₂(15)"
    (is (≈ (/ (Math/log 15.0) (Math/log 2.0))
           (h/tenney-h [5 3]))))
  (testing "tritone — highest common-practice dissonance"
    (is (> (h/tenney-h [45 32]) 10.0)))
  (testing "input is reduced before computing — [6 4] = [3 2]"
    (is (≈ (h/tenney-h [3 2]) (h/tenney-h [6 4]))))
  (testing "single harmonic integer form"
    (is (≈ 0.0 (h/tenney-h 1)))
    (is (≈ 1.0 (h/tenney-h 2)))
    (is (≈ (/ (Math/log 5.0) (Math/log 2.0)) (h/tenney-h 5))))
  (testing "consonance band structure matches the voice leading seed thresholds"
    ;; H < 3.5: perfect consonances (fifth, octave) — acoustic fusion / parallel motion risk
    ;; H 3.5–5.5: imperfect consonances (thirds, sixths, fourth) — preferred for counterpoint
    ;; H > 5.5: dissonance — active tension requiring resolution
    (let [fifth     (h/tenney-h [3 2])   ; 2.585
          fourth    (h/tenney-h [4 3])   ; 3.585 — just inside imperfect band
          maj-third (h/tenney-h [5 4])   ; 4.322
          maj-sixth (h/tenney-h [5 3])   ; 3.907
          min-third (h/tenney-h [6 5])]  ; 4.907
      (is (< fifth 3.5))          ; perfect consonance
      (is (> fourth 3.5))         ; fourth is imperfect (just above threshold)
      (is (< fourth 5.5))
      (is (> maj-third 3.5))      ; thirds and sixths in imperfect band
      (is (< maj-third 5.5))
      (is (> maj-sixth 3.5))
      (is (< maj-sixth 5.5))
      (is (> min-third 3.5))
      (is (< min-third 5.5)))))

(deftest tenney-h-movable-attractor-test
  (testing "distance from self is zero"
    (is (≈ 0.0 (h/tenney-h [3 2] [3 2])))
    (is (≈ 0.0 (h/tenney-h [5 4] [5 4]))))
  (testing "attractor [1 1] gives standard Tenney H"
    (is (≈ (h/tenney-h [3 2]) (h/tenney-h [3 2] [1 1])))
    (is (≈ (h/tenney-h [5 4]) (h/tenney-h [5 4] [1 1]))))
  (testing "major sixth above the fifth has H = log₂(15)"
    ;; interval from [3 2] to [5 4] is [5 3]; H([5 3]) = log₂(15)
    (is (≈ (/ (Math/log 15.0) (Math/log 2.0))
           (h/tenney-h [5 4] [3 2]))))
  (testing "distance is not symmetric (interval direction matters)"
    ;; from fifth to major-third = major-sixth [5 3]; H ≈ 3.907
    ;; from major-third to fifth = minor-sixth [6 5]; H ≈ 4.907
    (let [from-fifth-to-third (h/tenney-h [5 4] [3 2])
          from-third-to-fifth (h/tenney-h [3 2] [5 4])]
      (is (not (≈ from-fifth-to-third from-third-to-fifth))))))

;; ---------------------------------------------------------------------------
;; Pitch conversion
;; ---------------------------------------------------------------------------

(deftest ratio->cents-test
  (testing "unison = 0 cents"
    (is (≈ 0.0 (h/ratio->cents [1 1]))))
  (testing "octave = 1200 cents"
    (is (≈ 1200.0 (h/ratio->cents [2 1]))))
  (testing "just fifth ≈ 701.955 cents (not 700)"
    (is (≈ (* 1200.0 (/ (Math/log 1.5) (Math/log 2.0)))
           (h/ratio->cents [3 2])))
    (is (> (h/ratio->cents [3 2]) 700.0))
    (is (< (h/ratio->cents [3 2]) 702.0))))

(deftest ratio->voct-test
  (testing "unison = 0V"
    (is (≈ 0.0 (h/ratio->voct [1 1]))))
  (testing "octave = 1V"
    (is (≈ 1.0 (h/ratio->voct [2 1]))))
  (testing "V/oct consistent with cents / 1200"
    (is (≈ (/ (h/ratio->cents [3 2]) 1200.0)
           (h/ratio->voct [3 2])))))

(deftest ratio->midi-offset-test
  (testing "unison = 0 semitones"
    (is (≈ 0.0 (h/ratio->midi-offset [1 1]))))
  (testing "octave = 12 semitones"
    (is (≈ 12.0 (h/ratio->midi-offset [2 1]))))
  (testing "just major third ≈ 3.86 semitones (not 4)"
    (let [offset (h/ratio->midi-offset [5 4])]
      (is (> offset 3.8))
      (is (< offset 3.9))))
  (testing "just fifth ≈ 7.02 semitones (not 7)"
    (let [offset (h/ratio->midi-offset [3 2])]
      (is (> offset 7.0))
      (is (< offset 7.1)))))

;; ---------------------------------------------------------------------------
;; semitone->ji-ratio
;; ---------------------------------------------------------------------------

(deftest semitone->ji-ratio-test
  (testing "all 13 entries present"
    (is (= 13 (count h/semitone->ji-ratio))))
  (testing "unison and octave"
    (is (= [1 1] (get h/semitone->ji-ratio 0)))
    (is (= [2 1] (get h/semitone->ji-ratio 12))))
  (testing "perfect fifth is [3 2]"
    (is (= [3 2] (get h/semitone->ji-ratio 7))))
  (testing "Tenney H increases with semitone distance (generally)"
    (let [h0 (h/tenney-h (get h/semitone->ji-ratio 0))
          h5 (h/tenney-h (get h/semitone->ji-ratio 5))
          h6 (h/tenney-h (get h/semitone->ji-ratio 6))]
      (is (< h0 h5))
      (is (< h5 h6)))))   ; tritone is most dissonant

;; ---------------------------------------------------------------------------
;; gravity-field
;; ---------------------------------------------------------------------------

(deftest gravity-field-test
  (testing "at attractor — zero pull"
    (is (≈ 0.0 (h/gravity-field [1 1])))
    (is (≈ 0.0 (h/gravity-field [3 2] :attractor [3 2]))))
  (testing "output is in [0, 1]"
    (doseq [ratio [[3 2] [5 4] [9 8] [7 4] [45 32]]]
      (let [f (h/gravity-field ratio)]
        (is (>= f 0.0))
        (is (<= f 1.0)))))
  (testing "clamped to 1.0 beyond horizon"
    (is (≈ 1.0 (h/gravity-field [45 32] :horizon 8.0))))  ; tritone H > 10 > horizon 8
  (testing "stronger pull farther from attractor"
    (is (< (h/gravity-field [3 2]) (h/gravity-field [9 8]))))  ; fifth closer than major second
  (testing "custom attractor shifts the field"
    (let [at-fifth-from-fifth (h/gravity-field [3 2] :attractor [3 2])
          at-fifth-from-root  (h/gravity-field [3 2] :attractor [1 1])]
      (is (≈ 0.0 at-fifth-from-fifth))
      (is (> at-fifth-from-root 0.0)))))

;; ---------------------------------------------------------------------------
;; gravity-weight
;; ---------------------------------------------------------------------------

(deftest gravity-weight-test
  (testing "at attractor — maximum weight 1.0"
    (is (≈ 1.0 (h/gravity-weight [1 1])))
    (is (≈ 1.0 (h/gravity-weight [3 2] :attractor [3 2]))))
  (testing "weight is in (0, 1]"
    (doseq [ratio [[3 2] [5 4] [9 8] [7 4]]]
      (let [w (h/gravity-weight ratio)]
        (is (> w 0.0))
        (is (<= w 1.0)))))
  (testing "lower H → higher weight"
    (is (> (h/gravity-weight [3 2]) (h/gravity-weight [9 8]))))
  (testing "higher strength sharpens the preference"
    (let [w1 (h/gravity-weight [9 8] :strength 1.0)
          w2 (h/gravity-weight [9 8] :strength 3.0)]
      (is (> w1 w2)))))  ; higher strength → lower weight for non-attractor positions

;; ---------------------------------------------------------------------------
;; gravity-force
;; ---------------------------------------------------------------------------

(deftest gravity-force-test
  (testing "at attractor — zero force"
    (is (≈ 0.0 (h/gravity-force [1 1])))
    (is (≈ 0.0 (h/gravity-force [3 2] :attractor [3 2]))))
  (testing "force is non-negative"
    (doseq [ratio [[3 2] [5 4] [9 8]]]
      (is (>= (h/gravity-force ratio) 0.0))))
  (testing "larger tau reduces force magnitude"
    (let [f-tight (h/gravity-force [9 8] :tau 2.0)
          f-loose (h/gravity-force [9 8] :tau 16.0)]
      (is (> f-tight f-loose))))
  (testing "force scales with dt"
    (let [f1 (h/gravity-force [9 8] :dt 1.0)
          f2 (h/gravity-force [9 8] :dt 2.0)]
      (is (≈ (* 2.0 f1) f2)))))

;; ---------------------------------------------------------------------------
;; gravity-accept?
;; ---------------------------------------------------------------------------

(deftest gravity-accept-test
  (testing "always accept movement toward attractor (lower H)"
    (is (h/gravity-accept? 5.0 3.0))
    (is (h/gravity-accept? 4.32 2.58))
    (is (h/gravity-accept? 0.01 0.0)))
  (testing "always accept no movement (same H)"
    (is (h/gravity-accept? 3.0 3.0)))
  (testing "very low temperature rarely accepts uphill movement"
    (let [trials  1000
          accepts (count (filter identity
                                 (repeatedly trials
                                             #(h/gravity-accept? 2.0 6.0 :temp 0.001))))]
      (is (< accepts 5))))   ; near zero probability
  (testing "very high temperature frequently accepts uphill movement"
    (let [trials  1000
          accepts (count (filter identity
                                 (repeatedly trials
                                             #(h/gravity-accept? 2.0 3.0 :temp 100.0))))]
      (is (> accepts 900))))) ; near certainty
