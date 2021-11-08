(ns org-rwtodd.cellz
  )

;; ====== utilities...
(defn print-row
  "Display a row in a line"
  [row]
  (println (apply str  (map (fn [n]
                              (case n
                                0 \.
                                n)) row))))

;; ====== Rule steppers....
(defn step-w2!
  "Step forward a row, using the given rule func at width of 2"
  [^bytes row rule]
  (let [rlast     (dec (alength row))
        orig-idx0 (aget row 0)]
    (dotimes [idx rlast]
      (aset-byte row idx (rule (aget row idx) (aget row (inc idx)))))
    (aset-byte row rlast (rule (aget row rlast) orig-idx0))
    row))

(defn step-w3!
  "Step forward a row, using the given rule func at width of 3"
  [^bytes row rule]
  (let [rlast     (dec (alength row))
        orig-idx0 (aget row 0)]
    (loop [idx  0
           prev (aget row rlast)]
      (if (< idx rlast)
        (let [curval (aget row idx)]
          (aset-byte row idx (rule prev curval (aget row (inc idx))))
          (recur (inc idx) curval))
        ;; on the last one...
        (aset-byte row idx (rule prev (aget row rlast) orig-idx0))))))

(defn run-rule-w2
  "Run STEPS steps of width-2 rule RULE against initial row R"
  [steps r rule]
  (print-row r)
  (dotimes [_ steps]
    (step-w2! r rule)
    (print-row r)))

(defn run-rule-w3
  "Run STEPS steps of width-3 rule RULE against initial row R"
  [steps r rule]
  (print-row r)
  (dotimes [_ steps]
    (step-w3! r rule)
    (print-row r)))

;; ====== Ways to set the initial cell
(defn initial-single-cell
  "Create an initial row with 1 cell set to 1"
  [^long size]
  (doto (byte-array size) (aset-byte (bit-shift-right size 1) 1)))

(defn initial-random-cells
  "Create an initial row with random cells that can be from 0 to VALS-1"
  [^long size vals]
  (byte-array (repeatedly size #(byte (rand vals)))))

;; ====== Example rules ----
(defn rfunc-2
  [a b]
  (mod (+ a b) 3))

(defn rfunc-3
  [a b c]
  (mod
   (if (<= a c) (+ a c) (+ a b))
   3))

(comment "some example runs"
    (run-rule-w3 20 (initial-single-cell 21) rfunc-3))
    
;; ====== end of file
;; Local Variables:
;; page-delimiter: "^;; ======"
;; End:
