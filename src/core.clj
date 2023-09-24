(ns core
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn binary [n] (Integer/toString n 2))

(defn hex [n] (Integer/toString n 16))

;; decoder

;; https://edge.edx.org/c4x/BITSPilani/EEE231/asset/8086_family_Users_Manual_1_.pdf

(def w0
  {2r000 :al
   2r001 :cl
   2r010 :dl
   2r011 :bl
   2r100 :ah
   2r101 :ch
   2r110 :dh
   2r111 :bh})

(def w1
  {2r000 :ax
   2r001 :cx
   2r010 :dx
   2r011 :bx
   2r100 :sp
   2r101 :bp
   2r110 :si
   2r111 :di})

(defmacro locals [& xs] (zipmap (map keyword xs) xs))

(defn decode [byte byte-stream]
  (let [byte-1 byte, byte-2 (.read byte-stream)]
    (cond
      ;; register/memory to/from register
      (= (bit-and byte-1 2r11111100) 2r10001000)
      (let [d?
            (not (zero? (bit-and byte-1 2r00000010)))

            w?
            (not (zero? (bit-and byte-1 2r00000001)))

            _mod
            (-> byte-2
                (bit-and 2r11000000)
                (bit-shift-right 6))

            reg
            (-> byte-2
                (bit-and 2r00111000)
                (bit-shift-right 3))

            rm
            (bit-and byte-2 2r00000111)

            ->reg (if w? w1 w0)

            [src dst]
            (map ->reg (if d? [rm reg] [reg rm]))]
        (format "mov %s, %s" (name dst) (name src))))))

(defn decode-file [f]
  (with-open [byte-stream (io/input-stream f)]
    (loop [instructions []]
      (let [byte (.read byte-stream)]
        (if (= byte -1)
          instructions
          (recur (conj instructions (decode byte byte-stream))))))))

(comment
  (decode-file "support/one-instruction")
  (decode-file "support/multiple-instructions")

  #_#_(= (bit-and byte-1 2r11110000) 2r10110000)
  (let [w?
        (zero? (bit-and byte-1 2r00000001))

        reg
        (bit-and byte-2 2r00000111)

        ]
    #_(locals src dst))
  nil)
