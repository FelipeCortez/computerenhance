(ns core
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn binary [n] (Integer/toString n 2))

(defn hex [n] (Integer/toString n 16))

;; decoder

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

(defn decode [instruction]
  (let [byte-1
        (bit-shift-right instruction 8)

        byte-2
        (bit-and instruction 2r11111111)

        opcode
        (case (bit-and byte-1 2r11111100)
          2r10001000 :reg-reg)

        d
        (bit-and byte-1 2r00000010)

        w
        (bit-and byte-1 2r00000001)

        mod
        (-> byte-2
            (bit-and 2r11000000)
            (bit-shift-right 6))

        reg
        (-> byte-2
            (bit-and 2r00111000)
            (bit-shift-right 3))

        rm
        (bit-and byte-2 2r00000111)

        ->reg (if (zero? w) w0 w1)

        [src dst]
        (map ->reg
             (if (zero? d) [reg rm] [rm reg]))]
    {:opcode opcode
     :d d
     :w w
     :mod mod
     :reg reg
     :r/m rm
     :src src
     :dst dst}))

(defn read-instructions [f]
  (with-open [r (io/input-stream f)]
    (loop [instructions []]
      (let [b (.read r)]
        (if (= b -1)
          instructions
          (recur (conj instructions (bit-or (bit-shift-left b 8)
                                            (.read r)))))))))


(defn decode-file [f]
  (str/join "\n"
            (map (comp (fn [{:keys [src dst]}] (format "mov %s, %s"
                                                       (name dst)
                                                       (name src)))
                       decode)
                 (read-instructions f))))

(comment
  (decode-file "../support/one-instruction")
  (decode-file "../support/multiple-instructions")
  nil)
