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

(def rm->regs
  {2r000 [:bx :si]
   2r001 [:bx :di]
   2r010 [:bp :si]
   2r011 [:bp :di]
   2r100 [:si]
   2r101 [:di]
   2r110 [:bp]
   2r111 [:bx]})

(def opcode->op
  {2r001 :mov
   2r000 :add
   2r101 :sub
   2r111 :cmp})

(defmacro locals [& xs] (zipmap (map keyword xs) xs))

(defn decode [byte byte-stream]
  (let [byte-1 byte, byte-2 (.read byte-stream)]
    (cond
      ;; register/memory to/from register
      (or (= (bit-and byte-1 2r11111100) 2r10001000)
          (= (bit-and byte-1 2r11111100) 2r00000000)
          (= (bit-and byte-1 2r11111100) 2r00101000)
          (= (bit-and byte-1 2r11111100) 2r00111000))
      (let [op
            (-> byte-1
                (bit-and 2r00111000)
                (bit-shift-right 3)
                (opcode->op)
                name)

            d?
            (not (zero? (bit-and byte-1 2r00000010)))

            w?
            (not (zero? (bit-and byte-1 2r00000001)))

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

            ->reg
            (if w? w1 w0)

            [one two]
            (case mod
              2r11
              (mapv (comp name ->reg) [reg rm])

              2r00
              [(name  (->reg reg))
               (if (= rm 2r110)
                 (str "["
                      (bit-or (.read byte-stream)
                              (bit-shift-left (.read byte-stream) 8))
                      "]")
                 (str "[" (str/join " + " (mapv name (rm->regs rm))) "]"))]

              2r01
              (let [disp (.read byte-stream)]
                [(name (->reg reg))
                 (str "["
                      (str/join " + " (cond-> (mapv name (rm->regs rm))
                                        (not (zero? disp)) (conj disp)))
                      "]")])

              2r10
              (let [disp (bit-or (.read byte-stream)
                                 (bit-shift-left (.read byte-stream) 8))]
                [(name (->reg reg))
                 (str "["
                      (str/join " + " (cond-> (mapv name (rm->regs rm))
                                        (not (zero? disp)) (conj disp)))
                      "]")]))

            [src dst]
            (if d? [two one] [one two])]
        (format "%s %s, %s" op dst src))

      ;; mov immediate to register
      (= (bit-and byte-1 2r11110000) 2r10110000)
      (let [w?
            (not (zero? (bit-and byte-1 2r00001000)))

            reg
            (bit-and byte-1 2r00000111)

            ->reg
            (if w? w1 w0)

            data
            (if w?
              (bit-or (bit-shift-left (.read byte-stream) 8)
                      byte-2)
              byte-2)]
        (format "mov %s, %s" (-> reg ->reg name) data))

      ;; immediate to register/memory
      (= (bit-and byte-1 2r11111100) 2r10000000)
      (let [s?
            (not (zero? (bit-and byte-1 2r00000010)))

            w?
            (not (zero? (bit-and byte-1 2r00000001)))

            mod
            (-> byte-2
                (bit-and 2r11000000)
                (bit-shift-right 6))

            op
            (-> byte-2
                (bit-and 2r00111000)
                (bit-shift-right 3)
                opcode->op
                name)

            rm
            (bit-and byte-2 2r00000111)

            ->reg
            (if w? w1 w0)

            bw-str
            (if (= 2r11 mod)
              ""
              (if w? "word" "byte"))

            src
            (case mod
              2r11
              ((comp name ->reg) rm)

              2r00
              (if (= rm 2r110)
                (str "["
                     (bit-or (.read byte-stream)
                             (bit-shift-left (.read byte-stream) 8))
                     "]")
                (str "[" (str/join " + " (mapv name (rm->regs rm))) "]"))

              2r01
              (let [disp (.read byte-stream)]
                (str "["
                     (str/join " + " (cond-> (mapv name (rm->regs rm))
                                       (not (zero? disp)) (conj disp)))
                     "]"))

              2r10
              (let [disp (bit-or (.read byte-stream)
                                 (bit-shift-left (.read byte-stream) 8))]
                (str "["
                     (str/join " + " (cond-> (mapv name (rm->regs rm))
                                       (not (zero? disp)) (conj disp)))
                     "]")))

            data-byte-5
            (.read byte-stream)

            data
            data-byte-5
            #_(if w?
              (bit-or (bit-shift-left (.read byte-stream) 8)
                      data-byte-5)
              data-byte-5)]
        (format "%s %s %s, %s" op bw-str src data))

      ;; immediate from accumulator
      (= (bit-and byte-1 2r00000110) 2r00000100)
      (let [w?
            (not (zero? (bit-and byte-1 2r00000001)))

            op
            (-> byte-1
                (bit-and 2r00111000)
                (bit-shift-right 3)
                opcode->op
                #_name)

            dst
            (if w? "ax" "al")

            data
            (if w?
              (bit-or (bit-shift-left (.read byte-stream) 8)
                      byte-2)
              byte-2)]
        (format "%s %s, %s" op dst data))

      :else [(Integer/toBinaryString byte-1)
             (Integer/toBinaryString byte-2)])))

(defn decode-file [f]
  (with-open [byte-stream (io/input-stream f)]
    (loop [instructions []]
      (let [byte (.read byte-stream)]
        (if (= byte -1)
          instructions
          (recur (conj instructions (decode byte byte-stream))))))))

(defn instructions->s [instructions] (str/join "\n" instructions))


(comment
  (with-meta
    (map vector
         (decode-file "support/add-sub-cmp-jnz")
         (core-test/instructions "support/add-sub-cmp-jnz.asm"))
    {:portal.viewer/default :portal.viewer/table})
  )
