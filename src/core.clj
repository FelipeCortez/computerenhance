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

(defn decode [byte byte-stream]
  (let [byte-1 byte
        *bytes-read (volatile! 1)

        next-byte!
        (fn []
          (vswap! *bytes-read inc)
          (.read byte-stream))

        byte-2 (next-byte!)]
    [(cond
       ;; conditional jumps
       (= byte-1 2r01110100)
       [:je byte-2]

       (= byte-1 2r01111100)
       [:jl  byte-2]

       (= byte-1 2r01111110)
       [:jle byte-2]

       (= byte-1 2r01110010)
       [:jb  byte-2]

       (= byte-1 2r01110110)
       [:jbe  byte-2]

       (= byte-1 2r01111010)
       [:jp  byte-2]

       (= byte-1 2r01110000)
       [:jo  byte-2]

       (= byte-1 2r01111000)
       [:js  byte-2]

       (= byte-1 2r01110101)
       [:jne  byte-2]

       (= byte-1 2r01111101)
       [:jnl  byte-2]

       (= byte-1 2r01111111)
       [:jnle  byte-2]

       (= byte-1 2r01110011)
       [:jnb  byte-2]

       (= byte-1 2r01110111)
       [:jnbe  byte-2]

       (= byte-1 2r01111011)
       [:jnp  byte-2]

       (= byte-1 2r01110001)
       [:jno  byte-2]

       (= byte-1 2r01111001)
       [:jns  byte-2]

       (= byte-1 2r11100010)
       [:loop  byte-2]

       (= byte-1 2r11100001)
       [:loopz  byte-2]

       (= byte-1 2r11100000)
       [:loopnz  byte-2]

       (= byte-1 2r11100011)
       [:jcxz  byte-2]

       ;; register/memory to/from register
       (or (= (bit-and byte-1 2r11111100) 2r10001000)
           (= (bit-and byte-1 2r11111100) 2r00000000)
           (= (bit-and byte-1 2r11111100) 2r00101000)
           (= (bit-and byte-1 2r11111100) 2r00111000))
       (let [op
             (-> byte-1
                 (bit-and 2r00111000)
                 (bit-shift-right 3)
                 (opcode->op))

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
               (mapv ->reg [reg rm])

               2r00
               [(->reg reg)
                (if (= rm 2r110)
                  (bit-or (next-byte!)
                          (bit-shift-left (next-byte!) 8))
                  (into [:+] (rm->regs rm)))]

               2r01
               (let [disp (next-byte!)]
                 [(->reg reg)
                  (into [:+]
                        (cond-> (rm->regs rm)
                          (not (zero? disp)) (conj disp)))])

               2r10
               (let [disp (bit-or (next-byte!)
                                  (bit-shift-left (next-byte!) 8))]
                 [(->reg reg)
                  (into [:+]
                        (cond-> (rm->regs rm)
                          (not (zero? disp)) (conj disp)))]))

             [src dst]
             (if d? [two one] [one two])]
         [op dst src])

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
               (bit-or (bit-shift-left (next-byte!) 8)
                       byte-2)
               byte-2)]
         [:mov (->reg reg) data])

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
                 opcode->op)

             rm
             (bit-and byte-2 2r00000111)

             ->reg
             (if w? w1 w0)

             src
             (case mod
               2r11
               (->reg rm)

               2r00
               (if (= rm 2r110)
                 [(bit-or (next-byte!) (bit-shift-left (.read byte-stream) 8))]
                 (into [:+] (rm->regs rm)))

               2r01
               (let [disp (next-byte!)]
                 (cond-> (rm->regs rm)
                   (not (zero? disp)) (conj disp)))

               2r10
               (let [disp (bit-or (next-byte!)
                                  (bit-shift-left (next-byte!) 8))]
                 (into [:+]
                       (cond-> (rm->regs rm)
                         (not (zero? disp)) (conj disp)))))


             src
             (if (= 2r11 mod)
               src
               [(if w? :word :byte) src])


             data-byte-5
             (next-byte!)

             data
             (if (and (not s?) w?)
               (bit-or (bit-shift-left (next-byte!) 8)
                       data-byte-5)
               data-byte-5)]
         [op src data])

       ;; immediate from accumulator
       (= (bit-and byte-1 2r00000110) 2r00000100)
       (let [w?
             (not (zero? (bit-and byte-1 2r00000001)))

             op
             (-> byte-1
                 (bit-and 2r00111000)
                 (bit-shift-right 3)
                 opcode->op)

             dst
             (if w? :ax :al)

             data
             (if w?
               (bit-or (bit-shift-left (next-byte!) 8)
                       byte-2)
               byte-2)]
         [op dst data])

       :else [(binary byte-1) (binary byte-2)])
     @*bytes-read]))

(defn decode-file [f]
  (with-open [byte-stream (io/input-stream f)]
    (loop [instructions [], bytes-so-far 0]
      (let [byte (.read byte-stream)]
        (if (= byte -1)
          instructions
          (let [[instruction bytes-read] (decode byte byte-stream)]
            (recur (conj instructions [bytes-so-far [instruction bytes-read]])
                   (+ bytes-so-far bytes-read))))))))

(defn instructions->s [instructions] (str/join "\n" instructions))

(def registers (vals w1))

(defn signed-16? [n]
  (not (zero? (bit-and n #=(bit-shift-left 1 15)))))

(defn simulate [instructions]
  (let [ip->instr (into {} instructions)
        max-ip (reduce max (keys ip->instr))]
    (loop [computer (assoc (zipmap registers (repeat 0)) :ip 0)]
      (if (> (:ip computer) max-ip)
        computer
        (let [[[op dst src] instr-bytes] (ip->instr (:ip computer))
              computer (update computer :ip + instr-bytes)]
          (recur
           (case op
             :mov
             (if (number? src)
               (assoc computer dst src)
               (assoc computer dst (get computer src)))

             :add
             (let [result
                   (if (number? src)
                     (+ (get computer dst) src)
                     (+ (get computer dst) (get computer src)))

                   flags
                   {:s? (signed-16? result)
                    :z? (zero? result)}]
               (-> computer
                   (assoc dst result)
                   (assoc :flags flags)))


             :sub
             (let [result
                   (if (number? src)
                     (- (get computer dst) src)
                     (- (get computer dst) (get computer src)))

                   flags
                   {:s? (signed-16? result)
                    :z? (zero? result)}]
               (-> computer
                   (assoc dst result)
                   (assoc :flags flags)))

             :cmp
             (let [result
                   (if (number? src)
                     (- (get computer dst) src)
                     (- (get computer dst) (get computer src)))

                   flags
                   {:s? (signed-16? result)
                    :z? (zero? result)}]
               (assoc computer :flags flags))

             :jne
             (if (:z? (:flags computer))
               computer
               (update computer :ip + (unchecked-byte dst))))))))))

(comment
  (with-meta
    (simulate (decode-file "support/conditional-jump"))
    {:portal.viewer/default :portal.viewer/table})
  )
