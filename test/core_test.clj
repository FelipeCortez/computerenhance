(ns core-test
  (:require [core :as sut]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]))

(defn instructions [f]
  (->> f
       slurp
       str/split-lines
       (filter #(str/starts-with? % "mov"))))

(deftest one []
  (is (= (instructions "support/one-instruction.asm")
         (sut/decode-file "support/one-instruction"))))

(deftest multiple []
  (is (= (instructions "support/multiple-instructions.asm")
         (sut/decode-file "support/multiple-instructions"))))
