(ns euler-solutions.core
  (:gen-class))

;;;
;; Utility Functions 
;;;

(defn factorOf?
  "Is x a factor of y"
  [x y]
  (== (mod y x) 0))

(defn isPalindrome?
  "Is n a palindrome"
  [n]
  (let [testseq (seq (clojure.string/replace (str n) #" " ""))]
    (loop [subseq testseq]
      (if (> (count subseq) 1)
        (if (= (first subseq) (last subseq))
          (recur (drop-last (rest subseq)))
          false)
        true))))

(defn prime-sieve
  [s]
  (cons (first s)
        (lazy-seq (prime-sieve (remove (partial factorOf? (first s)) (rest s)))))) 

(defn exp
  [x n]
  (loop [acc 1
         n n]
    (if (zero? n) acc
      (recur (* x acc) (dec n)))))

;;;
;; 1. Multiples of 3 and 5
;;;
(defn multipleOf3or5?
  "return if n is multiple of 3 or multiple of 5"
  [n]
  (or (factorOf? 3 n) (factorOf? 5 n)))

; Find the sum of all the multiples of 3 or 5 below 1000
(reduce + (filter multipleOf3or5? (range 0 1000)))

;;;
;; 2. Even Fibonacci numbers
;;;
(defn fibonacci
  ([] (fibonacci 1 2))
  ([a b] (lazy-seq (cons a (fibonacci b (+ b a))))))

; Sum the even fibonacci numbers less than 4000000
(reduce + (take-while #(< % 4000000) (filter even? (fibonacci))))

;;;
;; 3. Largest Prime Factor
;;;
(defn primeFactors
  ([numberToFactor]
   (primeFactors numberToFactor 2 []))
  ([numberToFactor index factors]
   (if (<= index numberToFactor)
    (if (factorOf? index numberToFactor)
     (recur (/ numberToFactor index) index (conj factors index))
     (recur numberToFactor (inc index) factors))
    factors)))
  
; Find the largest prime factor of 600851475143 
(apply max (primeFactors 600851475143))

;;;
;; 4. Largest Palindrome Product
;;;
(defn palindromeProductsBetween
  "Get the palindrome products for all numbers between bottom and top"
  [bottom top]
    (filter isPalindrome? 
        (for [x (range bottom top)
              y (range bottom top)]
          (* x y))))

; Find the largest palindrome made from the product of two 3-digit numbers
(apply max (palindromeProductsBetween 100 999))

;;;
;; 5. Smallest Multiple
;;;
(defn perfectPower
  [k p]
  (Math/floor (/ (Math/log k) (Math/log p))))

(defn smallestMultipleOfAllUpTo
  "Get the smallest positive integer that is evently divisible by all numbers from 1 to maximum"
  [maximum]
  (reduce 
    (fn [acc prime]
      (* acc (exp prime (perfectPower maximum prime))))
    1
    (take-while #(< % maximum) (prime-sieve (iterate inc 2)))))

; What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20
(smallestMultipleOfAllUpTo 20)

;;;
;; 6. Sum Square Difference
;;;
(defn sumOfSquares
  [nums]
  (reduce + (map #(* % %) nums)))

(defn squareOfSum
  [nums]
  (exp (reduce + nums) 2))

; Find the difference between the sum of the squares of the first one hundred natural numbers and the square of the sum.
(- (squareOfSum (range 101)) (sumOfSquares (range 101)))

;;;
;; 7. 10001st Prime
;;;

;;;
;; 8. Largest Product in a Series
;;;
(def thousandDigitNumber "731671765313306249192251196744265747423553491949349
                          698352031277450632623957831801698480186947885184385
                          861560789112949495459501737958331952853208805511125
                          406987471585238630507156932909632952274430435576689
                          664895044524452316173185640309871112172238311362229
                          893423380308135336276614282806444486645238749303589
                          072962904915604407723907138105158593079608667017242
                          712188399879790879227492190169972088809377665727333
                          001053367881220235421809751254540594752243525849077
                          116705560136048395864467063244157221553975369781797
                          784617406495514929086256932197846862248283972241375
                          657056057490261407972968652414535100474821663704844
                          031998900088952434506585412275886668811642717147992
                          444292823086346567481391912316282458617866458359124
                          566529476545682848912883142607690042242190226710556
                          263211111093705442175069416589604080719840385096245
                          544436298123098787992724428490918884580156166097919
                          133875499200524063689912560717606058861164671094050
                          775410022569831552000559357297257163626956188267042
                          8252483600823257530420752963450")

; Find the thirteen adjacent digits in the 1000-digit number that have the greatest product. 
; What is the value of this product?
(defn adjacentProducts
  [inputString digitLength]
  (reduce 
    (fn [acc digits]
      (conj acc (reduce * digits)))
    []
    (partition digitLength 1 (map #(Character/digit % 10) inputString))))

(apply max (adjacentProducts thousandDigitNumber 13))
