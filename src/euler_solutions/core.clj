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
      (if (<= prime (Math/sqrt maximum))
        (* acc (exp prime (perfectPower maximum prime)))
        (* acc prime)))
    1
    (take-while #(< % maximum) (prime-sieve (iterate inc 2)))))

; What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20
(smallestMultipleOfAllUpTo 20)
