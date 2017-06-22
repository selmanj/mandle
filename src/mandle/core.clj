(ns mandle.core
  (:gen-class)
  (:require [clojure.string :as str]))

(def color-to-ansi-code
  {:black   30
   :red     31
   :green   32
   :yellow  33
   :blue    34
   :magenta 35
   :cyan    36
   :white   37
   :normal  0})

(defn format-color [fmt & args]
  "Like format, but also expands inline ansi colors. Wrap the color name with
  ${} (eg. '${green} prints green). Note that the color is always reset at the
  end of the string."
  (let [formatted (format fmt args)
        color-replace-fns (map #(fn [s]
                                  (str/replace s
                                               (format "${%s}" (name %1))
                                               (format "\033[%dm" (get color-to-ansi-code %1))))
                               (keys color-to-ansi-code))
        replace-all-colors-fn (apply comp color-replace-fns)]
    (str (replace-all-colors-fn formatted) (format "\033[%dm" (:normal color-to-ansi-code)))) )

(defn square [x]
  "Returns x squared (e.g. (* x x))."
  (* x x))

(defn mandlebrot [[a0 b0] [a b]]
  "Represents one iteration of the mandlebrot set-defining function. a is the
  real part of the complex number, and b is the imaginary part. Returns a vector
  with a real component followed by a complex."
  [(+ (- (square a)
         (square b))
      a0)
   (+ (* 2 a b)
      b0)])

(defn mandlebrot-seq [c]
  "Given a complex number (represented as a two-element seq), returns a lazy seq of all iterations of the mandlebrot set-defining function."
  (iterate (partial mandlebrot c) [0 0]))

(defn escaped? [c]
  "Check whether or not a complex number is considered 'escaped' (e.g. the number is not part of the mandlebrot set)."
  (let [[a b] c]
    (>= (+ (square a)
           (square b))
        (square 2))))

(defn mandle-escape-iters [c max-iterations]
  "Given a complex number (represented as a two-element seq), returns the min of
  mandlebrot set-defining function applications applied until escape, or
  max-iterations."
  (let [mandlebrot-seq-idxed (map vector
                                  (range 0 (inc max-iterations))
                                  (mandlebrot-seq c))]
    (or (ffirst (drop-while (fn [[idx c]]
                              (not (escaped? c)))
                            mandlebrot-seq-idxed))
        max-iterations)))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
