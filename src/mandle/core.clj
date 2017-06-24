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
  "Given a complex number (represented as a two-element seq), returns a lazy seq
  of all iterations of the mandlebrot set-defining function."
  (iterate (partial mandlebrot c) [0 0]))

(defn escaped? [c]
  "Check whether or not a complex number is considered 'escaped' (e.g. the
  number is not part of the mandlebrot set)."
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

(defn interpolate [a b n]
  "Given two numbers a and b, return n number of points interpolating between a
  and b (inclusive)."
  (if (< n 2)
    (throw (ex-info "Can't interpolate less than two points"
                    {:n n}))
    (let [step (double (/ (- b a)
                          (dec n)))]
      (conj (into [] (take (dec n) (range a b step))) b))))

(defn interpolate-plane
  [[min_a max_a] [min_b max_b] n_a n_b]
  "Given two min/max sets of values and two counts, interpolate a
  plane (inclusive)."
  (for [b_p (interpolate min_b max_b n_b)
        a_p (interpolate min_a max_a n_a)]
    [a_p b_p]))

(defn iteration-color [cs max v]
  "Find the color of an iteration."
  ;; TODO this used to be used for linear interpolation but should probably
  ;; reflect what is actually being done via a histogram.
  (let [idx (int (* (dec (count cs))
                    (/ v max)))]
    (nth cs idx)))

(def mandle-color-order
  "Simple order of colors to render, from small num of iterations to large."
  [:blue :cyan :green :magenta :red :yellow :white])

(defn print-mandlebrot
  [[min_x max_x] [min_y max_y] n_x n_y max-iters]
  ;; TODO this code got away from me! needs cleaning up
  (doseq [chars (partition n_x (let [iters (map #(mandle-escape-iters % max-iters)
                                                (interpolate-plane [min_x max_x] [min_y max_y] n_x n_y))
                                     hist (dissoc (frequencies iters) max-iters)
                                     total (reduce + (vals hist))]
                                 (for [iter iters]
                                   (let [hist-sum (->> hist
                                                       keys
                                                       (filter #(< % iter))
                                                       (map hist)
                                                       (reduce +))
                                         color (if (= iter max-iters)
                                                 "black"
                                                 (name (iteration-color mandle-color-order total hist-sum)))]
                                     (format-color (str "${" color "}#"))))))]
    (println (apply str chars))))

(defn -main
  [& args]
  (print-mandlebrot [-2.5 1] [-1 1] 160 40 1000))
