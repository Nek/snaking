(ns com.awayfrompeople.snaking.app
  (:require
   [thi.ng.math.core :as m]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.vector :as v :refer [vec2 vec3]]
   [thi.ng.geom.matrix :as mat :refer [M32 M44]]
   [thi.ng.geom.circle :as c]
   [thi.ng.geom.rect :as r]
   [thi.ng.geom.triangle :as t]
   [thi.ng.geom.spatialtree :as accel]
   [thi.ng.geom.svg.core :as svg]
   [thi.ng.geom.physics.core :as phys]
   [thi.ng.geom.gl.webgl.animator :refer [animate]]
   [thi.ng.domus.core :as dom]
   [com.awayfrompeople.snaking.audio :refer [resume-audio-context events]]
   [clojure.test.check.random :as rnd]))

(def rnd (atom (rnd/make-random 1)))
(defn next-random! [] (let [res (rnd/rand-double @rnd)]
                        (swap! rnd #(first (rnd/split %)))
                        res))

(defn choose [prng v] (nth v (Math/floor (* (count v) (prng)))))

(defn attract!
  "Takes a 2d or 3d attractor position, a particle and attractor
  params (squared radius, strength, time delta). If particle is within
  attraction radius, applies proportional strength force to particle.
  If strength is negative, particle will be repelled."
  [p q rsq strength delta]
  (let [d (m/- p (phys/position q))
        l (+ (m/mag-squared d) 1e-6)]
    (if (< l rsq)
      (phys/add-force
       q (m/* d (/ (* (- 1.0 (/ l rsq)) (* strength delta))
                   (Math/sqrt l)))))))

(defn accelerated-force-field
  "Takes a mutable quadtree or octree, an attraction radius and strength.
  Returns a function which will be applied as behavior to all
  particles to create a force field around each. The spatial tree is
  used to limit k-neighbor lookups to only particles within the given
  radius around each particle."
  [accel r strength]
  (let [rsq (* r r)]
    (fn [p delta]
      (let [p' (phys/position p)]
        (loop [neighbors (accel/select-with-circle accel p' r)]
          (when-let [n (first neighbors)]
            (if-not (= p n) (attract! p' n rsq strength delta))
            (recur (next neighbors))))))))

(defn update-accelerator
  "Takes a mutable quadtree or octree and returns a function to be
  used as simulation listener. When called, updates the tree to
  reflect current positions of all particles in the physics sim."
  [accel]
  (fn [physics _]
    (reduce
     #(g/add-point % (phys/position %2) %2)
     (g/clear! accel)
     (:particles physics))))

(defn make-strand
  "Creates a strand of spring-connected 2d particles in a zigzag order
  along the Y axis. The first arg defines the total number of
  particles in the strand, the second how many particles per row. The
  last arg defines the 2d start position. Returns 2-elem vector
  of [particles springs]."
  [n fold offset]
  (let [particles (->> (range n)
                       (partition-all fold)
                       (map-indexed
                        (fn [i p]
                          (let [o (* i fold)]
                            (map #(m/+ offset (- % o) i) (if (odd? i) (reverse p) p))))))
        particles (mapv phys/particle (mapcat identity particles))
        springs   (map (fn [[a b]] (phys/spring a b 0.5 1)) (partition 2 1 particles))]
    [particles springs]))

(defn init-physics
  "Takes a state map and integer defining number of particles per
  strand. First creates two strands, each with its own circular
  constraint. Then defines full VerletPhysics setup with gravity and
  force field behaviors. Also attaches a simulation listener to keep
  particle quadtree in sync. Returns updated state map w/ physics
  related data injected."
  [state n1 n2 n3]
  (let [[p1 s1] (make-strand n1 6 (vec2 7 2))
        [p2 s2] (make-strand n2 6 (vec2 2 2))
        [p3 s3] (make-strand n3 6 (vec2 5 2))
        c1      {:c (phys/shape-constraint-inside (c/circle (vec2 10 10) 9))}
        c2      {:c (phys/shape-constraint-inside (r/rect (vec2 5 1) 7))}
        c3      {:c (phys/shape-constraint-inside (t/triangle2 (vec2 8 6) (vec2 11 13) (vec2 5 13)))}
        accel   (accel/quadtree 0 0 26)]
    (doseq [p p1] (phys/add-constraints p c1))
    (doseq [p p2] (phys/add-constraints p c2))
    (doseq [p p3] (phys/add-constraints p c3))
    (assoc state
           :physics  (phys/physics
                      {:particles (concat p1 p2 p3)
                       :springs   (concat s1 s2 s3)
                       :behaviors {:g (phys/gravity (:gravity state))
                                   :f (accelerated-force-field accel 1 -1)}
                       :listeners {:iter (update-accelerator accel)}})
           :clusters [p1 p2 p3])))

(defn particle-positions
  "Takes a seq of particles, returns vector of their positions."
  [particles]
  (mapv #(phys/position %) particles))

(defn svg-strand
  "Takes a seq of particles and stroke/fill colors, returns a SVG
  group defining particles as circles and a polyline between
  particles."
  [particles stroke fill]
  (let [pos (map (fn [[x y]] [(* 2 x) (* 2 y)]) (particle-positions particles))]
    (svg/group
     {:stroke stroke}
     (svg/line-strip pos {:fill "none" :stroke-width 1 :stroke-linecap "round" :stroke-linejoin "round"})
     #_(svg/group
        {:fill fill :stroke "none"}
        (sequence (map #(svg/circle % 0.2)) pos)))))

(defn visualize-svg
  "Takes a state map and visualizes the current state of the physic
  sim as SVG DOM element."
  [{:keys [physics root svg-attrs] :as state}]
  (let [[c1 c2 c3] (:clusters state)]
    (->> root
         (dom/clear!)
         (dom/create-dom!
          (svg/svg
           {:width 600
            :height 600
            :view-box "0 0 600 600"
            :preserve-aspect-ratio "xMidYMid meet"}
           (svg/group
            svg-attrs
            (svg-strand c1 "#00f" "#0ff")
            (svg-strand c2 "#0f0" "#ff0")
            (svg-strand c3 "#0ff" "#f00")))))))

(defn update-gravity [state g]
  (swap! state assoc :gravity g)
  (swap! state update :physics phys/add-behaviors {:g (phys/gravity (:gravity @state))}))

(defn fit-range [[a b] [c d] v]
  (+ (/ (* (- v a) (- d c)) (- b a)) c))

(defn fit-y-angle [v] (if (> v 0) (Math/min v 179) 1))

(when (not (undefined? (.-DeviceOrientationEvent js/window)))
  (let [doe (.-DeviceOrientationEvent js/window)]
    (when (.hasOwnProperty doe "requestPermission")
      (.addEventListener js/document "touchend"
                         (fn [_]
                           (.then (.requestPermission doe)
                                  #(when (= %1 "granted") (println "Can gravitate!"))))))))

(defn to-radians [v] (/ (* v Math/PI) 180))

(defn init
  []
  (let [state (-> {:root (dom/by-id "app")
                   :gravity (vec2 0 0)
                   :svg-attrs {:transform (g/scale M32 15) :stroke-width 0.1}}
                  (init-physics 100 50 25)
                  (atom))
        update-app-from-mouse (fn [e]
                                (let [y (.-offsetY e)
                                      vert (fit-range [0 (.-clientHeight js/document.body)] [-0.5 0.5] y)]
                                  #_(update-audio (fit-range [0 (.-clientHeight js/document.body)] [220 110] y))
                                  (update-gravity state (vec2 0 vert))))
        update-app-from-orientation (fn [e]
                                      (let [beta (fit-y-angle (- (- 180 (.-beta e)) 90))
                                            y (Math/cos (to-radians beta))
                                            vert (fit-range [-1 1] [-0.5 0.5] y)]
                                        #_(update-audio (fit-range [-1 1] [440 220] y))
                                        (update-gravity state (vec2 0 vert))))]
    (.addEventListener js/window "deviceorientation" update-app-from-orientation false)
    (.addEventListener js/window "mousemove" update-app-from-mouse false)
    (doseq [ev events] (.addEventListener js/document.body ev resume-audio-context false))

    (animate
     (fn [t frame]
       (phys/timestep (:physics @state) 2)
       (visualize-svg @state)
       true))))


