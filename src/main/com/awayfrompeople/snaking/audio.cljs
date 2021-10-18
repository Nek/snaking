(ns com.awayfrompeople.snaking.audio
  (:require
    [cljs-audio.webaudio :as wa]
    [cljs-audio.envelopes :refer [l-adsr! at-time!]]
    [cljs-audio.scheduling :as s]
    [cljs-audio.time :as t]
    [cljs-audio.modules :as m]
    [camel-snake-kebab.core :refer [->camelCase]]
    [cljs.core.async.interop :refer-macros [<p!]]
    [promesa.core :as p]
    ["standardized-audio-context" :as sac :refer (AudioContext)]))

(defn graph []
  [{:voice  (m/simple-voice
              {:frequency 0
               :gain      0
               :type      "sawtooth"})
    :voice2 (m/simple-voice
              {:frequency 0
               :detune    -4
               :gain      0
               :type      "sine"})
    :reverb [:convolver {:buffer :ir}]
    :fx     (m/multi-tap-delay
              {:times (mapv m/at-start
                            [(t/seconds 120 "1/4")
                             (t/seconds 120 "1/2")]) :gains (mapv m/at-start [0.5 0.25 0.1])})
    :vca    [:gain {:gain 0.3}]
    :comp   [:dynamics-compressor {:threshold -50 :knee 40 :ratio 12 :attack 0 :release 0.25}]
    }
   #{[:voice2 :vca]
     [:voice :vca]
     [:vca :reverb]
     [:reverb :comp]
     [:comp :>]}
   #_#{[:frequency [:voice :osc :frequency]]
       [:frequency [:voice2 :osc :frequency]]
       [:gain [:voice :vca :gain]]
       [:gain [:voice2 :vca :gain]]}])

(def ir (atom nil))

(def events ["touchstart" "touchend" "mousedown" "keydown"])

(def bpm 120)

(defn schedule-with-routes! [audio routes path commands]
  (doseq [ref (filter #(= (first %) path) routes)]
    (wa/schedule! audio (second ref) commands)))

(def routes #{[[:frequency] [:voice :osc :frequency]]
              [[:frequency] [:voice2 :osc :frequency]]
              [[:gain] [:voice :vca :gain]]
              [[:gain] [:voice2 :vca :gain]]})

(defn schedule-events [{:keys [audio freq]}]
  (let [current-time (wa/current-time audio)
        quarter (t/seconds bpm "1/4")
        notes [{:pitch 220 :velocity 0.2 :length quarter}
               {:pitch 440 :velocity 0.1 :length quarter}
               {:length quarter}
               {:pitch 236 :velocity 0.3 :length quarter}
               {:pitch 330 :velocity 0.1 :length quarter}]
        note->event (fn [current-time {:keys [pitch velocity length] :as note}]
                      (let [scheduled-time (+ current-time length)]
                        (if (= 1 (count note))
                          [scheduled-time nil]
                          [scheduled-time
                           [{:frequency pitch
                            :gain      velocity
                            ::s/time   scheduled-time}]])))
        [_ events] (reduce (fn [[current-time events] note]
                             (let [[scheduled-time event] (note->event current-time note)]
                               [scheduled-time (into events event)]))
                           [current-time []]
                           notes)
        schedule (s/schedule-parameters
                   {:frequency at-time! :gain (partial l-adsr! 0.03 0.15 0.7 0.1 0.2)}
                   events)]
    (doseq [param-name (keys schedule)]
      (schedule-with-routes! audio routes [param-name] (param-name schedule)))))

(defn resume-audio-context []
  (let [audio (atom (wa/make-audio {:polyfill sac}))]
    (p/then (wa/resume @audio)
            (fn []
              (p/let [;stream (<p! (js/navigator.mediaDevices.getUserMedia (clj->js {:audio true})))
                      ir-audio-data (wa/fetch-sample @audio "resources/ir.mp3")]
                     (try
                       (reset! audio (into @audio {:buffers {:ir ir-audio-data}}))
                       (doseq [ev events] (.removeEventListener js/document.body ev resume-audio-context))
                       (p/then (wa/calculate-updates @audio (graph))
                               (fn [updates]
                                 (reset! audio (wa/eval-updates! @audio updates))
                                 (js/document.body.addEventListener "mousedown"
                                                                    (fn [e]
                                                                      (schedule-events {:audio @audio
                                                                                        :freq  (.-offsetY e)})))
                                 (js/document.body.addEventListener "touchstart"
                                                                    (fn [e]
                                                                      (let [touch (.item (.-changedTouches e) 0)]
                                                                        (when (not (nil? touch))
                                                                          (schedule-events {:audio @audio
                                                                                            :freq  (.-offsetY e)}))
                                                                        )))))
                       (catch js/Error err (js/console.log (ex-cause err)))))))))

(doseq [ev events] (js/document.body.addEventListener ev resume-audio-context false))