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
                             (t/seconds 120 "1/2")
                             (t/seconds 120 "1")]) :gains (mapv m/at-start [1 0.5 0.25 0.1])})
    :vca    [:gain {:gain 0.3}]
    :comp   [:dynamics-compressor {:threshold -50 :knee 40 :ratio 12 :attack 0 :release 0.25}]
    }
   #{[:voice2 :vca]
     ;[:vca :reverb]
     [:vca :comp]
     [:voice :fx]
     [:fx :comp]
     [:comp :>]
     }
   #_#{[:frequency [:voice :osc :frequency]]
     [:frequency [:voice2 :osc :frequency]]
     [:gain [:voice :vca :gain]]
     [:gain [:voice2 :vca :gain]]}])

(def ir (atom nil))

(def events ["touchstart" "touchend" "mousedown" "keydown"])

(defn schedule-events [{:keys [audio freq]}]
  (let [time (wa/current-time audio)
        {:keys [frequency gain]}
        (s/schedule-parameters
          {:frequency at-time! :gain (partial l-adsr! 0.1 0.15 0.6 1 0.2)}
          [{:frequency freq :gain 1 ::s/time (+ time 0)}
           {:frequency (* 2 freq) :gain 1 ::s/time (+ time (t/seconds 120 "1/4"))}
           {:frequency (* 4 freq) :gain 1 ::s/time (+ time (t/seconds 120 "1/4") (t/seconds 120 "1/4"))}])]
    (wa/schedule! audio [:voice :osc :frequency] frequency)
    (wa/schedule! audio [:voice :vca :gain] gain)
    (wa/schedule! audio [:voice2 :osc :frequency] frequency)
    (wa/schedule! audio [:voice2 :vca :gain] gain)))

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