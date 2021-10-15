(ns com.awayfrompeople.snaking.audio
  (:require
    [cljs-audio.webaudio :as wa]
    [cljs-audio.envelopes :refer [adsr! adsr at-time!]]
    [cljs-audio.scheduling :as s]
    [cljs-audio.time :as t]
    [cljs-audio.modules :as m]
    [camel-snake-kebab.core :refer [->camelCase]]
    [cljs.core.async.interop :refer-macros [<p!]]
    [promesa.core :as p]
    ["standardized-audio-context" :as sac :refer (AudioContext)]))

(defn graph [{:keys [events ir]}]
  (let [params (s/schedule-parameters
                 {:frequency at-time! :gain (partial adsr! 0.1 0.15 0.6 1 0.2)}
                 events)
        {:keys [frequency gain]} params]
    [{:voice  (m/simple-voice
                {:frequency frequency
                 :gain      gain
                 :type      "sawtooth"})
      :voice2 (m/simple-voice
                {:frequency frequency
                 :detune    -4
                 :gain      gain
                 :type      "sine"})
      :reverb [:convolver {:buffer ir}]
      :fx     (m/multi-tap-delay
                {:times (mapv m/at-start
                              [(t/seconds 120 "1/4")
                               (t/seconds 120 "1/2")
                               (t/seconds 120 "1")]) :gains (mapv m/at-start [1 0.5 0.25 0.1])})
      :vca    [:gain {:gain 0.3}]
      :comp   [:dynamics-compressor {:threshold -50 :knee 40 :ratio 12 :attack 0 :release 0.25}]
      }
     #{[:voice2 :vca]
       [:vca :reverb]
       [:reverb :comp]
       [:voice :fx]
       [:fx :comp]
       [:comp :>]
       }]))

(def ir (atom nil))

(def events ["touchstart" "touchend" "mousedown" "keydown"])

(def is-initialized (atom false))

(defn update-audio [{:keys [audio freq time]}]
  (let [time (+ 0.1 time)]
    (p/then (wa/calculate-updates @audio (graph {:ir :ir :events [{:frequency freq :gain 1 ::s/time (+ time 0)}
                                                                  {:frequency (* 2 freq) :gain 1 ::s/time (+ time (t/seconds 120 "1/4"))}
                                                                  {:frequency (* 4 freq) :gain 1 ::s/time (+ time (t/seconds 120 "1/4") (t/seconds 120 "1/4"))}]}))
            (fn [updates]
              (let [new-env (wa/eval-updates! @audio updates)]
                (reset! audio (into @audio {:env new-env})))))))


(defn resume-audio-context []
  (let [audio (atom (wa/make-audio {:polyfill sac}))]
    (p/then (wa/resume @audio)
            (fn []
              (p/let [;stream (<p! (js/navigator.mediaDevices.getUserMedia (clj->js {:audio true})))
                      ir-audio-data (wa/fetch-sample @audio "resources/ir.mp3")]
                     (try
                       (reset! audio (into @audio {:buffers {:ir ir-audio-data}}))
                       (doseq [ev events] (.removeEventListener js/document.body ev resume-audio-context))
                       (js/document.body.addEventListener "mousedown"
                                                          (fn [e] (update-audio {:audio audio
                                                                                 :freq  (.-offsetY e)
                                                                                 :time  (wa/current-time @audio)
                                                                                 })))
                       (js/document.body.addEventListener "touchstart"
                                                          (fn [e]
                                                            (let [touch (.item (.-changedTouches e) 0)]
                                                              (when (not (nil? touch))
                                                                (update-audio {:audio audio
                                                                               :freq  (.-offsetY e)
                                                                               :time  (wa/current-time @audio)
                                                                               }))
                                                              ))
                                                          (reset! is-initialized true))
                       (catch js/Error err (js/console.log (ex-cause err)))))))))

(doseq [ev events] (js/document.body.addEventListener ev resume-audio-context false))