(ns cuadricula.core
  (:require [quil.core :as q]
            [quil.middleware :as m]
    ; [genartlib.algebra :refer :all]
    ; [genartlib.curves :refer :all]
    ; [genartlib.geometry :refer :all]
    ; [genartlib.random :refer :all]
            [genartlib.util :refer [set-color-mode w h]]
            [quil.core :as q]
            [quil.helpers.seqs :refer [range-incl]]
            ))

(declare actual-draw)

(defn draw-state [state]
  ; disable animation, just one frame
  (q/no-loop)

  ; set color space to HSB with hue in [0, 360], saturation in [0, 100],
  ; brightness in [0, 100], and alpha in [0.0, 1.0]
  (set-color-mode)

  ; Calculate x and y coordinates of the circle.
  ; make it easy to generate multiple images
  (doseq [img-num (range 1)]
    (let [cur-time (System/currentTimeMillis)
          seed (System/nanoTime)]

      (println "setting seed to:" seed)
      (q/random-seed seed)

      (try
        (actual-draw (:rh state) (:rw state) (:det state) (:colors state))
        (catch Throwable t
          (println "Exception in draw function:" t)))

      (println "gen time:" (/ (- (System/currentTimeMillis) cur-time) 1000.0) "s")
      (let [img-filename (str "img-" img-num "-" cur-time "-" seed ".tif")]
        (q/save img-filename)
        (println "done saving" img-filename)

        ; (sh "bash" "-c" (str "convert -compress lzw " img-filename " " img-filename))
        ; (println "done compressing")
        ))))


(defn get-fill-color [v, colors]
  (let [value (mod (q/abs v) (count colors))
        idx1 (int (mod value (count colors)))
        idx2 (int (mod (inc value) (count colors)))
        c1 (get colors idx1)
        c2 (get colors idx2)
        ]
    (q/lerp-color c1 c2 (mod value 1))
    )
  )

(defn actual-draw [rh rw det colors]
  (q/background 0)
  (q/stroke 0 90)
  (let [dw (/ (q/width) (float rw))
        dh (/ (q/height) (float rh))
        hh (* dw 2)]

    (doall
      (for [j (range rh)]
        (let [ic (q/random (count colors))
              dc (+ 3 (q/random 0.02))]

          (doall
            (for [i (range rw)]
              (let [x1 (* i dw)
                    y1 (* j dh)
                    x2 (* (inc i) dw)
                    y2 (* (inc j) dh)
                    ]
                (q/fill (get-fill-color (+ ic (* dc i)) colors))

                (q/begin-shape)
                (q/vertex x1 y1 (* hh (q/noise (* x1 det) (* y1 det))))
                (q/vertex x2 y1 (* hh (q/noise (* x2 det) (* y1 det))))
                (q/vertex x2 y2 (* hh (q/noise (* x2 det) (* y2 det))))
                (q/vertex x1 y2 (* hh (q/noise (* x1 det) (* y2 det))))
                (q/end-shape :close))
              ))
          )
        )
      )
    )
  )

(defn setup []
  (q/smooth)
  ; avoid some saving issues
  (q/hint :disable-async-saveframe)

  ; Set color mode to HSB (HSV) instead of default RGB.
  (q/color-mode :hsb)

  ; setup function returns initial state.
  {:rh     (int (q/random 2 50))
   :rw     (int (q/random 2 180))
   :det    (q/random 0.005)
   :colors [(q/color 39 63 92)
            (q/color 339 29 93)
            (q/color 37 6 82)
            (q/color 194 31 77)
            (q/color 0 73 92)
            (q/color 244 36 53)]
   }
  )

(q/defsketch cuadricula
             :title "Cuadricula"
             :size [500 500]
             :renderer :p3d
             ; setup function called only once, during sketch initialization.
             :setup setup
             :draw draw-state
             :features [:keep-on-top]
             ; This sketch uses functional-mode middleware.
             ; Adds update function which takes current state and returns new state.
             ; Makes all other functions (setup, draw, mouse-click, etc) state-aware.
             :middleware [m/fun-mode])
