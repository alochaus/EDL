(ns img2ascii.core
  (:gen-class)
  (:import (java.awt.image BufferedImage)
           (java.awt Image
                     Color)
           (java.io File)
           (javax.imageio ImageIO)))

(def grey-ramp (vec (reverse (char-array "$@B%8&WM#*oahkbdpqwmZO0QLCJUYXzcvunxrjft/\\|()1{}[]?-_+~<>i!lI;:,\"^`'. "))))

(def grey-range
  (let [n     (count grey-ramp)
        ratio (/ 256 n)]
    (map #(* ratio %) (range n))))

(defn get-character
  [val]
  (let [x     (/ (* val (count grey-range)) 256)
        index (first (filter #(>= % x) grey-range))]
    (nth grey-ramp (int index))))


(defn load-image
  [path]
  (let  [file (File. path)]
    (-> file
        (ImageIO/read))))

(defn resize-image
  [image width height]
  (let [scaled_instance (-> image
                            (.getScaledInstance width
                                                height
                                                Image/SCALE_SMOOTH))
        resized-image (BufferedImage. width height BufferedImage/TYPE_INT_ARGB)

        graphics (-> resized-image (.createGraphics))]
    (.drawImage graphics scaled_instance 0 0 nil)
    (.dispose graphics)
    resized-image))

(defn greyscale
  [image]
  (let [greyscaled-image (BufferedImage. (.getWidth image) (.getHeight image) BufferedImage/TYPE_BYTE_GRAY)
        graphics (.getGraphics greyscaled-image)]

    (.drawImage graphics image 0 0 nil)
    (.dispose graphics)
    greyscaled-image))

(defn export-image
  [image path]
  (->> (File. path)
       (ImageIO/write image "png")))

(defn asciify
  [image-ref x y text]
  (let [image        (deref  image-ref)
        image-width  (.getWidth  image)
        image-height (.getHeight image)]
    (cond
      (>= y image-height)  text
      (not= x image-width) (let [val (-> (Color. (.getRGB image x y)) (.getRed))]
                             (recur image-ref (inc x) y (str text (get-character val))))
      :else                (recur image-ref 0 (inc y) (str text \newline)))))

(defn -main
  [& args]
  (let [img      (load-image "chico.png")
        rszd-img (resize-image img 100 100)
        grey-img (greyscale rszd-img)
        ascii    (asciify (ref grey-img) 0 0 "")]
    ascii))

(println (-main))
