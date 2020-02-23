(asdf:defsystem synthia
  :version "1.0"
  :description "Software synthesizer"
  :author "Li Feng <fengli@gmail.com>"
  :licence "public domain"
  :depends-on (cffi cl-openal cl-alc sdl2 sdl2-image sdl2kit bordeaux-threads midi)
  :components
  ((:module "src"
    :serial t
    :components ((:file "packages")
                 (:file "synth")
                 (:file "audio")
                 (:file "ui")
                 (:file "midi")))))
