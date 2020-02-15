(asdf:defsystem synthia
  :version "1.0"
  :description "Software synthesizer"
  :author "Li Feng <fengli@gmail.com>"
  :licence "public domain"
  :depends-on (cffi cl-openal cl-alc sdl2 sdl2-image sdl2kit)
  :serial t
  :components
  ((:file "main")))
