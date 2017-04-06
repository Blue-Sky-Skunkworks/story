(in-package :story)

(define-story-module voice
  :directories ("mespeak")
  :scripts ("/mespeak/mespeak.js")
  :imports (("voice-demo" voice-demo-template))
  :depends-on (:polymer))

(define-template voice-demo
  :properties (("say" string "Voice is functional."))
  :style (("#say" :width 400px :margin 20px))
  :content ((input :id "say" :value "{{say}}" :on-change "update"))
  :methods
  ((attached ()
             ((@ me-speak load-config) "mespeak/mespeak_config.json")
             ((@ me-speak load-voice) "mespeak/voices/en/en.json")
             ((@ me-speak speak) (@ this say))
             ((@ this $ say focus)))
   (update () ((@ me-speak speak) (@ this say)))))


