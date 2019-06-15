#lang racket

(require racket/gui/base)

(define app-title "Still Video")

(define image-file #f)

(define sound-file #f)

(define (extension file)
  (car (reverse (string-split (path->string file) "."))))

(define (image-file? file)
  (member (extension file) (list "png" "gif" "jpg" "jpeg" "bmp")))

(define (sound-file? file)
  (member (extension file) (list "wav" "mp3" "ogg" "flac")))

(define (accept-file file)
  (when (image-file? file) (set-image file))
  (when (sound-file? file) (set-sound file)))

(define (set-image file)
  (set! image-file file)
  (send target-dc clear)
  (send target-dc draw-bitmap (make-object bitmap% file) 0 0))

(define (set-sound file)
  (set! sound-file file)
  (send text-sound set-value (path->string (file-name-from-path sound-file))))

(define (valid-files?)
  (and sound-file image-file))

(define (create-still-video image sound output)
  (define cmd
    (string-append
       "ffmpeg -y -loop 1 -i "
       "\"" image "\""
       " -i "
       "\"" sound "\""
       " -framerate 24 -c:v libx264 -c:a aac -b:a 384k -shortest "
       "\"" output "\""))
  (system cmd))

(define (attempt-create)
  (create-still-video
    (path->string image-file)
    (path->string sound-file)
    (path->string (path-replace-extension sound-file ".mp4"))))

(define file-drop-canvas%
  (class canvas%
    (define/override (on-drop-file file)
      (accept-file file)
      (super on-drop-file file))
    (super-new)))

(define frame (new frame% (label app-title))) 

(define target-canvas
  (new file-drop-canvas%
       (parent frame)
       (min-width 640)
       (min-height 360)
       (paint-callback
         (lambda (canvas event)
           (when (and image-file (file-exists? image-file))
                 (send target-dc draw-bitmap
                                 (make-object bitmap% image-file) 0 0))))))

(define target-dc (send target-canvas get-dc))

(define message-panel (new horizontal-panel%
                           (parent frame)
                           (stretchable-height #f)))

(define message (new message%
                     (parent message-panel)
                     (label "")))

(define control-panel (new horizontal-panel%
                           (parent frame)
                           (stretchable-height #f)))

(define text-sound
  (new text-field%
       (label "Music File")
       (parent control-panel)))

(define button-create
  (new button%
       (label "Create")
       (parent control-panel)
       (callback
         (lambda (button event)
           (if (valid-files?)
               (
                 (send message set-label "Please wait...")
                 (if (attempt-create)
                     (message-box "Done" "Video created.")
                     (message-box "Error" "Something went wrong."))
                 (send message set-label ""))
               (message-box "Bad Input"
                            "Need to select image and sound file."))))))

(define button-exit
  (new button%
       (label "Exit")
       (parent control-panel)
       (callback (lambda (button event) (send frame show #f)))))

(send target-canvas accept-drop-files #t)
(send frame show #t)
