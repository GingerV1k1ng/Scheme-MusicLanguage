;Made by Kurt Nørmark
(load "pp-standard-functions.scm")
(load "pp-standard-higher-order-functions.scm")
(load "music-basis-r5rs.scm")

;music element, that can either be a note, pause, sequential music element or a parallel music element.
(define(musicElement? element)
  (or(note? element)
     (pause? element)
     (sequentialMusicElement element)
     (parallelMusicElement element)))

;Got help by Charlie Byrdam to make the instrument part of note
(define instrument '(piano organ guitar violin flute trumpet helicopter telephone))

;A note in the system, it has the values pitch, duration and channel.
(define(note pitch duration channel)
  (cond  ((not (pitch? pitch)) (error "not a valid pitch"))
         ((not (duration? duration)) (error "not a valid duration"))
         ((not (channel? channel)) (error "not a valid intrument")))
  (list 'note channel pitch duration))

;Defines the pause.
(define(pause duration)
  (if (not (duration? duration))
      (error "Not a valid duration")
      (list 'pause duration)))

;Checks if the element is a pause
(define(pause? lst)
  (if (not (pair? lst))
      #f
      (eq? (car lst) 'pause)))

;Checks if the element is a pitch
(define(pitch? value)
  (and (integer? value) (>= value 0) (<= value 127)))

;Checks if the element is a duration
(define(duration? value)
  (and (integer? value) (>= value 0)))

;Checks if the element is a note
(define(note? lst)
  (if (not (pair? lst))
      #f
      (eq? (car lst) 'note)))

;find-in-list is made by Kurt Nørmark
(define(channel? value)
  (if (find-in-list (lambda(x) (eq? x value)) instrument)
      #t
      #f))

;Makes a list of music elements in sequential order
(define(sequentialMusicElement . listOfMusicElements)
  (cond
    ((find-in-list
      (lambda(x)
        (not (musicElement? x))) listOfMusicElements)
     (error "List contains invalid music elements")))
  (list 'sequentialMusicElement listOfMusicElements))

;Makes a list of music elements in parallel order
(define(parallelMusicElement . listOfMusicElements)
  (cond
    ((find-in-list
      (lambda(x)
        (not (musicElement? x))) listOfMusicElements)
     (error "List contains invalid music elements")))
  (list 'parallelMusicElement listOfMusicElements))

(define(sequentialMusicElement? lst)
  (if (not (pair? lst))
      #f
      (eq? (car lst) 'sequentialMusicElement)))

(define(parallelMusicElement? lst)
  (if (not (pair? lst))
      #f
      (eq? (car lst) 'parallelMusicElement)))

;Got help from Daniel Bolhuis with making of the scale, transposes and re-instrument functions
;Scaling adds extra duration by multiplying the exsiting duration with the decided value
;If sequential or parallel, it multiplies all the durations with the decided value, in the list.
(define(scaling element val)
  (cond
    ((pause? element) (pause (+(getDuration element) val)))
    ((note? element) (note(getPitch element) (*(getDuration element) val) (getInstrument element)))
    ((sequentialMusicElement? element) (apply sequentialMusicElement(scaleHelper(cadr element) val )))
    ((parallelMusicElement? element) (apply parallelMusicElement(scaleHelper(cadr element) val )))))

(define(scaleHelper element val)
  (if (null? element)
      '()
      (cons (scaling (car element) val) (scaleHelper(cdr element) val))))

;Changes the pitch of a music element by adding the extra value, to excisting value
;If it's a sequential or parallel, it adds the value to all the music elements in the list
(define(transposes element val)
  (cond
    ((pause? element) (error "Can't change pitch on a pause"))
    ((note? element) (note (+(getPitch element) val) (getDuration element) (getInstrument element)))
    ((sequentialMusicElement? element) (apply sequentialMusicElement(transposesHelper(cdr element) val)))
    ((parallelMusicElement? element) (apply parallelMusicElement(transposesHelper(cadr element) val)))))

(define(transposesHelper element val)
  (if (null? element)
      '()
      (cons (transposes (car element) val) (transposesHelper(cdr element) val))))

;Changes the instrument of a specific music element.
;If it is sequantial or parallel, it changes all the instruments to the new one, in the list.
(define(reInstrument element instrument)
  (cond
    ((pause? element) (error "Can't change instrument on a pause"))
    ((note? element) (note (getPitch element) (getDuration element) instrument))
    ((sequentialMusicElement? element) (apply sequentialMusicElement(instrumentHelper(cadr element) instrument)))
    ((parallelMusicElement? element) (apply parallelMusicElement(instrumentHelper(cadr element) instrument)))))

(define(instrumentHelper element instrument)
  (if (null? element)
      '()
      (cons(reInstrument (car element) instrument) (instrumentHelper(cdr element) instrument))))

;Gets the duration of a music element. If it's the sequential it gets the sum of the music element
;If its parallel it takes the maximum duration of the whole music element
(define(getDuration element)
  (cond
    ((pause? element) (cdr element))
    ((note? element) (car(cdddr element)))
    ((sequentialMusicElement? element) (getDurationSequential(cadr element)))
    ((parallelMusicElement? element) (getDurationParallel (cadr element)))))
    
(define(getInstrument element)
  (if(note? element) (car(cdr element))))

(define(getPitch element)
  (if (note? element) (car(cddr element))))

(define(getDurationSequential element)
  (if (null? element)
      0
      (+ (getDuration (car element)) (getDurationSequential(cdr element)))))

(define(getDurationParallel element)
  (if (null? element)
      0
      (max (getDuration (car element)) (getDurationParallel(cdr element)))))

;Linearizer, makes all music elements to one list. I use two helper functions, linearizer-helper for sequential and for parallel.
;They appends the strings together and makes the recursion. To calculate the abselute time. 
(define(linearizer element time)
  (cond
    ((pause? element) '() )
    ((note? element) (list (note-abs-time-with-duration time (channelToInstrument(getInstrument element)) (getPitch element) 88 (getDuration element))))
    ((sequentialMusicElement? element) (linearizerHelperSequential(cadr element) time))
    ((parallelMusicElement? element) (linearizerHelperParallel(cadr element) time))))

(define(linearizerHelperSequential element time)
  (if (null? element)
      '()
      (append (linearizer(car element) time) (linearizerHelperSequential(cdr element) (+ time (getDuration(car element)))))))

(define(linearizerHelperParallel element time)
  (if (null? element)
      '()
      (append (linearizer(car element) time) (linearizerHelperParallel(cdr element) (max time (getDuration(car element)))))))

;Converts a symbol to a number. Checks if the instrument is equal with the symbol, then returns a number.
(define(channelToInstrument instrument)
  (cond
    ((eq? instrument 'piano) 1)
    ((eq? instrument 'organ) 2)
    ((eq? instrument 'guitar) 3)
    ((eq? instrument 'violin) 4)
    ((eq? instrument 'flute) 5)
    ((eq? instrument 'trumpet) 6)
    ((eq? instrument 'helicopter) 7)
    ((eq? instrument 'telephone) 8)))

;Definetions helping with coding
(define p(pause 50))
(define n(note 22 50 'piano))
(define sequentialLst(sequentialMusicElement n n n n n n))
(define parallelLst(parallelMusicElement n n))

;Does not work, I get an error in music-basis-r5rs.scm. If this line gets removed, it can compile.
(transform-to-midi-file-and-write-to-file! (linearizer sequentialLst 0) "sodmusik.MIDI")

