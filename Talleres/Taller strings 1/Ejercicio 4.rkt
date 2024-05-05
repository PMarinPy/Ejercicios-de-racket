#lang racket
(define (pasar c)
  (if (and(> (char->integer c) 64)(< (char->integer c) 91))
          (integer->char (+ 32 (char->integer c)))
          (integer->char (- (char->integer c) 32))))

(define (contar s esp cont)
  (if (< cont (string-length s))
      (contar s (if (char=? (string-ref s cont) #\ )
                    (+ esp 1)
                    esp) (+ cont 1) )
      (+ esp 1)))

(define (pasarstr str ind)
  (if (< ind (string-length str))
      (begin
        (string-set! str ind (pasar (string-ref str ind)))
        (pasarstr str (+ ind 1)))
      (display str)))

(define (substr str strf pos1 pos2 ind)
  (if (< pos1 pos2)
      (begin
        (string-set! strf ind (string-ref str pos1))
        (substr str strf (+ pos1 1) pos2 (+ ind 1)))
      strf))

(define (eliminar str ind1 ind2 strf)
  (if (< ind1 (string-length str))
      (begin
        (if (not(char=? (string-ref str ind1) #\ ))
            (begin
              (string-set! strf ind2 (string-ref str ind1))
              (eliminar str (+ ind1 1) (+ ind2 1) strf))
            (eliminar str (+ ind1 1) ind2 strf)))
      strf))

(define (agregar1 str strf ind)
  (if (< ind (string-length str))
      (begin
        (string-set! strf  ind (string-ref str ind))
        (agregar1 str strf (+ ind 1)))
      strf))

(define (agregar2 str strf ind i)
  (if (< i (string-length str))
      (begin
        (string-set! strf  ind (string-ref str i))
        (agregar2 str strf (+ ind 1) (+ i 1)))
      strf))

(define (append str1 str2)
  (define strf (make-string (+ (string-length str1)  (string-length str2)) #\a ))
  (agregar1 str1 strf 0)
  (display strf)
  (agregar2 str2 strf (string-length str1) 0))

(define (buscsub str sub ind ind1 cont)
  (if (< ind (string-length str))
      (begin
        (cond
          [(char=? (string-ref str ind) (string-ref sub ind1))
           (if (< ind1 (- (string-length sub) 1))
               (buscsub str sub (+ 1 ind) (+ 1 ind1) cont)
               (buscsub str sub (+ 1 ind) 0 (+ 1 cont)))]
          [(not(char=? (string-ref str ind) (string-ref sub ind1)))(buscsub str sub (+ 1 ind) 0 cont)]
          ))
      cont))






