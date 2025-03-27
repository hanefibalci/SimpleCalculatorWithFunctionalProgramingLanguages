#lang racket

(require racket/base
         racket/eval
         racket/string)

;; Bu fonksiyon, verilen sembolün matematik operatörü olup olmadığını kontrol eder.
(define (operator? x)
  (member x '(+ - * /)))

;; Operatörlerin öncelik derecelerini belirliyoruz.
(define (precedence op)
  (case op
    ((* /) 2)
    ((+ -) 1)
    (else 0)))

;; Infix ifadeleri prefix ifadeye çevirme fonksiyonu
(define (infix->prefix expr)
  (let loop ((tokens expr)
             (output '())
             (ops '()))
    (cond
      ((null? tokens)
       (reverse (append (reverse ops) output)))
      ((number? (car tokens))
       (loop (cdr tokens) (cons (car tokens) output) ops))
      ((operator? (car tokens))
       (let ((op1 (car tokens)))
         (let recur ((ops ops))
           (if (and (not (null? ops))
                    (>= (precedence (car ops)) (precedence op1)))
               (recur (cdr ops))
               (loop (cdr tokens) output (cons op1 ops))))))
      (else
       (error "Geçersiz token:" (car tokens))))))

;; Racket'ta eval kullanırken bir namespace belirtmemiz gerekiyor
(define (calculate expr)
  (eval expr (make-base-namespace)))

;; Kullanıcıdan satır satır girdi alan ve işlemleri yapan ana döngü
(define (main)
  (display "Scheme Hesap Makinesi (Çıkmak için 'exit' yazın)\n")
  (let loop ()
    (display ">>> ")
    (flush-output-port (current-output-port))
    (let ((input (read-line)))
      (cond
        [(eof-object? input)
         (display "Çıkılıyor...\n")]
        [(string=? input "exit")
         (display "Çıkılıyor...\n")]
        [else
         (with-handlers ([exn:fail? (lambda (ex)
                                      (display "Hata: Geçersiz ifade!\n"))])
           (let* ((tokens (map (lambda (x)
                                 (or (string->number x)
                                     (string->symbol x)))
                               (string-split input)))
                  (prefix (infix->prefix tokens)))
             (display "Sonuç: ")
             (display (calculate prefix))
             (newline)))
         (loop)]))))

;; Programı başlat
(main)
