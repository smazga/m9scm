; Scheme 9 from Empty Space
; Real Number Test Suite
; By Nils M Holm, 2008, 2009

(define testfile "test.tmp")

(if (file-exists? testfile)
    (delete-file testfile))

(define Errors 0)

(define (void) (if #f #f))

(define (seq)
  (let ((n 1))
    (lambda ()
      (let ((x n))
        (set! n (+ 1 n))
        x))))

(define (fail expr result expected)
  (display "test failed: ")
  (write expr)
  (newline)
  (display "got result:  ")
  (write result)
  (newline)
  (display "expected:    ")
  (write expected)
  (newline)
  (set! Errors (+ 1 Errors)))

(define (test3 expr result expected)
;  (write expr) (display " => ") (write result) (newline)
  (if (not (equal? result expected))
      (fail expr result expected)))

(define-syntax (test form result)
  `(test3 ',form ,form ,result))

; --- arithmetics ---

(test (+  0.0) 0.0)
(test (+  1.0) 1.0)
(test (+ -1.0) -1.0)
(test (+ 0.0 1234567890123.4) 1234567890123.4)
(test (+ 1234567890123.4 0.0) 1234567890123.4)
(test (+  123.45  123.45) 246.9)
(test (+  123.45 -123.45) 0.0)
(test (+ -123.45  123.45) 0.0)
(test (+ -123.45 -123.45) -246.9)
(test (+  1e10  12345.67) 1.000001234567e10)
(test (+  1e10 -12345.67) 9.99998765433e9)
(test (+ -1e10  12345.67) -9.99998765433e9)
(test (+ -1e10 -12345.67) -1.000001234567e10)
(test (+  1e-10  12345.67) 12345.6700000001)
(test (+  1e-10 -12345.67) -12345.6699999999)
(test (+ -1e-10  12345.67) 12345.6699999999)
(test (+ -1e-10 -12345.67) -12345.6700000001)
(test (+  12345.67  1e10) 1.000001234567e10)
(test (+  12345.67 -1e10) -9.99998765433e9)
(test (+ -12345.67  1e10) 9.99998765433e9)
(test (+ -12345.67 -1e10) -1.000001234567e10)
(test (+  12345.67  1e-10) 12345.6700000001)
(test (+  12345.67 -1e-10) 12345.6699999999)
(test (+ -12345.67  1e-10) -12345.6699999999)
(test (+ -12345.67 -1e-10) -12345.6700000001)
(test (+ 999999999.9 1) 1000000000.9)
(test (+ 1 999999999.9) 1000000000.9)
(test (+ 1000000000.9 -1) 999999999.9)
(test (+ -1 1000000000.9) 999999999.9)
(test (+ 12345.67  1234567) 1246912.67)
(test (+ 12345.67 123456.7) 135802.37)
(test (+ 12345.67 12345.67) 24691.34)
(test (+ 12345.67 1234.567) 13580.237)
(test (+ 12345.67 123.4567) 12469.1267)
(test (+ 12345.67 12.34567) 12358.01567)
(test (+ 12345.67 1.234567) 12346.904567)
(test (+ 12345.67 .1234567) 12345.7934567)
(test (+  1234567 12345.67) 1246912.67)
(test (+ 123456.7 12345.67) 135802.37)
(test (+ 12345.67 12345.67) 24691.34)
(test (+ 1234.567 12345.67) 13580.237)
(test (+ 123.4567 12345.67) 12469.1267)
(test (+ 12.34567 12345.67) 12358.01567)
(test (+ 1.234567 12345.67) 12346.904567)
(test (+ .1234567 12345.67) 12345.7934567)
(test (+ 1.1 2.2 3.3 4.4 5.5) 16.5)
(test (exact? (+   1.0   1.0)) #f)
(test (exact? (+ #i1.0   1.0)) #f)
(test (exact? (+   1.0 #i1.0)) #f)
(test (exact? (+ #i1.0 #i1.0)) #f)

(test (-  0.0) 0.0)
(test (-  1.0) -1.0)
(test (- -1.0) 1.0)
(test (- 0.0 1234567890123.4) -1234567890123.4)
(test (- 1234567890123.4 0.0) 1234567890123.4)
(test (-  123.45  123.45) 0.0)
(test (-  123.45 -123.45) 246.9)
(test (- -123.45  123.45) -246.9)
(test (- -123.45 -123.45) 0.0)
(test (-  1e10  12345.67) 9.99998765433e9)
(test (-  1e10 -12345.67) 1.000001234567e10)
(test (- -1e10  12345.67) -1.000001234567e10)
(test (- -1e10 -12345.67) -9.99998765433e9)
(test (-  1e-10  12345.67) -12345.6699999999)
(test (-  1e-10 -12345.67) 12345.6700000001)
(test (- -1e-10  12345.67) -12345.6700000001)
(test (- -1e-10 -12345.67) 12345.6699999999)
(test (-  12345.67  1e10) -9.99998765433e9)
(test (-  12345.67 -1e10) 1.000001234567e10)
(test (- -12345.67  1e10) -1.000001234567e10)
(test (- -12345.67 -1e10) 9.99998765433e9)
(test (-  12345.67  1e-10) 12345.6699999999)
(test (-  12345.67 -1e-10) 12345.6700000001)
(test (- -12345.67  1e-10) -12345.6700000001)
(test (- -12345.67 -1e-10) -12345.6699999999)
(test (- 999999999.9 -1) 1000000000.9)
(test (- -1 999999999.9) -1000000000.9)
(test (- 1000000000.9 1) 999999999.9)
(test (- 1 1000000000.9) -999999999.9)
(test (- 12345.67  1234567) -1222221.33)
(test (- 12345.67 123456.7) -111111.03)
(test (- 12345.67 12345.67) 0.0)
(test (- 12345.67 1234.567) 11111.103)
(test (- 12345.67 123.4567) 12222.2133)
(test (- 12345.67 12.34567) 12333.32433)
(test (- 12345.67 1.234567) 12344.435433)
(test (- 12345.67 .1234567) 12345.5465433)
(test (-  1234567 12345.67) 1222221.33)
(test (- 123456.7 12345.67) 111111.03)
(test (- 12345.67 12345.67) 0.0)
(test (- 1234.567 12345.67) -11111.103)
(test (- 123.4567 12345.67) -12222.2133)
(test (- 12.34567 12345.67) -12333.32433)
(test (- 1.234567 12345.67) -12344.435433)
(test (- .1234567 12345.67) -12345.5465433)
(test (- 1.1 2.2 3.3 4.4 5.5) -14.3)
(test (exact? (-   2.0   1.0)) #f)
(test (exact? (- #i2.0   1.0)) #f)
(test (exact? (-   2.0 #i1.0)) #f)
(test (exact? (- #i2.0 #i1.0)) #f)

(test (* 0.0  0.0)  0.0)
(test (* 0.0  0.1)  0.0)
(test (* 0.0  1.0)  0.0)
(test (* 0.0 -0.0)  0.0)
(test (* 0.0 -0.1)  0.0)
(test (* 0.0 -1.0)  0.0)
(test (* 0.1  0.0)  0.0)
(test (* 0.1  0.1)  0.01)
(test (* 0.1  1.0)  0.1)
(test (* 0.1 -0.0)  0.0)
(test (* 0.1 -0.1) -0.01)
(test (* 0.1 -1.0) -0.1)
(test (* 1.0  0.0)  0.0)
(test (* 1.0  0.1)  0.1)
(test (* 1.0  1.0)  1.0)
(test (* 1.0 -0.0)  0.0)
(test (* 1.0 -0.1) -0.1)
(test (* 1.0 -1.0) -1.0)
(test (*  123.45  123.45) 15239.9025)
(test (*  123.45 -123.45) -15239.9025)
(test (* -123.45  123.45) -15239.9025)
(test (* -123.45 -123.45) 15239.9025)
(test (*  123.45e+100  123.45e+100) 1.52399025e204)
(test (*  123.45e+100 -123.45e+100) -1.52399025e204)
(test (* -123.45e+100  123.45e+100) -1.52399025e204)
(test (* -123.45e+100 -123.45e+100) 1.52399025e204)
(test (*  123.45e-100  123.45e-100) 1.52399025e-196)
(test (*  123.45e-100 -123.45e-100) -1.52399025e-196)
(test (* -123.45e-100  123.45e-100) -1.52399025e-196)
(test (* -123.45e-100 -123.45e-100) 1.52399025e-196)
(test (* 12345.67 .1234567) 1524.155677489)
(test (* 12345.67 1.234567) 15241.55677489)
(test (* 12345.67 12.34567) 152415.5677489)
(test (* 12345.67 123.4567) 1524155.677489)
(test (* 12345.67 1234.567) 15241556.77489)
(test (* 12345.67 12345.67) 152415567.7489)
(test (* 12345.67 123456.7) 1.524155677489e9)
(test (* 12345.67 1234567.) 1.524155677489e10)
(test (* -.1234567 12345.67) -1524.155677489)
(test (* -1.234567 12345.67) -15241.55677489)
(test (* -12.34567 12345.67) -152415.5677489)
(test (* -123.4567 12345.67) -1524155.677489)
(test (* -1234.567 12345.67) -15241556.77489)
(test (* -12345.67 12345.67) -152415567.7489)
(test (* -123456.7 12345.67) -1.524155677489e9)
(test (* -1234567. 12345.67) -1.524155677489e10)
(test (* 1.0   2   3   4   5   6   7   8   9) 362880.0)
(test (* 1.1 2.2 3.3 4.4 5.5 6.6 7.7 8.8 9.9) 855652.05811008)

(test (/ 0.0  0.1) 0.0)
(test (/ 0.0  1.0) 0.0)
(test (/ 0.0 -0.1) 0.0)
(test (/ 0.0 -1.0) 0.0)
(test (/ 0.1  0.1) 1.0)
(test (/ 0.1  1.0) 0.1)
(test (/ 0.1 -0.1) -1.0)
(test (/ 0.1 -1.0) -0.1)
(test (/ 1.0  0.1) 10.0)
(test (/ 1.0  1.0) 1.0)
(test (/ 1.0 -0.1) -10.0)
(test (/ 1.0 -1.0) -1.0)
(test (/  12345.  123.45) 100.0)
(test (/  12345. -123.45) -100.0)
(test (/ -12345.  123.45) -100.0)
(test (/ -12345. -123.45) 100.0)
(test (/  152337.3  123.4) 1234.5)
(test (/  152337.3 -123.4) -1234.5)
(test (/ -152337.3  123.4) -1234.5)
(test (/ -152337.3 -123.4) 1234.5)
(test (/  1.52399025e+204  123.45e+100) 123.45e+100)
(test (/ -1.52399025e+204 -123.45e+100) 123.45e+100)
(test (/ -1.52399025e+204  123.45e+100) -123.45e+100)
(test (/  1.52399025e+204 -123.45e+100) -123.45e+100)
(test (/  1.52399025e-196  123.45e-100) 123.45e-100)
(test (/ -1.52399025e-196 -123.45e-100) 123.45e-100)
(test (/ -1.52399025e-196  123.45e-100) -123.45e-100)
(test (/  1.52399025e-196 -123.45e-100) -123.45e-100)
(test (/ 12345.67 .1234567) 100000.0)
(test (/ 12345.67 1.234567) 10000.0)
(test (/ 12345.67 12.34567) 1000.0)
(test (/ 12345.67 123.4567) 100.0)
(test (/ 12345.67 1234.567) 10.0)
(test (/ 12345.67 12345.67) 1.0)
(test (/ 12345.67 123456.7) 0.1)
(test (/ 12345.67 1234567.) 0.01)
(test (/ -.1234567 12345.67) -0.00001)
(test (/ -1.234567 12345.67) -0.0001)
(test (/ -12.34567 12345.67) -0.001)
(test (/ -123.4567 12345.67) -0.01)
(test (/ -1234.567 12345.67) -0.1)
(test (/ -12345.67 12345.67) -1.0)
(test (/ -123456.7 12345.67) -10.0)
(test (/ -1234567. 12345.67) -100.0)
(test (/ 1.0) 1.0)
(test (/ 2.0) 0.5)
(test (/ 5.0) 0.2)
(test (/ 128.0 64 32 16 8 4 2 1) 6.103515625e-5)

(test (<  0.0  0.0) #f)
(test (< -0.0  0.0) #f)
(test (<  0.0 -0.0) #f)
(test (< -0.0 -0.0) #f)
(test (<  1.0  1.0) #f)
(test (< -1.0  1.0) #t)
(test (<  1.0 -1.0) #f)
(test (< -1.0 -1.0) #f)
(test (<  0.1  0.1) #f)
(test (< -0.1  0.1) #t)
(test (<  0.1 -0.1) #f)
(test (< -0.1 -0.1) #f)
(test (<  123.45e+67  123.45e+67) #f)
(test (<  123.45e+67  123.45e-67) #f)
(test (<  123.45e-67  123.45e+67) #t)
(test (<  123.45e-67  123.45e-67) #f)
(test (<  123.45e+67 -123.45e+67) #f)
(test (<  123.45e+67 -123.45e-67) #f)
(test (<  123.45e-67 -123.45e+67) #f)
(test (<  123.45e-67 -123.45e-67) #f)
(test (< -123.45e+67  123.45e+67) #t)
(test (< -123.45e+67  123.45e-67) #t)
(test (< -123.45e-67  123.45e+67) #t)
(test (< -123.45e-67  123.45e-67) #t)
(test (< -123.45e+67 -123.45e+67) #f)
(test (< -123.45e+67 -123.45e-67) #t)
(test (< -123.45e-67 -123.45e+67) #f)
(test (< -123.45e-67 -123.45e-67) #f)
(test (<  0.0 0.0) #f)
(test (<  1.0 0.0) #f)
(test (< -1.0 0.0) #t)
(test (<  0.1 0.0) #f)
(test (< -0.1 0.0) #t)
(test (<  1e+0 0.0) #f)
(test (< -1e+0 0.0) #t)
(test (<  1e-0 0.0) #f)
(test (< -1e-0 0.0) #t)
(test (<  1e+100 0.0) #f)
(test (< -1e+100 0.0) #t)
(test (<  1e-100 0.0) #f)
(test (< -1e-100 0.0) #t)
(test (<  1e+10000 0.0) #f)
(test (< -1e+10000 0.0) #t)
(test (<  1e-10000 0.0) #f)
(test (< -1e-10000 0.0) #t)
(test (< 0.0  0.0) #f)
(test (< 0.0  1.0) #t)
(test (< 0.0 -1.0) #f)
(test (< 0.0  0.1) #t)
(test (< 0.0 -0.1) #f)
(test (< 0.0  1e+0) #t)
(test (< 0.0 -1e+0) #f)
(test (< 0.0  1e-0) #t)
(test (< 0.0 -1e-0) #f)
(test (< 0.0  1e+100) #t)
(test (< 0.0 -1e+100) #f)
(test (< 0.0  1e-100) #t)
(test (< 0.0 -1e-100) #f)
(test (< 0.0  1e+10000) #t)
(test (< 0.0 -1e+10000) #f)
(test (< 0.0  1e-10000) #t)
(test (< 0.0 -1e-10000) #f)
(test (<  1.0e14  1.0e14) #f)
(test (<  1.0e14 -1.0e14) #f)
(test (< -1.0e14  1.0e14) #t)
(test (< -1.0e14 -1.0e14) #f)
(test (<  1.0e14  1.1e14) #t)
(test (<  1.0e14 -1.1e14) #f)
(test (< -1.0e14  1.1e14) #t)
(test (< -1.0e14 -1.1e14) #f)
(test (<  1.0e-14  1.0e-14) #f)
(test (<  1.0e-14 -1.0e-14) #f)
(test (< -1.0e-14  1.0e-14) #t)
(test (< -1.0e-14 -1.0e-14) #f)
(test (<  1.0e-14  1.1e-14) #t)
(test (<  1.0e-14 -1.1e-14) #f)
(test (< -1.0e-14  1.1e-14) #t)
(test (< -1.0e-14 -1.1e-14) #f)
(test (<  1234567890123.4  1234567890123) #f)
(test (<  1234567890123.4 -1234567890123) #f)
(test (< -1234567890123.4  1234567890123) #t)
(test (< -1234567890123.4 -1234567890123) #t)
(test (<  1234567890123  1234567890123.4) #t)
(test (<  1234567890123 -1234567890123.4) #f)
(test (< -1234567890123  1234567890123.4) #t)
(test (< -1234567890123 -1234567890123.4) #f)
(test (< 1e-5 1e-4 1e-3 0.01 0.1 1.0 10.0) #t)
(test (< 1e-5 1e-4 1e-3 0.01 0.1 1.0  1.0) #f)
(test (< 1.0 1.0 1.0 1.0 1.0 1.0) #f)

(test (<=  0.0  0.0) #t)
(test (<= -0.0  0.0) #t)
(test (<=  0.0 -0.0) #t)
(test (<= -0.0 -0.0) #t)
(test (<=  1.0  1.0) #t)
(test (<= -1.0  1.0) #t)
(test (<=  1.0 -1.0) #f)
(test (<= -1.0 -1.0) #t)
(test (<=  0.1  0.1) #t)
(test (<= -0.1  0.1) #t)
(test (<=  0.1 -0.1) #f)
(test (<= -0.1 -0.1) #t)
(test (<=  123.45e+67  123.45e+67) #t)
(test (<=  123.45e+67  123.45e-67) #f)
(test (<=  123.45e-67  123.45e+67) #t)
(test (<=  123.45e-67  123.45e-67) #t)
(test (<=  123.45e+67 -123.45e+67) #f)
(test (<=  123.45e+67 -123.45e-67) #f)
(test (<=  123.45e-67 -123.45e+67) #f)
(test (<=  123.45e-67 -123.45e-67) #f)
(test (<= -123.45e+67  123.45e+67) #t)
(test (<= -123.45e+67  123.45e-67) #t)
(test (<= -123.45e-67  123.45e+67) #t)
(test (<= -123.45e-67  123.45e-67) #t)
(test (<= -123.45e+67 -123.45e+67) #t)
(test (<= -123.45e+67 -123.45e-67) #t)
(test (<= -123.45e-67 -123.45e+67) #f)
(test (<= -123.45e-67 -123.45e-67) #t)
(test (<=  0.0 0.0) #t)
(test (<=  1.0 0.0) #f)
(test (<= -1.0 0.0) #t)
(test (<=  0.1 0.0) #f)
(test (<= -0.1 0.0) #t)
(test (<=  1e+0 0.0) #f)
(test (<= -1e+0 0.0) #t)
(test (<=  1e-0 0.0) #f)
(test (<= -1e-0 0.0) #t)
(test (<=  1e+100 0.0) #f)
(test (<= -1e+100 0.0) #t)
(test (<=  1e-100 0.0) #f)
(test (<= -1e-100 0.0) #t)
(test (<=  1e+10000 0.0) #f)
(test (<= -1e+10000 0.0) #t)
(test (<=  1e-10000 0.0) #f)
(test (<= -1e-10000 0.0) #t)
(test (<= 0.0  0.0) #t)
(test (<= 0.0  1.0) #t)
(test (<= 0.0 -1.0) #f)
(test (<= 0.0  0.1) #t)
(test (<= 0.0 -0.1) #f)
(test (<= 0.0  1e+0) #t)
(test (<= 0.0 -1e+0) #f)
(test (<= 0.0  1e-0) #t)
(test (<= 0.0 -1e-0) #f)
(test (<= 0.0  1e+100) #t)
(test (<= 0.0 -1e+100) #f)
(test (<= 0.0  1e-100) #t)
(test (<= 0.0 -1e-100) #f)
(test (<= 0.0  1e+10000) #t)
(test (<= 0.0 -1e+10000) #f)
(test (<= 0.0  1e-10000) #t)
(test (<= 0.0 -1e-10000) #f)
(test (<=  1.0e14  1.0e14) #t)
(test (<=  1.0e14 -1.0e14) #f)
(test (<= -1.0e14  1.0e14) #t)
(test (<= -1.0e14 -1.0e14) #t)
(test (<=  1.0e14  1.1e14) #t)
(test (<=  1.0e14 -1.1e14) #f)
(test (<= -1.0e14  1.1e14) #t)
(test (<= -1.0e14 -1.1e14) #f)
(test (<=  1.0e-14  1.0e-14) #t)
(test (<=  1.0e-14 -1.0e-14) #f)
(test (<= -1.0e-14  1.0e-14) #t)
(test (<= -1.0e-14 -1.0e-14) #t)
(test (<=  1.0e-14  1.1e-14) #t)
(test (<=  1.0e-14 -1.1e-14) #f)
(test (<= -1.0e-14  1.1e-14) #t)
(test (<= -1.0e-14 -1.1e-14) #f)
(test (<=  1234567890123.4  1234567890123) #f)
(test (<=  1234567890123.4 -1234567890123) #f)
(test (<= -1234567890123.4  1234567890123) #t)
(test (<= -1234567890123.4 -1234567890123) #t)
(test (<=  1234567890123  1234567890123.4) #t)
(test (<=  1234567890123 -1234567890123.4) #f)
(test (<= -1234567890123  1234567890123.4) #t)
(test (<= -1234567890123 -1234567890123.4) #f)
(test (<= 1e-5 1e-4 1e-3 0.01 0.1 1.0 10.0) #t)
(test (<= 1e-5 1e-4 1e-3 0.01 0.1 1.0  1.0) #t)
(test (<= 1.0 1.0 1.0 1.0 1.0 1.0) #t)

(test (=  0.0  0.0) #t)
(test (=  0.0 -0.0) #t)
(test (= -0.0  0.0) #t)
(test (= -0.0 -0.0) #t)
(test (=  1.0  1.0) #t)
(test (=  1.0 -1.0) #f)
(test (= -1.0  1.0) #f)
(test (= -1.0 -1.0) #t)
(test (=  0.1  0.1) #t)
(test (=  0.1 -0.1) #f)
(test (= -0.1  0.1) #f)
(test (= -0.1 -0.1) #t)
(test (= 1.0   1) #t)
(test (=   1 1.0) #t)
(test (=  123.456e3 123456) #t)
(test (=  123.456e4 1234560) #t)
(test (=  123.456e5 12345600) #t)
(test (=  123.456e10 1234560000000) #t)
(test (= -123.456e3  -123456) #t)
(test (= -123.456e4  -1234560) #t)
(test (= -123.456e5  -12345600) #t)
(test (= -123.456e10 -1234560000000) #t)
(test (=  1.2345678901234  12345678901234.) #f)
(test (=  1.2345678901234  1234567890123.4) #f)
(test (=  1.2345678901234  123456789012.34) #f)
(test (=  1.2345678901234  12345678901.234) #f)
(test (=  1.2345678901234  1234567890.1234) #f)
(test (=  1.2345678901234  123456789.01234) #f)
(test (=  1.2345678901234  12345678.901234) #f)
(test (=  1.2345678901234  1234567.8901234) #f)
(test (=  1.2345678901234  123456.78901234) #f)
(test (=  1.2345678901234  12345.678901234) #f)
(test (=  1.2345678901234  1234.5678901234) #f)
(test (=  1.2345678901234  123.45678901234) #f)
(test (=  1.2345678901234  12.345678901234) #f)
(test (=  1.2345678901234  1.2345678901234) #t)
(test (= -1.2345678901234 1.2345678901234) #f)
(test (=  1.2345678901234 -1.2345678901234) #f)
(test (=  1.2345678901234  1.2345678901233) #f)
(test (=  1.2345678901234  1.2345678901235) #f)
(test (= 1e50 100000000000000000000000000000000000000000000000000) #t)
(test (= 100000000000000000000000000000000000000000000000000 1e50) #t)
(test (= 12345678901234.0 12345678901234) #t)
(test (= 12345678901234 12345678901234.0) #t)
(test (= -12345678901234.0 -12345678901234) #t)
(test (= -12345678901234 -12345678901234.0) #t)
(test (= 1.0 1.0 1.0 1.0 1.0 1.0 1.0) #t)
(test (= 1.0 1.0 1.0 1.0 1.0 1.0  .1) #f)

(test (>  0.0  0.0) #f)
(test (> -0.0  0.0) #f)
(test (>  0.0 -0.0) #f)
(test (> -0.0 -0.0) #f)
(test (>  1.0  1.0) #f)
(test (> -1.0  1.0) #f)
(test (>  1.0 -1.0) #t)
(test (> -1.0 -1.0) #f)
(test (>  0.1  0.1) #f)
(test (> -0.1  0.1) #f)
(test (>  0.1 -0.1) #t)
(test (> -0.1 -0.1) #f)
(test (>  123.45e+67  123.45e+67) #f)
(test (>  123.45e+67  123.45e-67) #t)
(test (>  123.45e-67  123.45e+67) #f)
(test (>  123.45e-67  123.45e-67) #f)
(test (>  123.45e+67 -123.45e+67) #t)
(test (>  123.45e+67 -123.45e-67) #t)
(test (>  123.45e-67 -123.45e+67) #t)
(test (>  123.45e-67 -123.45e-67) #t)
(test (> -123.45e+67  123.45e+67) #f)
(test (> -123.45e+67  123.45e-67) #f)
(test (> -123.45e-67  123.45e+67) #f)
(test (> -123.45e-67  123.45e-67) #f)
(test (> -123.45e+67 -123.45e+67) #f)
(test (> -123.45e+67 -123.45e-67) #f)
(test (> -123.45e-67 -123.45e+67) #t)
(test (> -123.45e-67 -123.45e-67) #f)
(test (>  0.0 0.0) #f)
(test (>  1.0 0.0) #t)
(test (> -1.0 0.0) #f)
(test (>  0.1 0.0) #t)
(test (> -0.1 0.0) #f)
(test (>  1e+0 0.0) #t)
(test (> -1e+0 0.0) #f)
(test (>  1e-0 0.0) #t)
(test (> -1e-0 0.0) #f)
(test (>  1e+100 0.0) #t)
(test (> -1e+100 0.0) #f)
(test (>  1e-100 0.0) #t)
(test (> -1e-100 0.0) #f)
(test (>  1e+10000 0.0) #t)
(test (> -1e+10000 0.0) #f)
(test (>  1e-10000 0.0) #t)
(test (> -1e-10000 0.0) #f)
(test (> 0.0  0.0) #f)
(test (> 0.0  1.0) #f)
(test (> 0.0 -1.0) #t)
(test (> 0.0  0.1) #f)
(test (> 0.0 -0.1) #t)
(test (> 0.0  1e+0) #f)
(test (> 0.0 -1e+0) #t)
(test (> 0.0  1e-0) #f)
(test (> 0.0 -1e-0) #t)
(test (> 0.0  1e+100) #f)
(test (> 0.0 -1e+100) #t)
(test (> 0.0  1e-100) #f)
(test (> 0.0 -1e-100) #t)
(test (> 0.0  1e+10000) #f)
(test (> 0.0 -1e+10000) #t)
(test (> 0.0  1e-10000) #f)
(test (> 0.0 -1e-10000) #t)
(test (>  1.0e14  1.0e14) #f)
(test (>  1.0e14 -1.0e14) #t)
(test (> -1.0e14  1.0e14) #f)
(test (> -1.0e14 -1.0e14) #f)
(test (>  1.0e14  1.1e14) #f)
(test (>  1.0e14 -1.1e14) #t)
(test (> -1.0e14  1.1e14) #f)
(test (> -1.0e14 -1.1e14) #t)
(test (>  1.0e-14  1.0e-14) #f)
(test (>  1.0e-14 -1.0e-14) #t)
(test (> -1.0e-14  1.0e-14) #f)
(test (> -1.0e-14 -1.0e-14) #f)
(test (>  1.0e-14  1.1e-14) #f)
(test (>  1.0e-14 -1.1e-14) #t)
(test (> -1.0e-14  1.1e-14) #f)
(test (> -1.0e-14 -1.1e-14) #t)
(test (>  1234567890123.4  1234567890123) #t)
(test (>  1234567890123.4 -1234567890123) #t)
(test (> -1234567890123.4  1234567890123) #f)
(test (> -1234567890123.4 -1234567890123) #f)
(test (>  1234567890123  1234567890123.4) #f)
(test (>  1234567890123 -1234567890123.4) #t)
(test (> -1234567890123  1234567890123.4) #f)
(test (> -1234567890123 -1234567890123.4) #t)
(test (> 10.0 1.0 0.1 0.01 1e-3 1e-4 1e-5) #t)
(test (> 10.0 1.0 0.1 0.01 1e-3 1e-4 1e-4) #f)
(test (> 1.0 1.0 1.0 1.0 1.0 1.0) #f)

(test (>=  0.0  0.0) #t)
(test (>= -0.0  0.0) #t)
(test (>=  0.0 -0.0) #t)
(test (>= -0.0 -0.0) #t)
(test (>=  1.0  1.0) #t)
(test (>= -1.0  1.0) #f)
(test (>=  1.0 -1.0) #t)
(test (>= -1.0 -1.0) #t)
(test (>=  0.1  0.1) #t)
(test (>= -0.1  0.1) #f)
(test (>=  0.1 -0.1) #t)
(test (>= -0.1 -0.1) #t)
(test (>=  123.45e+67  123.45e+67) #t)
(test (>=  123.45e+67  123.45e-67) #t)
(test (>=  123.45e-67  123.45e+67) #f)
(test (>=  123.45e-67  123.45e-67) #t)
(test (>=  123.45e+67 -123.45e+67) #t)
(test (>=  123.45e+67 -123.45e-67) #t)
(test (>=  123.45e-67 -123.45e+67) #t)
(test (>=  123.45e-67 -123.45e-67) #t)
(test (>= -123.45e+67  123.45e+67) #f)
(test (>= -123.45e+67  123.45e-67) #f)
(test (>= -123.45e-67  123.45e+67) #f)
(test (>= -123.45e-67  123.45e-67) #f)
(test (>= -123.45e+67 -123.45e+67) #t)
(test (>= -123.45e+67 -123.45e-67) #f)
(test (>= -123.45e-67 -123.45e+67) #t)
(test (>= -123.45e-67 -123.45e-67) #t)
(test (>=  0.0 0.0) #t)
(test (>=  1.0 0.0) #t)
(test (>= -1.0 0.0) #f)
(test (>=  0.1 0.0) #t)
(test (>= -0.1 0.0) #f)
(test (>=  1e+0 0.0) #t)
(test (>= -1e+0 0.0) #f)
(test (>=  1e-0 0.0) #t)
(test (>= -1e-0 0.0) #f)
(test (>=  1e+100 0.0) #t)
(test (>= -1e+100 0.0) #f)
(test (>=  1e-100 0.0) #t)
(test (>= -1e-100 0.0) #f)
(test (>=  1e+10000 0.0) #t)
(test (>= -1e+10000 0.0) #f)
(test (>=  1e-10000 0.0) #t)
(test (>= -1e-10000 0.0) #f)
(test (>= 0.0  0.0) #t)
(test (>= 0.0  1.0) #f)
(test (>= 0.0 -1.0) #t)
(test (>= 0.0  0.1) #f)
(test (>= 0.0 -0.1) #t)
(test (>= 0.0  1e+0) #f)
(test (>= 0.0 -1e+0) #t)
(test (>= 0.0  1e-0) #f)
(test (>= 0.0 -1e-0) #t)
(test (>= 0.0  1e+100) #f)
(test (>= 0.0 -1e+100) #t)
(test (>= 0.0  1e-100) #f)
(test (>= 0.0 -1e-100) #t)
(test (>= 0.0  1e+10000) #f)
(test (>= 0.0 -1e+10000) #t)
(test (>= 0.0  1e-10000) #f)
(test (>= 0.0 -1e-10000) #t)
(test (>=  1.0e14  1.0e14) #t)
(test (>=  1.0e14 -1.0e14) #t)
(test (>= -1.0e14  1.0e14) #f)
(test (>= -1.0e14 -1.0e14) #t)
(test (>=  1.0e14  1.1e14) #f)
(test (>=  1.0e14 -1.1e14) #t)
(test (>= -1.0e14  1.1e14) #f)
(test (>= -1.0e14 -1.1e14) #t)
(test (>=  1.0e-14  1.0e-14) #t)
(test (>=  1.0e-14 -1.0e-14) #t)
(test (>= -1.0e-14  1.0e-14) #f)
(test (>= -1.0e-14 -1.0e-14) #t)
(test (>=  1.0e-14  1.1e-14) #f)
(test (>=  1.0e-14 -1.1e-14) #t)
(test (>= -1.0e-14  1.1e-14) #f)
(test (>= -1.0e-14 -1.1e-14) #t)
(test (>=  1234567890123.4  1234567890123) #t)
(test (>=  1234567890123.4 -1234567890123) #t)
(test (>= -1234567890123.4  1234567890123) #f)
(test (>= -1234567890123.4 -1234567890123) #f)
(test (>=  1234567890123  1234567890123.4) #f)
(test (>=  1234567890123 -1234567890123.4) #t)
(test (>= -1234567890123  1234567890123.4) #f)
(test (>= -1234567890123 -1234567890123.4) #t)
(test (>= 10.0 1.0 0.1 0.01 1e-3 1e-4 1e-5) #t)
(test (>= 10.0 1.0 0.1 0.01 1e-3 1e-4 1e-4) #t)
(test (>= 1.0 1.0 1.0 1.0 1.0 1.0) #t)

(test (abs 1.234567890) 1.23456789)
(test (abs 1.234567890) 1.23456789)
(test (abs 0.0) 0.0)
(test (abs -0.0) 0.0)

(test (< 1.570796320 (acos 0.00) 1.570796329) #t)
(test (< 1.318116070 (acos 0.25) 1.318116079) #t)
(test (< 1.047197550 (acos 0.50) 1.047197559) #t)
(test (< 0.722734240 (acos 0.75) 0.722734249) #t)
(test (< 3.141592650 (acos -1.00) 3.141592659) #t)
(test (< 2.418858400 (acos -0.75) 2.418858409) #t)
(test (< 2.094395100 (acos -0.50) 2.094395109) #t)
(test (< 1.823476580 (acos -0.25) 1.823476589) #t)
(test (acos 1) 0)

(test (asin 0) 0.0)
(test (< 0.252680250 (asin 0.25) 0.252680259) #t)
(test (< 0.523598770 (asin 0.50) 0.523598779) #t)
(test (< 0.848062070 (asin 0.75) 0.848062079) #t)
(test (< 1.570796320 (asin 1.00) 1.570796329) #t)
(test (< -1.570796329 (asin -1.00) -1.570796320) #t)
(test (< -0.848062079 (asin -0.75) -0.848062070) #t)
(test (< -0.523598779 (asin -0.50) -0.523598770) #t)
(test (< -0.252680259 (asin -0.25) -0.252680250) #t)

(test (atan 0) 0.0)
(test (< 0.244978660 (atan 0.25) 0.244978669) #t)
(test (< 0.463647600 (atan 0.50) 0.463647610) #t)
(test (< 0.643501100 (atan 0.75) 0.643501109) #t)
(test (< 0.785398160 (atan 1.00) 0.785398169) #t)
(test (< -0.244978669 (atan -0.25) -0.244978660) #t)
(test (< -0.463647610 (atan -0.50) -0.463647600) #t)
(test (< -0.643501109 (atan -0.75) -0.643501100) #t)
(test (< -0.785398169 (atan -1.00) -0.785398160) #t)

(test (ceiling  0.0) 0.0)
(test (ceiling  1.0) 1.0)
(test (ceiling -1.0) -1.0)
(test (ceiling  1.1) 2.0)
(test (ceiling  1.4) 2.0)
(test (ceiling  1.5) 2.0)
(test (ceiling  1.9) 2.0)
(test (ceiling -1.1) -1.0)
(test (ceiling -1.4) -1.0)
(test (ceiling -1.5) -1.0)
(test (ceiling -1.9) -1.0)

(define pi 3.14159265358979323846264338327950288419716939937510)
(define pi/4  (/ pi 4))
(define pi/2  (/ pi 2))
(define 3pi/4 (+ pi/2 pi/4))
(define 3pi/2 (+ pi pi/2))
(define 5pi/4 (+ pi pi/4))
(define 7pi/4 (+ pi 3pi/4))
(define 2pi   (+ pi pi))

(test (cos   0.0) 1.0)
(test (cos  pi/2) 0.0)
(test (cos  pi  ) -1.0)
(test (cos 3pi/2) 0.0)
(test (cos 2pi  ) 1.0)
(test (<  0.7071067810 (cos  pi/4)  0.7071067819) #t)
(test (< -0.7071067819 (cos 3pi/4) -0.7071067810) #t)
(test (< -0.7071067819 (cos 5pi/4) -0.7071067810) #t)
(test (<  0.7071067810 (cos 7pi/4)  0.7071067819) #t)
(test (<  0.1699671420 (cos   1.4)  0.1699671430) #t)
(test (< -0.9422223409 (cos   2.8) -0.9422223400) #t)
(test (< -0.4902608219 (cos   4.2) -0.4902608210) #t)
(test (<  0.7755658780 (cos   5.6)  0.7755658789) #t)

(test (exact->inexact  #e0.0) #i0)
(test (exact->inexact  #i0.0) #i0)
(test (exact->inexact #e-0.0) #i0)
(test (exact->inexact #i-0.0) #i0)
(test (exact->inexact  #e1.0) #i1)
(test (exact->inexact  #i1.0) #i1)
(test (exact->inexact #e-1.0) #i-1)
(test (exact->inexact #i-1.0) #i-1)
(test (exact->inexact  #e1.0) #i1.0)
(test (exact->inexact  #i1.1) #i1.1)
(test (exact->inexact #e-1.0) #i-1.0)
(test (exact->inexact #i-1.1) #i-1.1)
(test (exact? (exact->inexact  0)) #f)
(test (exact? (exact->inexact  1)) #f)
(test (exact? (exact->inexact -1)) #f)
(test (exact? (exact->inexact  12345678901234567890)) #f)
(test (exact? (exact->inexact -12345678901234567890)) #f)
(test (exact? (exact->inexact  0.0)) #f)
(test (exact? (exact->inexact -0.0)) #f)
(test (exact? (exact->inexact  0.1)) #f)
(test (exact? (exact->inexact -0.1)) #f)
(test (exact? (exact->inexact  1.0)) #f)
(test (exact? (exact->inexact -1.0)) #f)
(test (exact? (exact->inexact  1234567890.1234)) #f)
(test (exact? (exact->inexact -1234567890.1234)) #f)
(test (exact? (exact->inexact  0.1234567890123)) #f)
(test (exact? (exact->inexact -0.1234567890123)) #f)
(test (exact? (exact->inexact  1.2345e+100)) #f)
(test (exact? (exact->inexact  1.2345e-100)) #f)
(test (exact? (exact->inexact -1.2345e+100)) #f)
(test (exact? (exact->inexact -1.2345e-100)) #f)

(test (exact?  0) #t)
(test (exact?  1) #t)
(test (exact? -1) #t)
(test (exact?  12345678901234567890) #t)
(test (exact? -12345678901234567890) #t)
(test (exact?  0.0) #f)
(test (exact? -0.0) #f)
(test (exact?  0.1) #f)
(test (exact? -0.1) #f)
(test (exact?  1.0) #f)
(test (exact? -1.0) #f)
(test (exact?  1234567890.1234) #f)
(test (exact? -1234567890.1234) #f)
(test (exact?  0.1234567890123) #f)
(test (exact? -0.1234567890123) #f)
(test (exact?  1.2345e+100) #f)
(test (exact?  1.2345e-100) #f)
(test (exact? -1.2345e+100) #f)
(test (exact? -1.2345e-100) #f)
(test (exact?  #i0) #f)
(test (exact?  #i1) #f)
(test (exact? #i-1) #f)
(test (exact?  #i12345678901234567890) #f)
(test (exact? #i-12345678901234567890) #f)
(test (exact?  #i0.0) #f)
(test (exact? #i-0.0) #f)
(test (exact?  #i0.1) #f)
(test (exact? #i-0.1) #f)
(test (exact?  #i1.0) #f)
(test (exact? #i-1.0) #f)
(test (exact?  #i1234567890.1234) #f)
(test (exact? #i-1234567890.1234) #f)
(test (exact?  #i0.1234567890123) #f)
(test (exact? #i-0.1234567890123) #f)
(test (exact?  #i1.2345e+100) #f)
(test (exact?  #i1.2345e-100) #f)
(test (exact? #i-1.2345e+100) #f)
(test (exact? #i-1.2345e-100) #f)

(test (exp 0) 1.0)
(test (< 1.6487212700 (exp 0.5) 1.6487212709) #t)
(test (< 2.7182818280 (exp 1.0) 2.7182818289) #t)
(test (< 7.3890560980 (exp 2.0) 7.3890560990) #t)
(test (< 20.085536920 (exp 3.0) 20.085536929) #t)

(test (expt 2.0 2.0) 4.0)
(test (expt 0. 2) 0.0)
(test (expt 2.0 0) 1)
(test (expt 2.0 1) 2.0)
(test (expt 2.0 2) 4.0)
(test (expt 2.0 3) 8.0)
(test (expt -2.0 3) -8.0)
(test (expt -2.0 4) 16.0)
(test (expt 2.5 5) 97.65625)
(test (expt -2.5 5) -97.65625)
(test (expt 0 0) 1)
(test (expt 0 1) 0)
(test (expt 0 0.1) 0)
(test (number? (expt 0 0.0)) #f)
(test (number? (expt 0 -0.1)) #f)

(test (expt  1 -1) 1.0)
(test (expt  2 -1) 0.5)
(test (expt -2  -1) -0.5)
(test (expt  2  -1) 0.5)
(test (expt  2  -2) 0.25)
(test (expt -2  -2) 0.25)
(test (expt  2  -3) 0.125)
(test (expt -2  -3) -0.125)
(test (expt  2 -10) 0.0009765625)
(test (expt -2 -10) 0.0009765625)
(test (expt 10 -1000) 1.0e-1000)

(test (floor  0.0) 0.0)
(test (floor  1.0) 1.0)
(test (floor -1.0) -1.0)
(test (floor  1.1) 1.0)
(test (floor  1.4) 1.0)
(test (floor  1.5) 1.0)
(test (floor  1.9) 1.0)
(test (floor -1.1) -2.0)
(test (floor -1.4) -2.0)
(test (floor -1.5) -2.0)
(test (floor -1.9) -2.0)

(test (inexact->exact  #e0.0) 0)
(test (inexact->exact  #i0.0) 0)
(test (inexact->exact #e-0.0) 0)
(test (inexact->exact #i-0.0) 0)
(test (inexact->exact  #e1.0) 1)
(test (inexact->exact  #i1.0) 1)
(test (inexact->exact #e-1.0) -1)
(test (inexact->exact #i-1.0) -1)
(test (inexact->exact  #e1.0) 1)
(test (inexact->exact  #i1.0) 1)
(test (inexact->exact #e-1.0) -1)
(test (inexact->exact #i-1.0) -1)
(test (exact? (inexact->exact  #i0)) #t)
(test (exact? (inexact->exact  #i1)) #t)
(test (exact? (inexact->exact #i-1)) #t)
(test (exact? (inexact->exact  #i1234567890)) #t)
(test (exact? (inexact->exact #i-1234567890)) #t)
(test (exact? (inexact->exact  #i0.0)) #t)
(test (exact? (inexact->exact #i-0.0)) #t)
(test (exact? (inexact->exact  #i1.0)) #t)
(test (exact? (inexact->exact #i-1.0)) #t)

(test (inexact?  0) #f)
(test (inexact?  1) #f)
(test (inexact? -1) #f)
(test (inexact?  12345678901234567890) #f)
(test (inexact? -12345678901234567890) #f)
(test (inexact?  0.0) #t)
(test (inexact? -0.0) #t)
(test (inexact?  0.1) #t)
(test (inexact? -0.1) #t)
(test (inexact?  1.0) #t)
(test (inexact? -1.0) #t)
(test (inexact?  1234567890.1234) #t)
(test (inexact? -1234567890.1234) #t)
(test (inexact?  0.1234567890123) #t)
(test (inexact? -0.1234567890123) #t)
(test (inexact?  1.2345e+100) #t)
(test (inexact?  1.2345e-100) #t)
(test (inexact? -1.2345e+100) #t)
(test (inexact? -1.2345e-100) #t)
(test (inexact?  #i0) #t)
(test (inexact?  #i1) #t)
(test (inexact? #i-1) #t)
(test (inexact?  #i12345678901234567890) #t)
(test (inexact? #i-12345678901234567890) #t)
(test (inexact?  #i0.0) #t)
(test (inexact? #i-0.0) #t)
(test (inexact?  #i0.1) #t)
(test (inexact? #i-0.1) #t)
(test (inexact?  #i1.0) #t)
(test (inexact? #i-1.0) #t)
(test (inexact?  #i1234567890.1234) #t)
(test (inexact? #i-1234567890.1234) #t)
(test (inexact?  #i0.1234567890123) #t)
(test (inexact? #i-0.1234567890123) #t)
(test (inexact?  #i1.2345e+100) #t)
(test (inexact?  #i1.2345e-100) #t)
(test (inexact? #i-1.2345e+100) #t)
(test (inexact? #i-1.2345e-100) #t)

(test (log 1) 0.0)
(test (< -2.3025850930 (log 0.1) -2.3025850920) #t)
(test (<  0.6931471800 (log 2.0)  0.6931471809) #t)
(test (<  1.0986122880 (log 3.0)  1.0986122889) #t)

(test (min 2 1 -2 -1 3) -2)
(test (exact? (min 2 1 -2 -1 3)) #t)
(test (min -2.0 1 -2 -1 3) -2.0)
(test (inexact? (min -2.0 1 -2 -1 3)) #t)

(test (max 2 -2 5 -1 3) 5)
(test (exact? (max 2 -2 5 -1 3)) #t)
(test (max 2 -2 5 -1 3.0) 5.0)
(test (inexact? (max 2 -2 5 -1 3.0)) #t)

(test (negative? -1.0) #t)
(test (negative? -0.1) #t)
(test (negative?  0.0) #f)
(test (negative? -0.0) #f)
(test (negative?  0.1) #f)
(test (negative?  1.0) #f)
(test (negative? -1e+100) #t)
(test (negative?  1e+100) #f)
(test (negative? -1e-100) #t)
(test (negative?  1e-100) #f)

(test (positive? -1.0) #f)
(test (positive? -0.1) #f)
(test (positive?  0.0) #f)
(test (positive? -0.0) #f)
(test (positive?  0.1) #t)
(test (positive?  1.0) #t)
(test (positive? -1e+100) #f)
(test (positive?  1e+100) #t)
(test (positive? -1e-100) #f)
(test (positive?  1e-100) #t)

(test (round  0.0) 0.0)
(test (round  1.0) 1.0)
(test (round -1.0) -1.0)
(test (round  1.1) 1.0)
(test (round  1.4) 1.0)
(test (round  1.5) 2.0)
(test (round  1.9) 2.0)
(test (round -1.1) -1.0)
(test (round -1.4) -1.0)
(test (round -1.5) -2.0)
(test (round -1.9) -2.0)

(test (sin    0.0) 0.0)
(test (sin   pi/2) 1.0)
(test (sin   pi  ) 0.0)
(test (sin  3pi/2) -1.0)
(test (sin    2pi) 0.0)
(test (<  0.7071067810 (sin  pi/4)  0.7071067819) #t)
(test (<  0.7071067810 (sin 3pi/4)  0.7071067819) #t)
(test (< -0.7071067819 (sin 5pi/4) -0.7071067810) #t)
(test (< -0.7071067819 (sin 7pi/4) -0.7071067810) #t)
(test (<  0.9854497290 (sin   1.4)  0.9854497300) #t)
(test (<  0.3349881500 (sin   2.8)  0.3349881509) #t)
(test (< -0.8715757729 (sin   4.2) -0.8715757720) #t)
(test (< -0.6312666379 (sin   5.6) -0.6312666370) #t)

(test (sqrt 0) 0)
(test (sqrt 1) 1.0)
(test (sqrt 144) 12.0)
(test (sqrt 144.0) 12.0)
(test (sqrt 15241578750190521) 123456789.0)
(test (< 1.4142135620   (sqrt 2) 1.4142135629) #t)
(test (< 11.090536500 (sqrt 123) 11.090536509) #t)
(test (sqrt 15239.9025) 123.45)
(test (sqrt 1e200) 1e100)

(test (tan    0.0) 0.0)
(test (tan   pi/4) 1.0)
(test (tan  3pi/4) -1.0)
(test (tan  5pi/4) 1.0)
(test (tan  7pi/4) -1.0)
(test (tan  2pi  ) 0.0)
(test (<  5.7978837150 (tan 1.4)  5.7978837159) #t)
(test (< -0.3555298319 (tan 2.8) -0.3555298310) #t)
(test (<  1.7777797740 (tan 4.2)  1.7777797749) #t)
(test (< -0.8139432839 (tan 5.6) -0.8139432830) #t)

(test (truncate  0.0) 0.0)
(test (truncate  1.0) 1.0)
(test (truncate -1.0) -1.0)
(test (truncate  1.1) 1.0)
(test (truncate  1.4) 1.0)
(test (truncate  1.5) 1.0)
(test (truncate  1.9) 1.0)
(test (truncate -1.1) -1.0)
(test (truncate -1.4) -1.0)
(test (truncate -1.5) -1.0)
(test (truncate -1.9) -1.0)

(test (zero? -1.0) #f)
(test (zero? -0.1) #f)
(test (zero?  0.0) #t)
(test (zero? -0.0) #t)
(test (zero?  0.1) #f)
(test (zero?  1.0) #f)
(test (zero?  1e+100) #f)
(test (zero?  1e-100) #f)
(test (zero? -1e+100) #f)
(test (zero? -1e-100) #f)

; --- equivalence ---

(test (eqv? 1   1.0) #f)
(test (eqv? 1.0 1  ) #f)
(test (eqv? 1.0 1.0) #t)
(test (equal? 1.0 1  ) #f)
(test (equal? 1   1.0) #f)
(test (equal? 1.0 1.0) #t)

; --- strings ---

(test (number->string 1.0) "1.0")
(test (number->string 123.0) "123.0")
(test (number->string 123.45) "123.45")
(test (number->string 1.23e2) "123.0")
(test (number->string 1.23e5) "123000.0")
(test (number->string 3.1415926535) "3.1415926535")
(test (number->string 123456789.5) "123456789.5")
(test (number->string 1234567890.1) "1.2345678901e+9")
(test (number->string 12345.67e100) "1.234567e+104")
(test (number->string 1.23450) "1.2345")
(test (number->string 0.12345) "0.12345")
(test (number->string 0.012345) "0.012345")
(test (number->string 0.0012345) "0.0012345")
(test (number->string 0.00012345) "0.00012345")
(test (number->string 0.000012345) "1.2345e-5")
(test (number->string 12345e-100) "1.2345e-96")
(test (number->string -1.0) "-1.0")
(test (number->string -123.0) "-123.0")
(test (number->string -123.45) "-123.45")
(test (number->string -3.1415926535) "-3.1415926535")
(test (number->string -123456789.5) "-123456789.5")
(test (number->string -1234567890.1) "-1.2345678901e+9")
(test (number->string -12345.67e100) "-1.234567e+104")
(test (number->string -1.23450) "-1.2345")
(test (number->string -0.12345) "-0.12345")
(test (number->string -0.012345) "-0.012345")
(test (number->string -0.0012345) "-0.0012345")
(test (number->string -0.00012345) "-0.00012345")
(test (number->string -0.000012345) "-1.2345e-5")
(test (number->string -12345e-100) "-1.2345e-96")

(test (string->number "+1 ") #f)
(test (string->number "-1 ") #f)
(test (string->number "0.0") 0.0)
(test (string->number "-0.0") -0.0)
(test (string->number "1.0") 1.0)
(test (string->number "-1.0") -1.0)
(test (string->number "12345.0") 12345.0)
(test (string->number "-12345.0") -12345.0)
(test (string->number "1.2345") 1.2345)
(test (string->number "-1.2345") -1.2345)
(test (string->number "0.12345") 0.12345)
(test (string->number "-0.12345") -0.12345)
(test (string->number "-0.00012345") -0.00012345)
(test (string->number "0.1") 0.1)
(test (string->number "0.01") 0.01)
(test (string->number "0.001") 0.001)
(test (string->number "0.0000000000001") 0.0000000000001)
(test (string->number "1e0") 1.0)
(test (string->number "1e-0") 1.0)
(test (string->number "1e1") 10.0)
(test (string->number "1e2") 100.0)
(test (string->number "1e5") 100000.0)
(test (string->number "1e10") 10000000000.0)
(test (string->number "1e-1") 0.1)
(test (string->number "1e-2") 0.01)
(test (string->number "1e-5") 0.00001)
(test (string->number "1e-10") 0.0000000001)
(test (string->number "123.456e0") 123.456)
(test (string->number "123.456e1") 1234.56)
(test (string->number "123.456e2") 12345.6)
(test (string->number "123.456e3") 123456.0)
(test (string->number "123.456e4") 1234560.0)
(test (string->number "123.456e5") 12345600.0)
(test (string->number "123.456e10") 1234560000000.0)
(test (string->number "-123.456e0") -123.456)
(test (string->number "-123.456e1") -1234.56)
(test (string->number "-123.456e2") -12345.6)
(test (string->number "-123.456e3") -123456.0)
(test (string->number "-123.456e4") -1234560.0)
(test (string->number "-123.456e5") -12345600.0)
(test (string->number "-123.456e10") -1234560000000.0)
(test (string->number "123.456e-1") 12.3456)
(test (string->number "123.456e-2") 1.23456)
(test (string->number "123.456e-3") 0.123456)
(test (string->number "123.456e-4") 0.0123456)
(test (string->number "123.456e-5") 0.00123456)
(test (string->number "123.456e-10") 0.0000000123456)
(test (string->number "-123.456e-1") -12.3456)
(test (string->number "-123.456e-2") -1.23456)
(test (string->number "-123.456e-3") -0.123456)
(test (string->number "-123.456e-4") -0.0123456)
(test (string->number "-123.456e-5") -0.00123456)
(test (string->number "-123.456e-10") -0.0000000123456)
(test (string->number "+123.45e+678") 123.45e678)
(test (string->number "-123.45e+678") -123.45e678)
(test (string->number "+123.45e-678") 123.45e-678)
(test (string->number "-123.45e-678") -123.45e-678)
(test (string->number "1.") 1.0)
(test (string->number ".1") 0.1)
(test (string->number "1.e1") 10.0)
(test (string->number ".1e1") 1.0)
(test (string->number "1000e0") 1e3)
(test (string->number "100e1") 1e3)
(test (string->number "10e2") 1e3)
(test (string->number "1e3") 1e3)
(test (string->number ".1e4") 1e3)
(test (string->number ".01e5") 1e3)
(test (string->number ".001e6") 1e3)
(test (string->number "12345678.901d10") 1.2345678901e+17)
(test (string->number "12345678.901e10") 1.2345678901e+17)
(test (string->number "12345678.901f10") 1.2345678901e+17)
(test (string->number "12345678.901l10") 1.2345678901e+17)
(test (string->number "12345678.901s10") 1.2345678901e+17)
(test (string->number "12345678.901D10") 1.2345678901e+17)
(test (string->number "12345678.901E10") 1.2345678901e+17)
(test (string->number "12345678.901F10") 1.2345678901e+17)
(test (string->number "12345678.901L10") 1.2345678901e+17)
(test (string->number "12345678.901S10") 1.2345678901e+17)
(test (string->number "1 ") #f)
(test (string->number "1.1 ") #f)
(test (string->number "1.1e1 ") #f)
(test (string->number "1e") #f)
(test (string->number "1e+") #f)
(test (string->number "1e-") #f)
(test (string->number "1.e") #f)
(test (string->number "e1") #f)
(test (string->number "+e1") #f)
(test (string->number "-e1") #f)
(test (string->number ".e1") #f)
(test (string->number "+.e1") #f)
(test (string->number "-.e1") #f)
(test (string->number ".") #f)
(test (string->number "1x1") #f)
(test (string->number "1.x1") #f)
(test (string->number "1.1x1") #f)
(test (string->number "#b100") 4)
(test (string->number "#o100") 64)
(test (string->number "#d100") 100)
(test (string->number "#x100") 256)
(test (string->number "#e100.0") 100)
(test (string->number "#i100") 100.0)
(test (string->number "#e100.1") #f)
(test (inexact? (string->number "#i100")) #t)

; --- I/O ---

(define (visibility-check x)
  (if (file-exists? testfile)
      (delete-file testfile))
  (let ((out (open-output-file testfile)))
    (write x out)
    (display #\space out)
    (display x out)
    (display #\space out)
    (write 'the-end out)
    (close-output-port out)
    (let ((in (open-input-file testfile)))
      (let ((vis (read in)))
        (let ((invis (read in)))
          (close-input-port in)
          (list vis invis))))))

(test (visibility-check 1.0) '(1.0 1.0))
(test (visibility-check 12345.6789e+10) '(12345.6789e+10 12345.6789e+10))
(test (visibility-check -12345.6789e+10) '(-12345.6789e+10 -12345.6789e+10))
(test (visibility-check 12345.6789e-10) '(12345.6789e-10 12345.6789e-10))
(test (visibility-check -12345.6789e-10) '(-12345.6789e-10 -12345.6789e-10))

(delete-file testfile)

(cond ((zero? Errors)
        (display "Everything fine!"))
      (else
        (display Errors)
        (if (> Errors 1)
            (display " errors.")
            (display " error."))))
(display #\newline)

(if (file-exists? testfile) (delete-file testfile))
