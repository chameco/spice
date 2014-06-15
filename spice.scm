;(module spice (get-stock-prices-range mean get-average-stock-price-range get-friday-stock-prices-range get-average-stock-price get-yearly-summary differences population-standard-deviation sample-standard-deviation)
 (import chicken scheme (srfi 1) utils)
 (use s bricko)

 (define month-days '#(31 28 31 30 31 30 31 31 30 31 30 31))

 (define (get-closing q)
   (string->number (cadr (fetch-contents '(close) q))))

 (define (get-date-closing-pair q)
   (cons
     (cadr (fetch-contents '(date) q))
     (string->number (cadr (fetch-contents '(close) q)))))

 (define (get-stock-prices-range selector symbol start end)
   (let* ([t (http-get (string-append "http://query.yahooapis.com/v1/public/yql?q=select%20*%20from%20yahoo.finance.historicaldata%20where%20symbol%3D%22"
                                     symbol "%22%20and%20startDate%3D%22" start "%22%20and%20endDate%3D%22" end "%22&env=store://datatables.org/alltableswithkeys"))]
          [contents (fetch-contents '(query results) t)])
     (map selector (cdr contents))))

 (define (mean lst)
   (if lst
     (let ([l (length lst)])
      (if (= l 0)
        0
        (/ (reduce + 0 lst) l)))
     #f))

 (define (differences lst)
   (if (null? lst)
     #f
     (if (null? (cdr lst))
       '()
       (cons (abs (- (car lst) (cadr lst))) (differences (cdr lst))))))

 (define (get-average-stock-price-range symbol start end)
   (mean (get-stock-prices-range get-closing symbol start end)))

 (define (get-stock-stability-range symbol start end)
   (let* ([l (get-stock-prices-range get-closing symbol start end)]
          [md (mean (differences l))])
     (if md
       (/ md (mean l))
       #f)))

 (define (get-month dstring)
   (string->number (cadr (s-split "-" dstring))))

 (define (get-day dstring)
   (string->number (caddr (s-split "-" dstring))))

 (define (day-succ? date1 date2)
   (let* ([m1 (get-month date1)]
          [m2 (get-month date2)]
          [md1 (vector-ref month-days (- m1 1))]
          [md2 (vector-ref month-days (- m2 1))]
          [d1 (get-day date1)]
          [d2 (get-day date2)])
     (or
       (and (eqv? m1 m2) (eqv? (+ d1 1) d2))
       (and (eqv? d1 md1) (eqv? (modulo (+ m1 1) 12) m2) (eqv? d2 1)))))

 (define (get-friday-stock-prices-range symbol start end)
   (let ([prices (reverse (get-stock-prices-range get-date-closing-pair symbol start end))])
    (let loop ([l prices])
     (if (null? (cdr l))
       '()
       (if (day-succ? (caar l) (caadr l))
         (loop (cdr l))
         (cons (car l) (loop (cdr l))))))))

 (define (get-average-stock-price symbol month year)
   (let ([d (number->string (vector-ref month-days (- month 1)))]
         [m (if (< month 10)
              (string-append "0" (number->string month))
              (number->string month))]
         [y (number->string year)])
     (get-average-stock-price-range symbol (string-append y "-" m "-1") (string-append y "-" m "-" d))))

 (define (get-yearly-summary symbol year)
   (let ([y (number->string year)])
    (map
      (lambda (m) (get-average-stock-price symbol m year))
      (iota 12 1))))

 (define (square x) (* x x))

 (define (general-standard-deviation lst length-func)
   (let* ([m (mean lst)]
          [sq-dev (map (lambda (x) (square (- x m))) lst)]
          [variance (/ (reduce + 0 sq-dev) (length-func lst))])
     (sqrt variance)))

 (define (population-standard-deviation lst) (general-standard-deviation lst length))
 (define (sample-standard-deviation lst) (general-standard-deviation lst (lambda (lst) (- (length lst) 1))))

 (define (debug x)
   (print x)
   x)

 (define (get-all-net-worths)
   (let loop ([p (http-get "http://archive.fortune.com/magazines/fortune/fortune500/2010/snapshots/2255.html")]
              [i 0])
     (if (< i 500)
       (cons 
         (debug (cons
                  (cadr (reverse (s-split "-" (apply string-append (cdr (fetch-contents '(html head meta title) p))))))
                  (string->number (list->string (remove (lambda (x) (eqv? #\, x)) (string->list (s-trim (caddr (fetch-contents '(tbody (tr 4) (td 1)) (get-by-id "snapTable1" p))))))))))
         (loop (http-get (string-append "http://archive.fortune.com/magazines/fortune/fortune500/2010/snapshots/" (get-attr 'href (fetch-contents '((a 1)) (get-by-id "slidesNav" p)))))
               (+ i 1)))
       '())))

 (define (associate-net-worths-with-prices nw)
   (let loop ([l nw])
    (if (null? l)
      '()
      (cons (debug (list (caar l) (cdar l) (get-stock-stability-range (caar l) "2010-01-01" "2010-12-31")))
            (loop (cdr l))))))

 (define (to-csv stdata)
   (let loop ([l stdata])
    (if (null? l)
      ""
      (string-append (format "~A,~A,~A~%" (caar l) (cadar l) (caddar l)) (loop (cdr l))))))

 (load "stability.scm")
 (display (to-csv STOCK-DATA))

 ;(load "net_worths.scm")
 ;(associate-net-worths-with-prices NET_WORTHS);)
