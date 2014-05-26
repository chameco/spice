(module spice (get-stock-prices-range mean get-average-stock-price-range get-friday-stock-prices-range get-average-stock-price get-yearly-summary differences population-standard-deviation sample-standard-deviation)
 (import chicken scheme (srfi 1))
 (use s bricko)

 (define month-days '#(31 28 31 30 31 30 31 31 30 31 30 31))

 (define (get-closing q)
   (string->number (cadr (fetch-contents '(close) q))))

 (define (get-date-closing-pair q)
   (cons
     (cadr (fetch-contents '(date) q))
     (string->number (cadr (fetch-contents '(close) q)))))

 (define (get-stock-prices-range selector symbol start end)
   (let ([t (http-get (string-append "http://query.yahooapis.com/v1/public/yql?q=select%20*%20from%20yahoo.finance.historicaldata%20where%20symbol%3D%22"
                                     symbol "%22%20and%20startDate%3D%22" start "%22%20and%20endDate%3D%22" end "%22&env=store://datatables.org/alltableswithkeys"))])
     (map selector (cdr (fetch-contents '(query results) t)))))

 (define (mean lst)
   (let ([l (length lst)])
    (if (= l 0)
      0
      (/ (reduce + 0 lst) l))))

 (define (get-average-stock-price-range symbol start end)
   (mean (get-stock-prices-range get-closing symbol start end)))

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

 (define (differences lst)
   (if (null? (cdr lst))
     '()
     (cons (abs (- (car lst) (cadr lst))) (differences (cdr lst)))))

 (define (general-standard-deviation lst length-func)
   (let* ([m (mean lst)]
          [sq-dev (map (lambda (x) (square (- x m))) lst)]
          [variance (/ (reduce + 0 sq-dev) (length-func lst))])
     (sqrt variance)))

 (define (population-standard-deviation lst) (general-standard-deviation lst length))
 (define (sample-standard-deviation lst) (general-standard-deviation lst (lambda (lst) (- (length lst) 1)))))
