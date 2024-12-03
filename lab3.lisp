 (defun replace-element (lst idx new-elem)
  (if (null lst)
      nil
      (if (= idx 0)
          (cons new-elem (rest lst))
          (cons (first lst)(replace-element (rest lst) (- idx 1) new-elem)))))

(defun shell-sort (lst n gap i)
  (if (>= gap 1)
      (if (< i n)
          (let ((j i))
            (if (and (>= j gap) (> (nth (- j gap) lst) (nth j lst)))
                (shell-sort (replace-element (replace-element lst j (nth (- j gap) lst))
                                             (- j gap) (nth j lst))
                            n gap (- j gap))
                (shell-sort lst n gap (+ i 1))))
          (shell-sort lst n (floor (/ gap 2)) 0))
      lst))

(defun shell-sort-wrapper (lst)
  (let ((n (length lst)))
    (shell-sort lst n (floor (/ n 2)) 0)))

(defun check-shell-sort (test-name input expected)
  "Execute shell-sort-wrapper on input, compare result with expected and print comparison status."
  (let ((result (shell-sort-wrapper input)))
    (format t "~:[~a FAILED! Expected: ~a Obtained: ~a~;~a PASSED! Expected: ~a Obtained: ~a~]~%"
            (equal result expected)
            test-name expected result)))

(defun test-shell-sort ()
  "Run a series of tests for shell-sort-wrapper."
  (format t "Start testing shell-sort-wrapper function~%")
  (check-shell-sort "Test 1" '(5 3 8 6 2 7 4 1) '(1 2 3 4 5 6 7 8))
  (check-shell-sort "Test 2" '(10 9 8 7 6 5 4 3 2 1) '(1 2 3 4 5 6 7 8 9 10))
  (check-shell-sort "Test 3" '(1 1 1 1 1) '(1 1 1 1 1))
  (check-shell-sort "Test 4" '(100 -10 0 50 -50) '(-50 -10 0 50 100))
  (check-shell-sort "Test 5" '(-1 -2 -3 -4 -5) '(-5 -4 -3 -2 -1))
  (check-shell-sort "Test 6" '(2 2 2 2 1 1 1 1 3 3 3 3) '(1 1 1 1 2 2 2 2 3 3 3 3))
  (format t "Testing completed~%"))

(defun iterative-shell-sort (lst)
  "Sorts a list using an iterative implementation of Shell sort."
  (let ((working-list (copy-list lst))
        (gap (floor (/ (length lst) 2))))
    (loop while (>= gap 1) do
          (loop for i from gap below (length working-list) do
                (let ((tmp (nth i working-list))
                      (j i))
                  (loop while (and (>= (- j gap) 0) (> (nth (- j gap) working-list) tmp)) do
                        (setf (nth j working-list) (nth (- j gap) working-list))
                        (setf j (- j gap)))
                  (setf (nth j working-list) tmp)))
          (setf gap (floor (/ gap 2))))
    working-list))

(defun verify-iterative-shell-sort (test-name input expected)
  "Execute iterative-shell-sort on input, compare result with expected and print comparison status."
  (let ((result (iterative-shell-sort input)))
    (format t "~:[~a FAILED! Expected: ~a Obtained: ~a~;~a PASSED! Expected: ~a Obtained: ~a~]~%"
            (equal result expected)
            test-name expected result)))

(defun run-iterative-shell-sort-tests ()
  "Run a series of updated tests for iterative-shell-sort."
  (format t "Start testing iterative-shell-sort function~%")
  (verify-iterative-shell-sort "Test 1" '(100 20 0 50 80 10 30) '(0 10 20 30 50 80 100))
  (verify-iterative-shell-sort "Test 2" '(7 7 7 7 7) '(7 7 7 7 7)) 
  (verify-iterative-shell-sort "Test 3" '(5 -1 0 -5 10 -10) '(-10 -5 -1 0 5 10))
  (verify-iterative-shell-sort "Test 4" '(11 2 5 3 8) '(2 3 5 8 11)) 
  (verify-iterative-shell-sort "Test 5" '(99 1 9 89 19 29) '(1 9 19 29 89 99)) 
  (verify-iterative-shell-sort "Test 6" '(100000 -100000 500 0 -500) '(-100000 -500 0 500 100000)) 
  (format t "Testing completed~%"))