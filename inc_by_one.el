;; Given a number represented by a list of digits (e.g. 3713 as (3 7 1 3)),
;; increment it by one and return the result as list of digits.

(defun inc-helper (n c)
  "Helper function that takes a number and carry, and returns result."
  (if (not n)
      ;; If n is nil, we must reach here because c=1.
      ;; In this case we'll return an extra digit (c).
      (list c)
    ;; Otherwise, we start with adding the carry to the last digit.
    (let* ((last-d (car (last n))) ; Take the last digit.
           (last-d-plus (+ c last-d))) ; Plus with carry.
      (if (< last-d-plus 10)
          ;; No further carry, return the list with last digit updated.
          (append (butlast n) (list last-d-plus))
        ;; Or else, there is carry. We flip the last digit t 0,
        ;; and do this recursively on the sublist (all but last digit).
        (append (inc-helper (butlast n) 1) '(0))))))

;; Main function. Incrementing by one simply means initial carry=1.
(defun inc-by-one (n)
  (inc-helper n 1))

;; Example run:
;; (inc-by-one '(1 2)) --> (1 3)
;; (inc-by-one '(2 9)) --> (3 0)
;; (inc-by-one '(9 9)) --> (1 0 0)
