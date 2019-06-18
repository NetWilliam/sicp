;a. the names of all people who are supervised by Ben Bitdidle, together with their addresses;
(and (supervisor ?x (Ben Bitdiddle)) (address ?x ?addr))
;b. all people whose salary is less than Ben Bitdiddle's, together with their salary and Ben Bitdiddle's salary;
(and (salary (Ben Bitdiddle) ?ben-salary) (salary ?x ?salary) (lisp-value <= ?salary ?ben-salary))
;c. all people who are supervised by someone who is not in the computer division, together with the superivosr's name and job.
(and (not (job ?x (computer . ?div))) (supervisor ?p ?x))
