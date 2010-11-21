algorithms = {
    custom: ";; Your code goes here",
    random: ";; Select a random space \n \
(let ((unused (state-unused current-state))) \n \
    (list-ref \n\
     unused \n\
     (random (length unused))))",
    ascending:  ";; Choose the first available spot \n \
(car (state-unused current-state))",
    descending: ";; Choose the last available spot \n \
(last (state-unused current-state))",
    error:  ";; This player will lose \n \
(raise 'some-error)"
}
