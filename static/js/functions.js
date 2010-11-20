algorithms = {
    custom: "",
    random: "(let ((unused (state-unused current-state))) \n\
    (list-ref \n\
     unused \n\
     (random (length unused)))))"
}
