density <- new_representation(connects_to = "cdf")

eval_density <- new_eval_fun(representation = density)
eval_bi_density <- new_eval_bi_fun(representation = density)

enframe_density <- new_enframe_uni(representation)

# Fall back
eval_representation.default

