#' Script to generate the network diagram connecting the various distribution
#' properties.
library(DiagrammeR)
library(DiagrammeRsvg)

# Define the graph using Graphviz DOT language
graph <- grViz("
digraph G {
  graph [layout = dot, rankdir = LR]

  # Define nodes
  quantile [label = 'quantile']
  cdf [label = 'cdf']
  return [label = 'return']
  range [label = 'range']
  realise [label = 'realise']
  median [label = 'median']
  survival [label = 'survival']
  chf [label = 'chf']
  hazard [label = 'hazard']
  density [label = 'density']
  moments [label = 'moments']
  stdev [label = 'stdev']
  kurtosis_exc [label = 'kurtosis_exc']
  odds [label = 'odds']
  pmf [label = 'pmf']
  junction [label = '', shape = point, width = 0, height = 0, style = invis]

  # Define edges
  cdf -> quantile
  quantile -> return
  quantile -> range
  quantile -> realise
  quantile -> median
  cdf -> survival
  survival -> chf
  survival -> junction [arrowhead = none]
  density -> junction [arrowhead = none]
  junction -> hazard
  density -> moments
  moments -> stdev
  stdev -> moments
  moments -> kurtosis_exc
  kurtosis_exc -> moments
  pmf -> odds

  # Arrange nodes
  {rank = same; cdf; density}
}
")

svg <- export_svg(graph)
writeLines(svg, here::here("vignettes/network_diagram.svg"))

# Export `graph` to png
rsvg::rsvg_png(charToRaw(svg), file = here::here("vignettes/network_diagram.png"), width = 1600)
