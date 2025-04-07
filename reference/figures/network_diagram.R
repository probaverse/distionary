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
  empty [label = ' ']

  # Define edges
  quantile -> cdf
  return -> quantile
  range -> quantile
  realise -> quantile
  median -> quantile
  survival -> cdf
  chf -> survival
  empty -> survival
  empty -> density
  hazard -> empty
  moments -> density
  stdev -> moments
  moments -> stdev
  kurtosis_exc -> moments
  moments -> kurtosis_exc
  odds -> pmf

  # Arrange nodes
  {rank = same; cdf; density}
}
")

svg <- export_svg(graph)
writeLines(svg, here::here("vignettes/network_diagram.svg"))
