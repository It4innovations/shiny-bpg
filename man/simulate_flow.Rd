% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/simulation.R
\name{simulate_flow}
\alias{simulate_flow}
\title{Simulate flow through network}
\usage{
simulate_flow(
  df_edges,
  random = 0,
  input_people = 100,
  l_events = NULL,
  timestep = NA,
  df_vertices = NULL,
  source_name = "source",
  exit_name = "exit",
  flow_text_side = "left",
  flow_text_distance = 0.12
)
}
\arguments{
\item{df_edges}{data.frame with network defined by edges. It should contain columns `from`, `to`, `weight`, `level`.}

\item{random}{numeric, volatility of the time spend at vertice. When entering a vertex a time defined by the edge weights is multiplied by the `max(1, rnorm(1, 0, random))`.}

\item{input_people}{integer, how many people should enter into the simulation.}

\item{l_events}{list, list of events that should affect the state of the simulation. See details section for more information.}

\item{timestep}{integer, what should be the timestep for the simulation. If set to NA a greatest common divisor of the edges weight is computed and used. Default NA.}

\item{df_vertices}{data.frame, with description of vertices get by the with columns `vertice`, `level`, `x`, `y`. It is an optional parameter since usually this is computed from `df_edges`.}

\item{source_name}{string, name of the source vertice}

\item{exit_name}{string, name of the exit vertice}

\item{flow_text_side}{string, side on which side the flow text should be visualized. One of "left" or "right". Default is "left".}

\item{flow_text_distance}{numeric, how far the text should be from the edge of the network. Distance is a multiple of the network width. Default is 0.12.}
}
\value{
list containing two data.frames
\itemize{
   \item{simulation}{data.frame of the simulation with timesteps identified by the column `snap`}
   \item{flow}{data.frame with the flow for each timestep identified by the column `snap`}
}
}
\description{
Simulate flow through network
}
\examples{

network <- create_source(set = c(2,2), weight = 2) \%>\%
 add_level_set(weight = c(3,1),
               previous_set = c(1,2),
               set = c(1,3)) \%>\%
  add_fully_connected(weight = 3,
                      set = 1) \%>\%
  add_exit()

res <- simulate_flow(network)
df_simulation <- res[[1]]
animate_simulation(network,
                   df_simulation)

}
