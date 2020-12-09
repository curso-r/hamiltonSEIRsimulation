get_about_text <- function() {
  p(HTML(
    paste0(
      "<p> This visualisation is for a simple, ",
      "standard stochastic model of an epidemic where individuals ",
      "can be in one of four states: S, susceptible to infection; ",
      "E, exposed to the infection, and so infectious, but ",
      "asymptomatic; I, infectious and symptomatic; R, recovered ",
      "from the infection and immune to further infection.<p> ",
      "Exposed and Infectious people are the actors in the system. ",
      "They interact a random number of times each day with ",
      "Susceptible, Exposed, Infectious, and Recovered people. The ",
      "probability that a given interaction is with Susceptible ",
      "person is the fraction of people in the population that are ",
      "Susceptible at that time. When they interact with a ",
      "Susceptible person, the Susceptible person moves to being ",
      "Exposed. An interaction with an Exposed, Infectious or ",
      "Recovered person leads to no change in the system.<p> Exposed ",
      "people stay in that state for an approximately exponentially ",
      "random time, with an average given by the model parameters, ",
      "whereupon they become Infectious. Infectious people stay in ",
      "that state for an approximately exponentially random time, ",
      "with an average given by the model parameters, whereupon they ",
      "become Recovered. Once there are no Exposed or Infectious ",
      "people left, the epidemic has ended.<p> As the system is ",
      "stochastic, significant heterogeneity occurs when the number ",
      "of Exposed and Infectious people is small. When started with ",
      "a small number of Exposed and Infectious people, there is a ",
      "chance that the epidemic dies out before it can get going, ",
      "or that it expands into a full-blown epidemic. Towards the ",
      "end of a full blown epidemic, there is significant ",
      "heterogeneity in the time until it ends. The closer the ",
      "effective replicative value is to 1, the greater this ",
      "variability."
    )
  ))
}

scientific_10 <- function(x) {
  parse(text = gsub("e", "%*%10^", scales::scientific_format()(x)))
}

