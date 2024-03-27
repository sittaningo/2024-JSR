
dag <- dagitty("dag {
               Y <- X <- Z -> Y
               Y <- M <- X -> U
               M <- U
               Y -> C <- X
               Y <- A
               X[exposure]
               Y[outcome]
               M[mediator]
}")
tidy_dagitty(dag)

coordinates(dag) <- list(x = c(X = 2, Y = 4, Z = 3, M = 3, A = 4, U = 2, C = 3),  
                         y = c(X = 2, Y = 2, Z = 0, M = 3, A = 3, U = 3, C = 1))

dag |> 
  ggdag() +
  theme_dag() + 
  theme(plot.background = element_rect(colour = "white", fill = "white"))
