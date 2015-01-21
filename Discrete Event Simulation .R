#title: "Discrete Event Simulation"
#author: "Cesar Munayco"
#date: "January 12, 2015"

---
  
#An example
#Let’s say we want to simulate a very simple patient trajectory; a patient enters the hospital and has to register at the desk, next, is seen by a nurse for an intake and finally is met by the doctor for a consult.

library(simmer)
library(ggplot2)
library(scales)
library(dplyr)
trajectory <- read.table(header=T, text=
                           "event_id  description   resource        amount  duration          successor
                         1         registration  administration  1       runif(1,3,10)     2
                         2         intake        nurse           1       runif(1,10,20)    3
                         3         consultation  doctor          1       runif(1,5,15)     NA"  )

trajectory



t0<-
  create_trajectory("my trajectory") %>%
  ## add an intake event 
  add_seize_event("nurse",1.0) %>%
  add_timeout_event(15) %>%
  add_release_event("nurse",1.0) %>%
  ## add a consultation event
  add_seize_event("doctor",1.0) %>%
  add_timeout_event(20) %>%
  add_release_event("doctor",1.0) %>%
  ## add a planning event
  add_seize_event("administration",1.0) %>%
  add_timeout_event(5) %>%
  add_release_event("administration",1.0)

t1<-
  create_trajectory("my trajectory") %>%
  ## add an intake event 
  add_seize_event("nurse",1.0) %>%
  add_timeout_event("rnorm(1,15)") %>%
  add_release_event("nurse",1.0) %>%
  ## add a consultation event
  add_seize_event("doctor",1.0) %>%
  add_timeout_event("rnorm(1,20)") %>%
  add_release_event("doctor",1.0) %>%
  ## add a planning event
  add_seize_event("administration",1.0) %>%
  add_timeout_event("rnorm(1,5)") %>%
  add_release_event("administration",1.0)


sim<-
  create_simulator("SuperDuperSim", n = 100, until = 80) %>%
  add_resource("nurse", 1) %>%
  add_resource("doctor", 2) %>%
  add_resource("administration", 1)


sim<-
  sim %>%
  add_entities_with_interval(trajectory = t1, n = 10, name_prefix = "patient", interval =  "rnorm(1, 10, 2)")

sim<-
  sim %>%
  add_entity(trajectory = t1, name = "separate_patient" , activation_time =  100)


sim <-
  sim %>%
  simmer()

t2<-
  create_trajectory("trajectory with a skip event") %>%
  ## add a skip event - (50 - 50 chance that the next event is skipped)
  add_skip_event(number_to_skip = "sample(c(0,1),1)") %>%
  add_timeout_event(15) %>%
  add_timeout_event(5)


plot_resource_utilization(sim, c("nurse", "doctor","administration"))

plot_resource_usage(sim, "doctor")

plot_resource_usage(sim, "doctor", 6)

plot_resource_usage(sim, resource_name = "doctor")

head(
  get_resource_monitor_values(sim, "nurse")
)

plot_evolution_entity_times(sim, type = "flow_time")

plot_evolution_entity_times(sim, type = "waiting_time")

plot_evolution_entity_times(sim, type = "activity_time")


head(
  get_entity_monitor_values(sim, aggregated = TRUE)
)


head(
  get_entity_monitor_values(sim, aggregated = T)
)

library(R.oo)
install.packages(file.choose(), repos=NULL, type="source")
library(sim101)
galileo
dump("galileo", file=file.choose(new=TRUE))


data(conway)
## plot after simulation:
plot(sim(conway), delay=100)
## plot during simulation
sim(conway, animate=TRUE, delay=100)
## discrete version of logistic growth equation
## Note: function main returns the *new value*, not the derivative
dlogist <- new("odeModel",
               main = function (time, init, parms, ...) {
                 x <- init
                 with(as.list(parms), {
                   x <- x + r * x * (1 - x / K) * DELTAT
                   #   ^^^ add to old value       ^^^^^^ special parameter with time step
                  list(c(x))
                 })
               },
      parms  = c(r=0.1, K=10),
      times  = seq(0, 100, 1),
      init   = c(population=0.1),
      solver = "iteration" #!!!
  )
plot(sim(dlogist))

## alternative with function that returns the derivative
## discrete steps are realized with the euler method
dlogist <- new("odeModel",
               main = function (time, init, parms, ...) {
                 x <- init
                 with(as.list(parms), {
                   x <- r * x * (1 - x / K)
                   list(c(x))
                 })
               },
               parms  = c(r=0.1, K=10),
               times  = seq(0, 100, 1),
               init   = c(population=0.1),
               solver = "euler"
)
plot(sim(dlogist))


                   