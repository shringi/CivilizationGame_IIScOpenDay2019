# Sigmoid function --------------------------------------------------------
sigmoid = function(x, a){
  1/(1 + exp(-a*x))
}
# Saturating exponential function -----------------------------------------
sat.exp = function(x, k){
  1 - exp(-x*k)
}
# Drought Function --------------------------------------------------------
x = seq(-1, 1, 0.01)
y = 1*sigmoid(x - 0.15, -20)
plot(x, y,
     xlim = c(0, 1), ylim = c(0, 1),
     type = 'l', main = "P(drought)", xlab = "A.forest/A.tot", ylab = "Probability of Drought")
rm(x)
# yield function ----------------------------------------------------------
library(manipulate)
x = seq(0, 100, 0.1)
manipulate(plot(x, K.farm_yield_0*2*sat.exp(x, K.fert_effectiveness)*(1 - runif(1,0.1,.5)*b.drought),
                xlab = "Fertiliser usage (kg/yr/hct)", ylab = "Farm yield (kg/yr/hct)", type = 'l',
                xlim = c(0, 100), ylim = c(0,1000)),
           K.farm_yield_0 = slider(100, 1000, step = 50, initial = 500),
           K.fert_effectiveness = slider(0, 1, step = 0.01, initial = 1/15),
           b.drought = slider(0, 1, step = 1, initial = 0))

# Demand-Supply Function --------------------------------------------------
library("rootSolve")

plot_supply_demand = function(supply.food_max, K.supply_elast, demand.food_0, K.demand_elast){
  x = seq(1,200,0.1)
  price.food = multiroot(f = function(x){
    supply.food_max*(1 - exp(-x/K.supply_elast)) - demand.food_0*(exp(-x/K.demand_elast))
  }, start = 0)$root

  plot(y = supply.food_max*(1 - exp(-x/K.supply_elast))/1000,
       x = x,
       type = "l",
       ylim = c(0, max(supply.food_max, demand.food_0))/1000,
       xlab = "Price (Rs/Kg)",
       ylab = "Quantity (tons)",
       main = "Food S&D")
  points(y = demand.food_0*exp(-x/K.demand_elast)/1000,
         x = x,
         type = "l",
         col = "blue")
  abline(v = price.food, col = "grey")
}
manipulate(plot_supply_demand(supply.food_max, K.supply_elast, demand.food_0, K.demand_elast),
           supply.food_max = slider(10000, 100000, step = 1000 , initial = 87566 ),
           K.supply_elast = slider(0, 50, step = 1, initial = 20),
           demand.food_0 = slider(10000,1000000, step = 10000, initial = 109500),
           K.demand_elast = slider(0,1000, step = 10, initial = 100))
