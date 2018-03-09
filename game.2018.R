require("rootSolve")


# ======== PARAMETERS =============
A.tot = 900   # hct

K.fert_price = 100  # Rs/kg 
K.food_cons_pc = 200/1000*365    # Kg/yr (xx grams/day * 365 days)
K.food_cons_pc_farmer = 200/1000*365    # Kg/yr (xx grams/day * 365 days)
K.farm_yield_0 = 500 # * f(rain, fertilizer, irrigation?) # kg/hct
K.demand_elast = 100
K.supply_elast = 20
K.rev_industry = 100000
K.fert_effectiveness = 1/15

plot_supply_demand = function(){
  x = seq(1,200,0.1)
  plot(y=supply.food_max*(1-exp(-x/K.supply_elast))/1000,
       x=x, 
       type="l", 
       ylim=c(0, max(supply.food_max, demand.food_0))/1000,
       xlab="Price (Rs/Kg)",
       ylab="Quantity (tons)",
       main="Food S&D")
  points(y=demand.food_0*exp(-x/K.demand_elast)/1000,
         x=x, 
         type="l", 
         col="blue")
  abline(v=price.food, col="grey")
}

# ===== Political decisions ======

world.areas = c(0.25, .45, 0.2, 0.1)
labels = c("F", "f", "c", "i")
names(world.areas) = labels
if (sum(world.areas) != 1) cat("Areas dont add up")

# ===== Farmer decisions ======


for (t in 1:100){

  farmer.fulfillment = 1
  fert.usage = 10  # kg/yr/hct
  
  cmd_pol  = readline(prompt=">> ")
  
  cmd_vec = strsplit(cmd_pol, split = " ")[[1]]
  agent = cmd_vec[1]
  command = cmd_vec[2]
  
  if (agent == "P"){
    if (command == "a"){
      from = cmd_vec[3]
      to = cmd_vec[4]
      amt = min(as.numeric(cmd_vec[5])/100*sum(world.areas), world.areas[from])
      world.areas[from] = world.areas[from] - amt
      world.areas[to]   = world.areas[to]   + amt
    }
    else if (command == "x"){}
  }
  else if (agent == "F"){
    if (command == "f"){
      farmer.fulfillment = as.numeric(cmd_vec[3])
    }
    if (command == "p"){
      fert.usage = as.numeric(cmd_vec[3])
    }
  }
  
  
  
  # ================= COUNTRY AND PEOPLE ==================
  
  A.forest = world.areas[1]*A.tot
  A.farm   = world.areas[2]*A.tot
  A.city   = world.areas[3]*A.tot
  A.ind    = world.areas[4]*A.tot

  N.farmers = 1000
  N.city = 2000
  demand.food_0   = (N.city)*K.food_cons_pc   # Kg
  
  # ==================  FARM SECTOR DYNAMICS  =========================
  
  yield.farm = K.farm_yield_0*2*(1-exp(-fert.usage*K.fert_effectiveness))  #### farm yield = f(fert usage) ####
  cons.food_pc_farmer = K.food_cons_pc* farmer.fulfillment #### farmer cons = f(city demand, yield)  ####
  produce.farm = yield.farm*A.farm 
  food.surplus = produce.farm - N.farmers*cons.food_pc_farmer  # total produce from all farm area (Kg) minus consumption by farmers
  supply.food_max = max(0, food.surplus)
  
  price.food = multiroot(f=function(x){supply.food_max*(1-exp(-x/K.supply_elast))-demand.food_0*(exp(-x/K.demand_elast))}, start = 0)$root   #### Market determined food price ####
  sold.food = supply.food_max*(1-exp(-price.food/K.supply_elast))  # Total produce sold 
  revenue.farmer = sold.food/N.farmers*price.food   # per cap revenue of farmers
  invest.farmer  = (A.farm/N.farmers)*fert.usage*K.fert_price  # per cap cost to farmers
  inc.farmer = revenue.farmer - invest.farmer  
  satiety.farmer = cons.food_pc_farmer/K.food_cons_pc
  hdi.farmer =  satiety.farmer * inc.farmer /2000 # HDI of farmer
  
  # ==================  CITY SECTOR DYNAMCIS  =========================
  
  cons.food_pc = supply.food_max*(1-exp(-price.food/K.supply_elast))/N.city # food consumption per cap
  revenue.city = A.ind/N.city * K.rev_industry
  cost.city = cons.food_pc*price.food
  inc.city = revenue.city - cost.city  # city income proportional to per capita industrial area 
  health.city = (A.forest/A.ind)*(A.city/N.city) # health prop to forest area, inv prop to pollution, inv prop to crowding (human density)
  satiety.city = cons.food_pc/K.food_cons_pc
  hdi.city =  satiety.city * inc.city * health.city/0.225/2000  # HDI of city
  
  # ==================  POLITICAL SECTOR DYNAMICS  =========================
  
  tax.collection =  inc.city*N.city*0.20 + inc.farmer*N.farmers*0.05
  polit.popularity = hdi.city*hdi.farmer
  
  country = matrix(nrow=100, ncol=100, data=0)
  
  
  # ================= PLOTS ============================
  
  layout(rbind(c(1,1,2,3),c(4,5,6,7)))
  par(cex.lab=1, cex=1)
  
  world = matrix(data = rep(c(1,2,3,4), c(round(A.forest), round(A.farm), round(A.city), round(A.ind))), 
                 nrow=30, 
                 byrow = F)
  b = (which(diff(c(-1,world[1,],10)) > 0))-1
  image(t(world), xaxt="n", yaxt="n", main="World Area", 
        col=c("green4","lightgreen", "grey", "red"))
  axis(side = 1, at = (b[-length(b)]+diff(b)/2)/30, labels = c("Forest", "Farm", "City", "Industry"))
  
  plot_supply_demand()
  barplot(c(N.city, N.farmers), main="Population", names.arg = c("city", "farm"))
  
  barplot(rbind(c(inc.city, inc.farmer), c(cost.city, invest.farmer)), main="Income", names.arg = c("city", "farm"))
  barplot(rbind(c(satiety.city, satiety.farmer), c(1-satiety.city, 1-satiety.farmer)), main="Satiety", names.arg = c("city", "farm"), ylim = c(0,1))
  barplot(rbind(c(hdi.city, hdi.farmer)), main="HDI", names.arg = c("city", "farm"))
  barplot(polit.popularity*100, main="Politician's\nApproval")

}


