require("rootSolve")
rm(list = ls())
#----- Commands -----------------------------------------------------------------------
# P/F <--- Politician / Farmer
# P a/x  <--- x = do nothing, a = allocate area
# P a XX YY xx <--- take xx% of total area from XX and add it to YY
#     XX/YY : F = forest, f = farm, c = city, i = industry
#
# F f/p/m xx
#   f xx <---- set fulfilment to xx %
#   p xx <---- set fertilizer usage to xx
#   m xx <---- xx % of farmers migrate to city (negative means xx % migrate from city)
#-------------------------------------------------------------------------------------
# CHECK THE functional forms f(). Refer section headings
# Sigmoid function --------------------------------------------------------
sigmoid = function(x, a){
  1/(1 + exp(-a*x))
}
# Saturating exponential function -----------------------------------------
sat.exp = function(x, k){
  1 - exp(-x*k)
}
# ======== PARAMETERS =============
A.tot = 900   # hct
N.farmers = 1500
N.city = 1500
farmer.fulfillment = 1
fert.usage = 10  # kg/yr/hct

K.fert_price = 200  # Rs/kg
K.food_cons_pc = 200/1000*365    # Kg/yr (xx grams/day * 365 days)
K.food_cons_pc_farmer = 200/1000*365    # Kg/yr (xx grams/day * 365 days)
K.farm_yield_0 = 500 # * f(rain, fertilizer, irrigation?) # kg/hct
K.demand_elast = 100
K.supply_elast = 20
K.rev_industry = 100000
K.fert_effectiveness = 1/15
fert.usage.per = 0
M.hdi.city = 0.5/0.5513588;
M.hdi.farmer = 0.5/0.1603679
t.drought = 0

world.areas = c(0.25, .45, 0.2, 0.1)
labels = c("F", "f", "c", "i")
names(world.areas) = labels
if (sum(world.areas) != 1) cat("Areas dont add up")

dat = data.frame(time = 1,
                 A.forest = 0, A.farm = 0, A.city = 0, A.ind = 0, # Area
                 demand.food_0 = 0, produce.farm = 0, food.surplus = 0, supply.food_max = 0, price.food = 0, sold.food = 0,t.drought, # Food
                 N.city = N.city, N.farmers = N.farmers, # Population
                 revenue.city = 0, revenue.farmer = 0, inc.city = 0, inc.farmer = 0, # Finance
                 satiety.city = 0, satiety.farmer = 0, hdi.city = 0.5, hdi.farmer = 0.5, polit.popularity = 0.5) # Social
Ts = 500

for (t in 2:Ts) {
  N.city = dat$N.city[t - 1]
  N.farmers = dat$N.farmers[t - 1]
  if (t > 2) {
    time = t
    # ============== input decisions ================
    cmd_pol  = readline(prompt = ">> ")
    if (cmd_pol != "") {
      cmd_vec = strsplit(cmd_pol, split = " ")[[1]]
      agent = cmd_vec[1]
      command = cmd_vec[2]

      if (agent == "P") {        #          v-------- area added to
        if (command == "a") {    # >> P a F i 20  --- what % of total area <--- take 20% of total area from Forest and add it to industry
          from = cmd_vec[3]     #        ^----- area taken from
          to = cmd_vec[4]
          amt = min(as.numeric(cmd_vec[5])/100*sum(world.areas), world.areas[from])
          world.areas[from] = world.areas[from] - amt
          world.areas[to]   = world.areas[to]   + amt
        }
        else if (command == "x") {}
      }
      else if (agent == "F") {
        if (command == "f") {    # >> F f xx    --- Farmer retains only xx % of his own food requirement
          farmer.fulfillment.per = as.numeric(cmd_vec[3])
          farmer.fulfillment = farmer.fulfillment*(1 + (farmer.fulfillment.per/100))
        }
        if (command == "p") {    # >> F p xx   --- Farmer decides to used xx kg/hct of fertilizer. Default is 10
          fert.usage.per = as.numeric(cmd_vec[3])
          fert.usage = fert.usage*(1 + (fert.usage.per/100))
        }
        if (command == "m") {    # >> F m xx   --- xx % of farmers migrate to city. negative means xx percent migrate FROM cities
          migrants = round(N.farmers*as.numeric(cmd_vec[3])/100)
          N.farmers = N.farmers - migrants
          N.city    = N.city    + migrants
        }
      }
    }
  }

  # ================= COUNTRY AND PEOPLE ==================
  time = t
  A.forest = world.areas[1]*A.tot
  A.farm   = world.areas[2]*A.tot
  A.city   = world.areas[3]*A.tot
  A.ind    = world.areas[4]*A.tot

  demand.food_0   = (N.city)*K.food_cons_pc   # Kg

  # ================== DROUGHT DYNAMICS ===============================
  p.drought = 1*sigmoid(A.forest/A.tot - 0.15, -20)  ##### p_drought = f(forest area) TODO: introduce lag #####
  b.drought = rbinom(n = 1, size = 1, prob = p.drought)
  t.drought = ifelse(b.drought == 1, t, 0)
  # cat(p.drought)

  # ==================  FARM SECTOR DYNAMICS  =========================

  yield.farm = K.farm_yield_0*2*sat.exp(fert.usage, K.fert_effectiveness)*(1 - runif(1,0.1,.5)*b.drought)  #### farm yield = f(fert usage) ####
  cons.food_pc_farmer = K.food_cons_pc*farmer.fulfillment #### farmer cons = f(city demand, yield)  ####
  produce.farm = yield.farm*A.farm
  food.surplus = produce.farm - N.farmers*cons.food_pc_farmer  # total produce from all farm area (Kg) minus consumption by farmers
  supply.food_max = max(0.1, food.surplus)

  price.food = multiroot(f = function(x){
    supply.food_max*sat.exp(x, 1/K.supply_elast) - demand.food_0*(exp(-x/K.demand_elast))
    }, start = 0)$root   #### Market determined food price ####

  sold.food = supply.food_max*sat.exp(price.food, 1/K.supply_elast)  # Total produce sold
  revenue.farmer = sold.food/N.farmers*price.food   # per cap revenue of farmers
  invest.farmer  = (A.farm/N.farmers)*fert.usage*K.fert_price  # per cap cost to farmers
  inc.farmer = revenue.farmer - invest.farmer
  satiety.farmer = min(cons.food_pc_farmer/K.food_cons_pc, 1)
  hdi.farmer =  M.hdi.farmer*sigmoid(satiety.farmer - 0.2, 5) * sigmoid(inc.farmer - 3000, .001)  ##### HDI of farmer = f(satiety, income) ####

  # ==================  CITY SECTOR DYNAMCIS  =========================

  cons.food_pc = supply.food_max*sat.exp(price.food, 1/K.supply_elast)/N.city # food consumption per cap
  revenue.city = A.ind/N.city * K.rev_industry
  cost.city = cons.food_pc*price.food
  inc.city = revenue.city - cost.city  # city income proportional to per capita industrial area
  health.city = A.forest/A.ind
  crowding.city = A.city/N.city # health prop to forest area, inv prop to pollution, inv prop to crowding (human density)
  satiety.city = cons.food_pc/K.food_cons_pc
  hdi.city =  M.hdi.city*sigmoid(satiety.city - 0.2, 5) * sigmoid(inc.city - 3000, .001) * sigmoid(crowding.city - 0.05,20)   ####  HDI of city  = f(satiety, income, health) #####

  # ==================  POLITICAL SECTOR DYNAMICS  =========================

  tax.collection =  inc.city*N.city*0.20 + inc.farmer*N.farmers*0.05
  polit.popularity = sigmoid(hdi.city*hdi.farmer - 0.25, 10)
  country = matrix(nrow = 100, ncol = 100, data = 0)

  # ================= Append to dataframe ==============
  dat1 = c(time,
           A.forest, A.farm, A.city, A.ind,
           demand.food_0, produce.farm, food.surplus, supply.food_max, price.food, sold.food,t.drought,
           N.city, N.farmers,
           revenue.city, revenue.farmer, inc.city, inc.farmer,
           satiety.city, satiety.farmer, hdi.city, hdi.farmer, polit.popularity)
  dat[t,] = dat1
  dat$N.city[t] = (2^(hdi.city - 0.5))*dat$N.city[t - 1]
  dat$N.farmers[t] = (2^(hdi.farmer - 0.5))*dat$N.farmers[t - 1]
  # ================= PLOTS ============================

  layout(rbind(c(1, 1, 2, 3), c(4, 4, 5, 6), c(7, 8, 9, 10), c(11, 12, 13, 14)))
  par(cex.lab = 1.2, cex = 1.2)
  par(mar = c(2,2,2,2))

  world = matrix(data = rep(rep(c(1, 2, 3, 4), c(round(A.forest), round(A.farm), round(A.city), round(A.ind))), each = 30), nrow = 30)
  b = (which(diff(c(-1,world[1,],10)) > 0)) - 1
  image(t(world), xaxt = "n", yaxt = "n", main = "World Area",
        col = c("green4", "lightgreen", "grey", "red"))
  axis(side = 1, at = c(A.forest/2, (A.farm/2) + A.forest, (A.city/2) + A.farm + A.forest, A.tot - (A.ind/2))/A.tot , labels = c("Forest", "Farm", "City", "Ind"))

  plotArrow = function(x,y,value){
    if (value < 0) {
      pch1 = -9660 # <-- dec
      col1 = "red"
    } else {
      pch1 = -9650 # <-- inc
      col1 = "green4"
    }
    cex = 1 #+ abs(value)/70
    if (value != 0) points(x = x, y = y, pch = pch1, col = col1, cex = cex)
  }

  barplot(c(N.city, N.farmers), main = "Population", names.arg = c("city", "farm"), ylim = c(0,3000),
          col = c("black","blue"))
  citypopchange = (dat$N.city[nrow(dat)] - dat$N.city[nrow(dat) - 1])/dat$N.city[nrow(dat) - 1]*100
  farmerpopchange = (dat$N.farmers[nrow(dat)] - dat$N.farmers[nrow(dat) - 1])/dat$N.farmers[nrow(dat) - 1]*100
  plotArrow(x = 0.7, y = 2800, citypopchange)
  plotArrow(x = 1.9, y = 2800, farmerpopchange)

#  plot_supply_demand()
  barplot(rbind(c(sold.food, sold.food),
                c(demand.food_0 - sold.food, supply.food_max - sold.food))/1000,
          main = "Food Production", names.arg = c("demand", "supply"), ylab = "Quantity (tons)",
          ylim = c(0, 150))
  sold.food.change = (dat$sold.food[nrow(dat)] - dat$sold.food[nrow(dat) - 1])/dat$sold.food[nrow(dat) - 1]*100
  plotArrow(x = 0.7, y = 148, sold.food.change)
  plotArrow(x = 1.9, y = 148, sold.food.change)

  # Trend -------------------------------------------------------------------
  plot(dat$A.farm ~ dat$time, xlim = c(2,20), pch = 19, col = "lightgreen", type = "o")
  abline(v = dat$t.drought, col = "red")
  points(dat$A.forest ~ dat$time, xlim = c(2,20), pch = 19, col = "green4", type = "o")

  points(dat$A.city ~ dat$time, xlim = c(2,20), pch = 19, col = "black", type = "o")
  points(dat$A.ind ~ dat$time, xlim = c(2,20), pch = 19, col = "red", type = "o")

  plot(dat$N.city ~ dat$time, xlim = c(2,20), ylim = c(500,2500), pch = 19, col = "black", type = "o")
  points(dat$N.farmers ~ dat$time, xlim = c(2,20), pch = 19, col = "blue", type = "o")

  plot(dat$demand.food_0 ~ dat$time, xlim = c(2,20), pch = 19, col = "black", type = "o")
  points(dat$supply.food_max ~ dat$time, xlim = c(2,20), pch = 19, col = "blue", type = "o")

# 2nd Row -----------------------------------------------------------------
  barplot(price.food, main = "Food Price", ylim = c(0,100), names.arg = "")
  price_change = (dat$price.food[nrow(dat)] - dat$price.food[nrow(dat) - 1])/dat$price.food[nrow(dat) - 1]*100
  plotArrow(x = 0.7, y = 95, price_change)

  barplot(rbind(c(inc.city, inc.farmer), c(cost.city, invest.farmer)), main = "Income", names.arg = c("city", "farm"), ylim = c(0, 8000), col = c("black","grey"))
  inc_citychange = (dat$inc.city[nrow(dat)] - dat$inc.city[nrow(dat) - 1])/dat$inc.city[nrow(dat) - 1]*100
  inc_farmchange = (dat$inc.farmer[nrow(dat)] - dat$inc.farmer[nrow(dat) - 1])/dat$inc.farmer[nrow(dat) - 1]*100
  plotArrow(x = 0.7, y = 7000, inc_citychange)
  plotArrow(x = 1.9, y = 7000, inc_farmchange)

  barplot(rbind(c(satiety.city, satiety.farmer)), main = "Nutrition", names.arg = c("city", "farm"), ylim = c(0,1),col = c("black","blue"), border = F)
  satiety_citychange = (dat$satiety.city[nrow(dat)] - dat$satiety.city[nrow(dat) - 1])/dat$satiety.city[nrow(dat) - 1]*100
  satiety_farmerchange = (dat$satiety.farmer[nrow(dat)] - dat$satiety.farmer[nrow(dat) - 1])/dat$satiety.farmer[nrow(dat) - 1]*100
  plotArrow(x = 0.7, y = 0.95, satiety_citychange)
  plotArrow(x = 1.9, y = 0.95, satiety_farmerchange)

  col.sch = c(ifelse(hdi.city < 0.5, "red", "green"),
              ifelse(hdi.farmer < 0.5, "red", "green"),
              ifelse(polit.popularity < 0.5, "red", "green"))

  barplot(c(hdi.city, hdi.farmer, polit.popularity),
          main = "Happiness", names.arg = c("city", "farm", "polit"), ylim = c(0,1),
          col = col.sch)
  hdi_citychange = (dat$hdi.city[nrow(dat)] - dat$hdi.city[nrow(dat) - 1])/dat$hdi.city[nrow(dat) - 1]*100
  hdi_farmerchange = (dat$hdi.farmer[nrow(dat)] - dat$hdi.farmer[nrow(dat) - 1])/dat$hdi.farmer[nrow(dat) - 1]*100
  polit_change = (dat$polit.popularity[nrow(dat)] - dat$polit.popularity[nrow(dat) - 1])/dat$polit.popularity[nrow(dat) - 1]*100
abline(h = .5, col = "red")
  plotArrow(x = 0.7, y = 0.95, hdi_citychange)
  plotArrow(x = 1.9, y = 0.95, hdi_farmerchange)
  plotArrow(x = 3.1, y = 0.95, polit_change)

  plot(dat$price.food ~ dat$time, xlim = c(2,20), pch = 19, col = "black", type = "o")

  plot(dat$inc.city ~ dat$time, xlim = c(2,20), pch = 19, col = "black", type = "o")
  points(dat$inc.farmer ~ dat$time, xlim = c(2,20), pch = 19, col = "blue", type = "o")

  plot(dat$satiety.city ~ dat$time, xlim = c(2,20), ylim = c(0,1), pch = 19, col = "black", type = "o")
  points(dat$satiety.farmer ~ dat$time, xlim = c(2,20), pch = 19, col = "blue", type = "o")

  plot(dat$hdi.city ~ dat$time, xlim = c(2,20), ylim = c(0,1), pch = 19, col = "black", type = "o")
  points(dat$hdi.farmer ~ dat$time, xlim = c(2,20), pch = 19, col = "blue", type = "o")
  points(dat$polit.popularity ~ dat$time, xlim = c(2,20), pch = 19, col = "red", type = "o")
}

# TODO: Introduce health/ climate consequences for increased industry levels.
