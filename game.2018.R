require("rootSolve")

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

sigmoid = function(x,a){
  1/(1+exp(-a*x))
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


world.areas = c(0.25, .45, 0.2, 0.1)
labels = c("F", "f", "c", "i")
names(world.areas) = labels
if (sum(world.areas) != 1) cat("Areas dont add up")

dat = data.frame(A.forest=0, A.farm=0, A.city=0, A.ind=0, demand.food_0=0, produce.farm=0, food.surplus=0, price.food=0, sold.food=0, N.city=0, N.farmers=0, revenue.city=0, revenue.farmer=0, inc.city=0, inc.farmer=0, satiety.city=0, satiety.farmer=0, hdi.city=0, hdi.farmer=0, polit.popularity=0)
Ts = 500

for (t in 2:Ts){

  
  if (t > 2){
    # ============== input decisions ================
    cmd_pol  = readline(prompt=">> ")

    if (cmd_pol != ""){
      cmd_vec = strsplit(cmd_pol, split = " ")[[1]]
      agent = cmd_vec[1]
      command = cmd_vec[2]
      
      if (agent == "P"){        #          v-------- area added to 
        if (command == "a"){    # >> P a F i 20  --- what % of total area <--- take 20% of total area from Forest and add it to industry
          from = cmd_vec[3]     #        ^----- area taken from 
          to = cmd_vec[4]
          amt = min(as.numeric(cmd_vec[5])/100*sum(world.areas), world.areas[from])
          world.areas[from] = world.areas[from] - amt
          world.areas[to]   = world.areas[to]   + amt
        }
        else if (command == "x"){}
      }
      else if (agent == "F"){
        if (command == "f"){    # >> F f xx    --- Farmer retains only xx % of his own food requirement   
          farmer.fulfillment = as.numeric(cmd_vec[3])
        }
        if (command == "p"){    # >> F p xx   --- Farmer decides to used xx kg/hct of fertilizer. Default is 10
          fert.usage = as.numeric(cmd_vec[3])
        }
        if (command == "m"){    # >> F m xx   --- xx % of farmers migrate to city. negative means xx percent migrate FROM cities
          migrants = round(N.farmers*as.numeric(cmd_vec[3])/100)
          N.farmers = N.farmers - migrants
          N.city    = N.city    + migrants
        }
      }
    }  
  }  
  
  # ================= COUNTRY AND PEOPLE ==================
  
  A.forest = world.areas[1]*A.tot
  A.farm   = world.areas[2]*A.tot
  A.city   = world.areas[3]*A.tot
  A.ind    = world.areas[4]*A.tot

  demand.food_0   = (N.city)*K.food_cons_pc   # Kg
  
  # ================== DROUGHT DYNAMICS ===============================
  p.drought = 0.4*sigmoid(A.forest/A.tot - 0.15, -20)  ##### p_drought = f(forest area) TODO: introduce lag #####
  b.drought = rbinom(n = 1, size=1, prob=p.drought)
  # cat(p.drought)
  
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
  satiety.farmer = min(cons.food_pc_farmer/K.food_cons_pc, 1)
  hdi.farmer =  sigmoid(satiety.farmer-0.2, 5) * sigmoid(inc.farmer-3000, .001)  ##### HDI of farmer = f(satiety, income) ####
  
  # ==================  CITY SECTOR DYNAMCIS  =========================
  
  cons.food_pc = supply.food_max*(1-exp(-price.food/K.supply_elast))/N.city # food consumption per cap
  revenue.city = A.ind/N.city * K.rev_industry
  cost.city = cons.food_pc*price.food
  inc.city = revenue.city - cost.city  # city income proportional to per capita industrial area 
  health.city = A.forest/A.ind
  crowding.city = A.city/N.city # health prop to forest area, inv prop to pollution, inv prop to crowding (human density)
  satiety.city = cons.food_pc/K.food_cons_pc
  hdi.city =  sigmoid(satiety.city-0.2, 5) * sigmoid(inc.city-3000, .001) * sigmoid(crowding.city-0.05,20)   ####  HDI of city  = f(satiety, income, health) #####
  
  # ==================  POLITICAL SECTOR DYNAMICS  =========================
  
  tax.collection =  inc.city*N.city*0.20 + inc.farmer*N.farmers*0.05
  polit.popularity = sigmoid(hdi.city*hdi.farmer - 0.2, 10)
  
  country = matrix(nrow=100, ncol=100, data=0)
  
  
  # ================= Append to dataframe ==============
  dat1 = c(A.forest, A.farm, A.city, A.ind, demand.food_0, produce.farm, food.surplus, price.food, sold.food, N.city, N.farmers, revenue.city, revenue.farmer, inc.city, inc.farmer, satiety.city, satiety.farmer, hdi.city, hdi.farmer, polit.popularity)
  dat[t,] = dat1
  # ================= PLOTS ============================
  
  layout(rbind(c(1,1,2,3),c(4,5,6,7)))
  par(cex.lab=1, cex=1)
  
  world = matrix(data = rep(c(1,2,3,4), c(round(A.forest), round(A.farm), round(A.city), round(A.ind))), 
                 nrow=30, 
                 byrow = F)
  b = (which(diff(c(-1,world[1,],10)) > 0))-1
  image(t(world), xaxt="n", yaxt="n", main="World Area", 
        col=c("green4","lightgreen", "grey", "red"))
  axis(side = 1, at = (b[-length(b)]+diff(b)/2)/30, labels = c("Forest", "Farm", "City", "Ind"))
  
  plotArrow = function(x,y,value){
    if (value < 0){
      pch1 = -9660 # <-- dec
      col1 = "red"
    }
    else {
      pch1 = -9650 # <-- inc
      col1 = "green3"
    }
    cex = 1+abs(value)/70
    if (value != 0) points(x=x,y=y, pch=pch1, col=col1, cex=cex)
  }
  
  plot_supply_demand()
  barplot(c(N.city, N.farmers), main="Population", names.arg = c("city", "farm"), ylim=c(0,3000))
  citypopchange=(dat$N.city[nrow(dat)] - dat$N.city[nrow(dat)-1])/dat$N.city[nrow(dat)-1]*100
  farmerpopchange=(dat$N.farmers[nrow(dat)] - dat$N.farmers[nrow(dat)-1])/dat$N.farmers[nrow(dat)-1]*100
  plotArrow(x=0.7,y=2800, citypopchange)
  plotArrow(x=1.9,y=2800, farmerpopchange)
  
  barplot(rbind(c(inc.city, inc.farmer), c(cost.city, invest.farmer)), main="Income", names.arg = c("city", "farm"), ylim=c(0,8000))
  inc_citychange=(dat$inc.city[nrow(dat)] - dat$inc.city[nrow(dat)-1])/dat$inc.city[nrow(dat)-1]*100
  inc_farmchange=(dat$inc.farmer[nrow(dat)] - dat$inc.farmer[nrow(dat)-1])/dat$inc.farmer[nrow(dat)-1]*100
  plotArrow(x=0.7,y=7000,inc_citychange)
  plotArrow(x=1.9,y=7000,inc_farmchange)
  
  barplot(rbind(c(satiety.city, satiety.farmer)), main="Nutrition", names.arg = c("city", "farm"), ylim = c(0,1), border=F)
  satiety_citychange=(dat$satiety.city[nrow(dat)] - dat$satiety.city[nrow(dat)-1])/dat$satiety.city[nrow(dat)-1]*100
  satiety_farmerchange=(dat$satiety.farmer[nrow(dat)] - dat$satiety.farmer[nrow(dat)-1])/dat$satiety.farmer[nrow(dat)-1]*100
  plotArrow(x=0.7,y=0.95,satiety_citychange)
  plotArrow(x=1.9,y=0.95,satiety_farmerchange)
  
  barplot(rbind(c(hdi.city, hdi.farmer)), main="Happiness", names.arg = c("city", "farm"), ylim=c(0,1))
  hdi_citychange=(dat$hdi.city[nrow(dat)] - dat$hdi.city[nrow(dat)-1])/dat$hdi.city[nrow(dat)-1]*100
  hdi_farmerchange=(dat$hdi.farmer[nrow(dat)] - dat$hdi.farmer[nrow(dat)-1])/dat$hdi.farmer[nrow(dat)-1]*100
  
  plotArrow(x=0.7,y=0.95,hdi_citychange)
  plotArrow(x=1.9,y=0.95,hdi_farmerchange)
  
  barplot(polit.popularity*100, main="Politician's\nPopularity", ylim=c(0,100), names.arg = "")
  polit_change=(dat$polit.popularity[nrow(dat)] - dat$polit.popularity[nrow(dat)-1])/dat$polit.popularity[nrow(dat)-1]*100
  plotArrow(x=0.7,y=95,polit_change)
  
}

# TODO: code disasters wth 10 round lag proportional to forest area
