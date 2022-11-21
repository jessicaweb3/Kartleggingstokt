#library(conflicted)
#conflict_prefer("select", "dplyr")
#conflict_prefer("filter", "dplyr")

### FZ & JT 22.09.2022          OLD ORIGINAL SCRIPT

## shrimp analysis on stomach data (main dataset) including catch survey data of shrimp

# Source the run first script
source("0_setup.R")

# Load cruise data (.rds data file needs to be created through 1_1 and 1_2 if not existing)
load("_data/Survey_trawl_data.rda")

diet_df <- readRDS("_data/Diet_data.rds")
#sdm_list <- readRDS("_data/Statistical_analysis.rda")


# creating a df only relevant for Shrimp (pres/abs & weight 0-Xg) 
# catchweight is both predators and shrimp in this dataset

#By stomach resolution
diet_df2 <- diet_df %>% filter(commonname %in% c("torsk", "hyse", "sei", "vanlig uer", "vanlig ulke")) %>%
    mutate(Prey=ifelse(Prey=="Pandalids","Shrimp","Other"),
           Fjord=case_when(area==4~"KVA",
                           area==3 & location==24~"PORS",
                           area==3~"TAN")) %>% 
    # various important cols such as length of torsk 
    group_by(cruise,Fjord,startyear,serialnumber,latitudestart,
             longitudestart,bottomdepthstart,bottomdepthstop, distance,
             commonname,specimenid,length,Prey, catchweight) %>%  #catchweight can be both for shrimp but also pred. (specify)
    #summing weight (g) per stomach & all other cols
    dplyr::summarise(preyweight=sum(totalweight)*1000) %>%     
    # maybe divide by distance to get density rather than totalweight. 
    #Higher density of shrimp then higher likelihood of being in stomach
    # OR use distance as offset 
    #expanding data to two columns for prey "Other" & "Shrimp"
    pivot_wider(names_from=Prey, values_from=preyweight,values_fill=0) %>% 
    #Proportional biomass (g) of shrimp in stomachs 
    mutate(ShrimpProp=Shrimp/(Shrimp+Other), 
           #binary presence/absence 
           ShrimpPresence=ifelse(Shrimp>0,1,0)) %>% ungroup() 

#### By station resolution ####
#diet_df3 <- diet_df %>% filter(commonname=="torsk") %>% 
#    mutate(Prey=ifelse(Prey=="Pandalids","Shrimp","Other"),
#           Fjord=case_when(area==4~"Kvænangen",
#                           area==3 & location==24~"Porsanger",
#                           area==3~"Tana")) %>% 
#    # various important cols such as length of torsk 
#    group_by(cruise,Fjord,startyear,serialnumber,latitudestart,
#             longitudestart,bottomdepthstart,bottomdepthstop,
#             commonname,Prey, catchweight) %>%
#summing weight (g) per stomach & all other cols
#    dplyr::summarise(preyweight=sum(totalweight)*1000) %>% 
#expanding data to two columns for prey "Other" & "Shrimp"
#    pivot_wider(names_from=Prey, values_from=preyweight,values_fill=0) %>% 
#Proportional biomass (g) of shrimp in stomachs 
#    mutate(ShrimpProp=Shrimp/(Shrimp+Other), 
#binary presence/absence 
#           ShrimpPresence=ifelse(Shrimp>0,1,0)) 

#### fixing ####
#biomass in gram in stomach
diet_df2 <- rename(diet_df2, c("Shrimp_biomass"="Shrimp"))

# add shrimp catches (from survey)
diet_df2 <- left_join(diet_df2,inner_join(catch_dt,station_dt) %>% filter(commonname=="dypvannsreke") %>% 
                          group_by(startyear,serialnumber) %>% 
                          #specifying that we want catchweight for dypvannsreke 
                          dplyr::summarise(ShrimpCatch=sum(catchweight)/mean(distance)))  %>% 
    mutate(ShrimpCatch=ifelse(is.na(ShrimpCatch),0,ShrimpCatch),
           ShrimpCatchPresence=ifelse(ShrimpCatch>0,1,0)) 

#transforming and standardizing data 
diet_df2 <- diet_df2 %>% mutate(
    Depth_mean=ifelse(!is.na(bottomdepthstop),(bottomdepthstart+bottomdepthstop)/2,bottomdepthstart), 
    Depth_std= (Depth_mean-mean(Depth_mean))/sd(Depth_mean),
    ShrimpCatch_std = (ShrimpCatch-mean(ShrimpCatch))/ sd(ShrimpCatch), 
    length_std= (length-mean(length))/sd(length),
    pred_biomass_std= (catchweight-mean(catchweight, na.rm=TRUE))/sd(catchweight, na.rm=TRUE),
    fFjord=as.factor(Fjord),
    fcommonname=as.factor(commonname),
    fCruise= as.factor(diet_df2$cruise))

#adding cruise no to include in stats
diet_df2 <- diet_df2 %>% mutate(cruise_no=case_when(cruise==2018006~1,cruise==2019824~2,cruise==2019825~3),
                                fCruiseNo=as.factor(cruise_no))

#To transform the response for the beta model:
#diet_df2$ShrimpPropT= (diet_df2$ShrimpProp*(nrow(diet_df2)-1) + 0.5) / nrow(diet_df2)


#### plotting basic relationships ####
# shrimp biomass (g) per cruise 
diet_df2 %>% ggplot(aes(cruise,Shrimp_biomass)) + geom_boxplot() +scale_y_continuous(trans="log10")

# Distance and shrimp in stomach data
diet_df2 %>% ggplot(aes(distance,Shrimp_biomass)) + geom_point() +scale_y_continuous(trans="log10") + 
    geom_smooth()

#comparing shrimp weight in catch with pred_biomass 
diet_df2 %>% ggplot(aes(pred_biomass_std,Shrimp_biomass)) + geom_point() + scale_y_continuous(trans="log10")
diet_df2 %>% ggplot(aes(ShrimpCatch_std,Shrimp_biomass)) + geom_point() + scale_y_continuous(trans="log10")

# diet_df2 %>% ggplot(aes(distance,ShrimpProp)) + geom_point() +scale_y_continuous(trans="log10") + geom_smooth()

# shrimp presence per cruise
#diet_df2 %>% ggplot(aes(cruise,ShrimpProp)) + geom_boxplot() +scale_y_continuous(trans="log10")

# shrimp biomass and bottom depth: correlated  
#(similar rel. for stomach res. & station res)
diet_df2 %>% ggplot(aes(Depth_mean,Shrimp_biomass, color=fFjord)) + geom_point() + scale_y_continuous(trans="log10") +
    geom_smooth()+ facet_wrap(~fcommonname)

# shrimp catch (survey data) and Depth
diet_df2 %>% ggplot(aes(Depth_mean,ShrimpCatch)) + geom_point()+ scale_y_continuous(trans="log10") +
    geom_smooth()

# standardized values of catch and depth
diet_df2 %>% ggplot(aes(Depth_std, ShrimpCatch_std )) + geom_point()

# shrimp prop. and bottom depth
#diet_df2 %>% ggplot(aes(Depth_mean,ShrimpProp)) + geom_point() + scale_y_continuous(trans="log10") +geom_smooth()

# cod biomass with shrimp biomass: not much correlation
diet_df2 %>% ggplot(aes(catchweight, Shrimp_biomass)) + geom_point() +scale_y_continuous(trans="log10") +
    geom_smooth()  #not much of a relationship bt cod biom. and shrimp biom. 

# length and predation
diet_df2 %>% ggplot(aes(length,Shrimp_biomass,color=commonname)) + geom_point() +scale_y_continuous(trans="log10") +
    geom_smooth(formula=y~s(x,k=3))# + facet_wrap(~commonname)

diet_df2 %>% ggplot(aes(ShrimpCatch,Shrimp_biomass)) + geom_point() +scale_y_continuous(trans="log10") +
    geom_smooth()

# predation of other vs shrimp
diet_df2 %>% ggplot(aes(Other, Shrimp_biomass)) + geom_point() +scale_y_continuous(trans="log10") +
    geom_smooth() + facet_wrap(~commonname)

diet_df2 %>% ggplot() + geom_point(aes(ShrimpCatch,Shrimp_biomass,color=cruise)) +
    scale_y_continuous(trans="log10") +
    geom_smooth(aes(ShrimpCatch,Shrimp_biomass))

diet_df2 %>% ggplot(aes(Fjord,Shrimp_biomass,fill=cruise)) + geom_boxplot() +scale_y_continuous(trans="log10")

#predators vs. shrimp in stomach
diet_df2 %>% ggplot(aes(commonname, Shrimp_biomass)) + geom_boxplot() + 
    scale_y_continuous(trans = "log10") + facet_wrap(~fFjord)


diet_df2 %>% ggplot() + geom_point(aes(pred_biomass_std, Shrimp_biomass)) + 
    scale_y_continuous(trans = "log10") + facet_wrap(~fFjord)

#checking the amount of observations pres/absence
diet_df2 %>% group_by(Shrimp_biomass) %>% summarise(n=n())
diet_df2 %>% group_by(Other) %>% summarise(n=n())

#### map of shrimp biomass in stomachs ####
basemap(limits=c(20.5,28.8,69.2,71.5))  +
    geom_spatial_point(data=diet_df2,
                       aes(x=longitudestart,y=latitudestart,size=Shrimp_biomass,color=cruise),alpha=0.5,shape=1,stroke=1) +
    theme_minimal() + theme(text=element_text(size=18),legend.position=c(.8,.2)) + # guides(color=FALSE) +
    ylab("") + xlab("") + scale_size_continuous("Number of stomachs",range=c(1,8)) +
    scale_color_viridis_d(name="Survey") + guides(size="none")


#basemap(limits=c(20.5,28.8,69.2,71.5))  +
#    geom_spatial_point(data=diet_df2,
#                       aes(x=longitudestart,y=latitudestart,size=ShrimpProp,color=cruise),alpha=0.5,shape=1,stroke=1) +
#    theme_minimal() + theme(text=element_text(size=18),legend.position=c(.8,.2)) + # guides(color=FALSE) +
#    ylab("") + xlab("") + #scale_size_continuous("Number of stomachs",range=c(1,8)) +
#    scale_color_viridis_d(name="Survey") + guides(size="none")

#basemap(limits=c(20.5,28.8,69.2,71.5))  +
#    geom_spatial_point(data=diet_df2,
#                       aes(x=longitudestart,y=latitudestart,
#                           color=as.factor(ShrimpCatchPresence),fill=as.factor(ShrimpPresence)),
#                       alpha=.8,size=2,shape=21,stroke=2) +
#    theme_minimal() + theme(text=element_text(size=18),legend.position=c(.8,.2)) + # guides(color=FALSE) +
#    ylab("") + xlab("") +
#    scale_color_viridis_d(name="Catch presence-absence") +
#    scale_fill_viridis_d(name="Stomach presence-absence", option="magma")



### Statistical analysis

#### GAM   - no spatial consideration ####
diet_gam <- gam(Shrimp_biomass ~ 0 + s(length,k=3) + s(Depth_std,k=3) + 
                    s(ShrimpCatch,k=3) +
                    Fjord + s(fCruise,bs="re"),
                family=tw(link="log"),
                data=diet_df2)

plot(diet_gam)
summary(diet_gam)

diet_df2$residuals <- residuals(diet_gam,type="pearson")
diet_df2$resdir <- ifelse(diet_df2$residuals>0,"positive","negative")

simRes <- DHARMa::simulateResiduals(diet_gam)
plot(simRes)   #neg. & pos. clustering of residuals show that there is spatial difference 

diet_df2 %>% ggplot(aes(longitudestart,latitudestart,size=abs(residuals),color=resdir)) + geom_point()


#### sdmTMB  - including spatial consideration ####
# distributions:
# 1. weight/density = tweedie - has both poisson and gamma dist
# 2. proportion = beta
# 3. presence - absence = binomial/Bernoulli

require(sdmTMB)
require(INLA)

# get Xkm and Ykm coordinates, convert latitude/longitude into UTM
xy <- data.frame(ID = 1:nrow(diet_df2), X = diet_df2$longitudestart, Y = diet_df2$latitudestart)
coordinates(xy) <- c("X", "Y")
proj4string(xy) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")  
res <- spTransform(xy,CRS(paste("+proj=utm +zone=",35," ellps=WGS84",sep='')))

diet_df2$x.utm <- res$X
diet_df2$y.utm <- res$Y
diet_df2$xkm   <- diet_df2$x.utm / 1000 
diet_df2$ykm   <- diet_df2$y.utm / 1000

#MESH 
#creating the spatial mesh through INLA rather than using sdmTMB standard
Hull <- inla.nonconvex.hull(cbind(diet_df2$xkm, diet_df2$ykm), convex = -.05) 
RangeGuess <- 30  # Not much diff. in converge in model, 
#but 30 produces AIC= 4790.725 & 50 AIC= 4797.714
MaxEdge  <- RangeGuess / 5
mesh     <- inla.mesh.2d(boundary = Hull,
                         max.edge = c(1, 5) * MaxEdge,
                         cutoff = MaxEdge / 5)

mesh$n
mesh_sdm <- make_mesh(diet_df2, c("xkm","ykm"),mesh=mesh)
plot(mesh_sdm)


#### test spatio-temporal structure COUNT ####
D2null <- sdmTMB(ShrimpPropT ~ Fjord + (1|fCruise),  #null model spatial
                 data=diet_df2,
                 spde=mesh_sdm2,
                 spatial_trend = FALSE, 
                 #  time = "startyear",
                 #  fields = "AR1",
                 spatial_only = TRUE,
                 family=Beta(link="logit"),   
                 reml=FALSE)

D2depth <- sdmTMB(ShrimpPropT ~ Fjord + (1|fCruise) + s(Depth_std), # depth only
                  data=diet_df2,
                  spde=mesh_sdm2,          #spde or mesh?
                  spatial_trend = FALSE, 
                  #  time = "startyear",
                  #  fields = "AR1",
                  spatial_only = TRUE,
                  family=Beta(link="logit"),   
                  reml=FALSE)

D2length <- sdmTMB(ShrimpPropT ~ Fjord + (1|fCruise) + s(Depth_std) +
                       s(length_std),  #length & depth model 
                   data=diet_df2,
                   spde=mesh_sdm2,          #spde or mesh?
                   spatial_trend = FALSE, 
                   #  time = "startyear",
                   #  fields = "AR1",
                   spatial_only = TRUE,
                   family=Beta(link="logit"),   
                   reml=FALSE)

D2full <- sdmTMB(ShrimpPropT ~ Fjord + (1|fCruise) + s(Depth_std) +
                     s(length_std) + s(ShrimpCatch_std), #+ s(pred_biomass_std), #full model 
                 data=diet_df2,
                 spde=mesh_sdm2,
                 #offset= diet_df2$distance,
                 spatial_trend = FALSE, 
                 #  time = "startyear",
                 #  fields = "AR1",
                 spatial_only = TRUE,
                 family=Beta(link="logit"),   
                 reml=FALSE)

# Converging issue solutions:
#1) are your variables highly correlated? 
#2) Do you have enough observations to estimate all the terms? 
#3) Try a different optimizer

AIC(D2null, D2depth, D2length, D2full)  #full model does not converge, but lowest AIC? 
summary(D2full)
D2nullpredict <- predict(D2full)
D2nullsim <- simulate(D2null,nsim=250)
dharma_residuals(D2nullsim, D2null)   #not good!


#### presence/absence shrimp #### 
#very sensitive
DP1 <- sdmTMB(ShrimpPresence ~ + s(Depth_std) + s(ShrimpCatch_std) + #s(length_std) + 
                  Fjord #+ (1|fCruise) #+ s(pred_biomass_std)
              , 
              #offset=trawl_df3$distance,
              data=diet_df3,
              spde=mesh_sdm3,
              spatiotemporal="off",
              family=binomial(link = "log"),
              reml=FALSE)

summary(DP1)
DP1predict <- predict(DP1)
DP1sim <- simulate(DP1,nsim=250)
dharma_residuals(DP1sim,DP1)






#### alternatively: weight instead of count, using tweedie  ####
DBnull <- sdmTMB(Shrimp_biomass ~ fFjord, #+ (1|fCruise),  #null model 
                 data=diet_df2,
                 mesh=mesh_sdm,
                 #time = "cruise_no",
                 spatial="on",
                 spatiotemporal = "off",#
                 family=tweedie(link="log"),
                 #control=sdmTMBcontrol(iter.max=5000,eval.max=5000,nlminb_loops = 2),
                 reml=FALSE)

DBdepth <- sdmTMB(Shrimp_biomass ~ fFjord + s(Depth_std, k=3), #depth model
                  data=diet_df2,
                  mesh=mesh_sdm,
                  #time = "cruise_no",
                  spatial="on",
                  spatiotemporal = "off",#
                  family=tweedie(link="log"),
                  #control=sdmTMBcontrol(iter.max=5000,eval.max=5000,nlminb_loops = 2),
                  reml=FALSE)

DBlength <- sdmTMB(Shrimp_biomass ~ fFjord + s(Depth_std, k=3) + s(length_std, k=3), #length model  
                   data=diet_df2,
                   mesh=mesh_sdm,
                   #time = "cruise_no",
                   spatial="on",
                   spatiotemporal = "off",#
                   family=tweedie(link="log"),
                   #control=sdmTMBcontrol(iter.max=5000,eval.max=5000,nlminb_loops = 2),
                   reml=FALSE)

DBfull <- sdmTMB(Shrimp_biomass ~ fFjord + s(Depth_std,k=3) + 
                     s(length_std,k=3) + s(ShrimpCatch_std,k=3), 
                 #fCruiseNo # adding fCruise as RE does not improve model fit!
                 # pred_biomass_std adds nothing. 
                 # both Depth and ShrimpCatch adds to model together
                 data=diet_df2,
                 mesh=mesh_sdm,
                 #offset= diet_df2$distance,  #adding distance or pred_biomass_std as offset - no diff
                 #time = "cruise_no",
                 spatial="on",
                 spatiotemporal = "off",#  "IID",
                 family=tweedie(link="log"),
                 #control=sdmTMBcontrol(iter.max=5000,eval.max=5000,nlminb_loDbops = 2),
                 reml=FALSE)


DBfullpred <- sdmTMB(Shrimp_biomass ~ fFjord + s(Depth_std,k=3) + 
                         s(length_std,k=3) + s(ShrimpCatch_std,k=3) + (1|fcommonname), #fcommonname,  
                     #fCruiseNo # adding fCruise as RE does not improve model fit!
                     # pred_biomass_std adds nothing. 
                     # both Depth and ShrimpCatch adds to model together
                     # AIC is lowest when using fcommonname (categorical)
                     data=diet_df2,
                     mesh=mesh_sdm,
                     #offset= diet_df2$distance,  #adding distance or pred_biomass_std as offset - no diff
                     #time = "cruise_no",
                     spatial="on",
                     spatiotemporal = "off",
                     family=tweedie(link="log"),
                     #control=sdmTMBcontrol(iter.max=5000,eval.max=5000,nlminb_loDbops = 2),
                     reml=FALSE)

#1. compare with temporal fields vs. spatial only  - DONE
#2. compare spatiotemporal=off with IID and AR1 - DONE
#3 Running only with spatial now, as time did not improve fit. 


AIC(DBnull, DBdepth, DBlength, DBfull, DBfullpred)  #full model pred. lowest AIC and converges 
AIC.df <- AIC(DBnull, DBdepth, DBlength, DBfull, DBfullpred)
summary(DBfullpred)
DBfullpredict <- predict(DBfullpred)
DBfullsim <- simulate(DBfullpred, nsim=250)
dharma_residuals(DBfullsim, DBfullpred)

plot_smooth(DBfullpred,select=3)

## add strata from shapefile
# simple fjord strata
simple_strata <- readOGR(dsn="_data/Shapefiles/kystreke_simpl",layer="kystreke_simpl")  
simple_strata <- spTransform(simple_strata,CRS("+proj=longlat +datum=WGS84"))
simple_strata <- subset(simple_strata,Stratum %in% c("K04.KVA","K03.TAN","K03.POR"))

# detailed strata with sub/depth areas
strata <- readOGR(dsn="_data/Shapefiles/Kartleggingsfjorder",layer="fjordmapping")  
strata <- spTransform(strata,CRS("+proj=longlat +datum=WGS84"))
strata$id <- factor(row.names(strata))
strata_df <- tidy(strata)
strata_df <- full_join(strata_df,strata@data)

# predict with prediction grid
# strata system with fjord strata
#merge them to whole fjord areas
require(maptools)
ID <- case_when(str_detect(strata@data$omr,"kven")~"KVA",str_detect(strata@data$omr,"tana") ~"TAN",TRUE~"PORS")
strata_merged <- unionSpatialPolygons(strata,ID)   # SpP is invalid Warning message: ...
ID <- data.frame(ID=unique(ID))
rownames(ID) <- ID$ID
strata_merged <- SpatialPolygonsDataFrame(strata_merged,ID) 

zone = floor((mean(bbox(strata)[1,]) + 180) / 6) + 1
utmCRS = CRS(paste0("+proj=utm +zone=", zone," +datum=WGS84 +units=km +no_defs"))

# above simplified strata are rather coards, use instead merged detailed 
strata_utm <- spTransform(strata_merged,utmCRS)
strata <- strata_merged

#Construct a grid of integration points
points = sp::makegrid(strata_utm,1e5)
pointsSP = sp::SpatialPoints(points,utmCRS)
inside = rep(1,dim(pointsSP@coords)[1])
inside[which(is.na(over(pointsSP,strata_utm)))] = 0
points = points[which(inside==1),]

#Convert back to lat lon
pointsSP = sp::SpatialPoints(points,utmCRS)
pointsXY =  spTransform(pointsSP,CRS("+proj=longlat"))

# match strata polygons with station coordinates
pointsinpoly = over(pointsSP,strata_utm)
#Define data frame with integration points to be returned
pointsUTM = data.frame(points[,1],points[,2],pointsXY@coords[,1],pointsXY@coords[,2],pointsinpoly$ID) #To be returned
colnames(pointsUTM) = c("xkm", "ykm","lon","lat","fjord")

#pointsUTM <- pointsUTM %>% mutate(fjord=case_when(fjord=="KVA")) # only works when fjord=case_when(fjord=="KVA"~1))

# add depth
bathy <- marmap::readGEBCO.bathy("_data/Shapefiles/gebco_2021.nc",res=1)
bathy <- marmap::fortify.bathy(bathy) %>% filter(between(z,-410,-35)) 

depth_df<-data.frame()
for(i in seq_along(strata)) {        # getting info about bottom depth (bathy) for our points             
    bathySP <- sp::SpatialPoints(bathy,CRS("+proj=longlat +datum=WGS84"))
    inside = rep(1,nrow(bathySP@coords)) #@ instead of $ for polygons
    inside[which(is.na(over(bathySP,subset(strata,ID==unique(pointsUTM$fjord)[i]))))] = 0
    bathySP = bathySP[which(inside==1),]
    bathy_fjord = bathy[which(inside==1),]
    
    bathyUTM = spTransform(bathySP,utmCRS)
    fjordUTM = SpatialPoints(subset(pointsUTM,fjord==unique(pointsUTM$fjord)[i])[,c("xkm","ykm")],utmCRS)
    
    dist = gDistance(bathyUTM,fjordUTM, byid=T)
    minDist <- apply(dist, 1, function(x) order(x, decreasing=F)[2])
    
    depth_df <- rbind(depth_df,data.frame(Fjord=unique(pointsUTM$fjord)[i],xkm=fjordUTM@coords[,1],ykm=fjordUTM@coords[,2],Depth_mean=-1*bathy_fjord$z[minDist]))
    
}

predgrid <- inner_join(pointsUTM,depth_df)

#adding predicted shrimp density from other analysis  
pred_shrimpdens <- st_list$pred %>% dplyr::select(xkm, ykm, lon, lat, fjord, est) 
#pred_shrimpdens$sdensity_exp <- exp(pred_shrimpdens$est)  #not do exp

predgrid <- left_join(predgrid, 
                      dplyr::select(pred_shrimpdens, xkm, ykm, lon, lat, fjord, est), #sdensity_exp
                      by=c("xkm", "ykm", "lon", "lat", "fjord"))

#### Create prediction df using ####
#predgrid <-  pointsUTM#cbind(rbind(pointsUTM,pointsUTM,pointsUTM),cruise_no=as.numeric(rep(c(1:3),each=nrow(pointsUTM))))
predgrid$Depth_std = (predgrid$Depth_mean-mean(diet_df2$Depth_mean))/sd(diet_df2$Depth_mean) 
#predgrid$fCruise= NA  #not using this 
predgrid$fFjord = as.factor(predgrid$Fjord)
predgrid$length_std = 0  #change it to something else if you use spatial difference in average length
predgrid$ShrimpCatch_std = 0 # (predgrid$est-mean(predgrid$est))/sd(predgrid$est) #OR Normal dist. mean=0
predgrid$fcommonname <- NA
#predgrid$distance <- mean(diet_df2$distance)   ## add distance if we add it (std or offset) 
# predgrid <- predgrid[rep(1:nrow(predgrid),length(unique(diet_df2$cruise_no))),]
# predgrid$cruise_no <- rep(unique(diet_df2$cruise_no),each=nrow(predgrid)/3)


# predict with prediction grid 
sdm_pred <- predict(DBfullpred, newdata=predgrid, area=1, re_form_iid=~0,return_tmb_object=TRUE) # population-level prediction

# prediction per area
sdm_pred_kva <- predict(DBfullpred, newdata=subset(predgrid,Fjord=="KVA"), area=1, re_form_iid=~0,return_tmb_object=TRUE) #no random effects for cruise
sdm_pred_pors <- predict(DBfullpred, newdata=subset(predgrid,Fjord=="PORS"), area=1, re_form_iid=~0,return_tmb_object=TRUE) #no random effects for cruise
sdm_pred_tan <- predict(DBfullpred, newdata=subset(predgrid,Fjord=="TAN"), area=1, re_form_iid=~0,return_tmb_object=TRUE) #no random effects for cruise

# predictions: est = full model prediction / est_rf = random field predictions / est_non_rf = fixed effect predictions
sdm_pred$data %>%  ggplot() + 
    geom_tile(aes(x = xkm, y = ykm, fill=(exp(est)))) +
    scale_fill_viridis_c(trans="log10") +
    theme_light() + #theme(legend.position = "bottom") +
    labs(fill = "Predicted biomass\n(g/nm)") +
    labs(x = "Longitude", y = "Latitude") #+ 
facet_wrap()

# comparing predicted biomass bt using shrimpCatch = 0 vs. predicted ShrimpCatch from other analysis
data1 <- dplyr::select(sdm_pred$data, xkm, ykm, lon, lat, fjord, est)  
data2 <- dplyr::select(sdm_pred_sdens$data, xkm, ykm, lon, lat, fjord, est) 
data3 <- inner_join(data1, data2, by=c("xkm", "ykm", "lon", "lat", "fjord"))

plot(data3$est.x, data3$est.y) #est.x is est biomass (shrimpcatch=0) & est.y is est biomass (predicted shrimp density)


# estimated shrimp biomass vs. observations shrimp_bio 
sdm_pred_sdens$data %>%ggplot(aes(fcommonname,exp(est),color=fFjord)) + geom_point() + 
    theme_light() + facet_wrap(~fFjord) + 
    geom_point(data=diet_df2,aes(fcommonname,Shrimp_biomass,color=fFjord)) + scale_y_log10()    


## fixed effects predictions w. random effects & include uncertainty 
#(re_form_iid=0 is the turning off the spatial grid because we only look at fixed effects)
# predict for depth 
newdata_df <- diet_df2 %>% group_by(fFjord) %>% 
    dplyr::summarise(Depth_std=seq(min(diet_df2$Depth_std),max(diet_df2$Depth_std),length=100),
                     length_std=0,ShrimpCatch_std=0, fcommonname=NA, xkm=mean(xkm),ykm=mean(ykm))

fe_depth <- predict(DBfullpred,newdata=newdata_df,se_fit=TRUE,re_form=~0,re_form_iid=~0)

fe_depth %>% ggplot(aes(Depth_std,exp(est),color=fFjord)) + geom_line(size=1.5) + 
    geom_ribbon(aes(Depth_std,ymin=exp(est-est_se),ymax=exp(est+est_se),fill=fFjord),alpha=.2) +
    theme_light() +
    geom_point(data=diet_df2,aes(Depth_std,Shrimp_biomass,color=fFjord)) + scale_y_log10()


#shrimp catch prediction (observed data)
newdata_df <- diet_df2 %>% group_by(fFjord) %>% 
    dplyr::summarise(ShrimpCatch_std=seq(min(diet_df2$ShrimpCatch_std),max(diet_df2$ShrimpCatch_std),length=100),
                     length_std=0,Depth_std=0, fcommonname=NA, xkm=mean(xkm),ykm=mean(ykm))

fe_shrimpcatch <- predict(DBfullpred,newdata=newdata_df,se_fit=TRUE,re_form=~0,re_form_iid=~0)

fe_shrimpcatch %>% ggplot(aes(ShrimpCatch_std,exp(est),color=fFjord)) + geom_line(size=1.5) + 
    geom_ribbon(aes(ShrimpCatch_std,ymin=exp(est-est_se),ymax=exp(est+est_se),fill=fFjord),alpha=.2) +
    theme_light() +
    geom_point(data=diet_df2,aes(ShrimpCatch_std,Shrimp_biomass,color=fFjord)) + scale_y_log10()


#Length of predators   
newdata_df <- diet_df2 %>% group_by(fFjord, fcommonname) %>% 
    dplyr::summarise(length_std=seq(min(diet_df2$length_std),max(diet_df2$length_std),length=100), Depth_std=0,
                     ShrimpCatch_std=0,xkm=mean(xkm),ykm=mean(ykm))

fe_length <- predict(DBfullpred,newdata=newdata_df,se_fit=TRUE,re_form=~0)
#re_form_iid=~0, not including this means including random effects (random effect corrects for outliers)


fe_length %>% ggplot(aes(length_std,exp(est),color=fFjord)) + geom_line(size=1.5) + 
    geom_ribbon(aes(length_std,ymin=exp(est-est_se),ymax=exp(est+est_se),fill=fFjord),alpha=.2) +
    theme_light() +
    geom_point(data=diet_df2,aes(length_std,Shrimp_biomass,color=fFjord)) + 
    scale_y_log10() + facet_wrap(~fcommonname)


#length without random effects for fjords join this w. random intercepts themselves 
newdata_df <- diet_df2 %>% group_by(fFjord) %>% 
    dplyr::summarise(length_std=seq(min(diet_df2$length_std),max(diet_df2$length_std),length=100), Depth_std=0,
                     ShrimpCatch_std=0, fcommonname=NA, xkm=mean(xkm),ykm=mean(ykm))

fe_length <- predict(DBfullpred,newdata=newdata_df,se_fit=TRUE,re_form=~0, re_form_iid=~0)

fe_length %>% ggplot(aes(length_std,exp(est),color=fFjord)) + geom_line(size=1.5) + 
    geom_ribbon(aes(length_std,ymin=exp(est-est_se),ymax=exp(est+est_se),fill=fFjord),alpha=.2) +
    theme_light() +
    geom_point(data=diet_df2,aes(length_std,Shrimp_biomass,color=fFjord, shape=fcommonname)) + 
    scale_y_log10() 


# random effects per species  
newdata_df <- diet_df2 %>% group_by(fFjord, fcommonname) %>% 
    dplyr::summarise(length_std=0, Depth_std=0,
                     ShrimpCatch_std=0, xkm=mean(xkm),ykm=mean(ykm))

fe_length <- predict(DBfullpred,newdata=newdata_df,se_fit=TRUE,re_form=~0)

fe_length %>% ggplot(aes(fcommonname,exp(est),color=fFjord)) + 
    geom_point(size= 1.5, position=position_dodge(width=.3)) + 
    geom_errorbar(aes(fcommonname,ymin=exp(est-est_se),ymax=exp(est+est_se),fill=fFjord),alpha=.3, 
                  position=position_dodge(width=.3)) +
    theme_light() + scale_y_log10() + 
    geom_point(data=diet_df2,aes(fcommonname,Shrimp_biomass,color=fFjord), size=0.5, alpha=0.4, 
               position=position_dodge(width=.3)) 



#random effects 
predict(DBfullpred,newdata=newdata_df,se_fit=TRUE,re_form=~0)


## FIX THIS: include the predicted shirmp density already in DBfullpred part ?!?!
# shrimp density (predicted data from previous analysis)
newdata_df <- pred_shrimpdens %>% group_by(fjord) %>% 
    dplyr::summarise(ShrimpCatch_std=seq(min(pred_shrimpdens$est),max(pred_shrimpdens$est),length=100),
                     xkm=mean(xkm),ykm=mean(ykm))

newdata_df <- rename(newdata_df, fFjord= fjord)  
newdata_df$length_std = 0
newdata_df$Depth_std = 0 
newdata_df$fcommonname= NA

fe_shrimpdens <- predict(DBfullpred,newdata=newdata_df,se_fit=TRUE,re_form=~0,re_form_iid=~0)

fe_shrimpdens %>% ggplot(aes(ShrimpCatch_std,exp(est),color=fFjord)) + geom_line(size=1.5) + 
    geom_ribbon(aes(ShrimpCatch_std,ymin=exp(est-est_se),ymax=exp(est+est_se),fill=fFjord),alpha=.2) +
    theme_light() +
    geom_point(data=pred_shrimpdens,aes(ShrimpCatch_std,Shrimp_biomass,color=fFjord)) + scale_y_log10()


# index
index <- get_index(sdm_pred,level=0.95)
# index by area
index_kva <- get_index(sdm_pred_kva,level=0.95)
index_area <- bind_rows(index_kva %>% mutate(index=est/nrow(subset(predgrid,Fjord=="KVA")),
                                             indexCIlo=lwr/nrow(subset(predgrid,Fjord=="KVA")),
                                             indexCIup=upr/nrow(subset(predgrid,Fjord=="KVA")),
                                             Fjord="Kvænangen"))


# uncertainty? still need to implement
require(tmbstan)
m_stan <- tmbstan(S2a$tmb_obj, iter = 200, chains = 1)
print(m_stan, pars = c("b_j", "thetaf", "ln_phi", "omega_s[1]", "epsilon_st[1]"))

# simulate fixed effects only
S2sim <- predict(S2a,return_tmb_object=TRUE,area=1,newdata=newdata_df,tmbstan_model=m_stan) 
S2sim %>% as.data.frame() %>% mutate(fTrawl=newdata_df$fTrawl,fClimate=newdata_df$fClimate) %>% 
    pivot_longer(cols=V1:V500) %>% group_by(fTrawl,fClimate) %>% 
    dplyr::summarise(pred=exp(mean(value)),
                     cil=exp(mean(value)-1.96*sd(value)/sqrt(n())),
                     cih=exp(mean(value)+1.96*sd(value)/sqrt(n())),
                     cil2=exp(quantile(value,.025)))

# 
sdm_output=list(input=diet_df2, model= DBfullpred, pred=sdm_pred, pred_kva=sdm_pred_kva, 
                pred_tan=sdm_pred_tan, pred_pors=sdm_pred_pors, grid=predgrid, mesh=mesh_sdm)

# save all data
save(sdm_output, file = "_data/Diet_analysis.rda", compress = "xz")





