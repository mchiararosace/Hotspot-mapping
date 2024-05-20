library(dplyr)
library(sf)
library(spdep)
library(INLA)
library(viridis)
library(bigDM)
library(terra)

df <- read_sf(dsn = "./data", layer="EU_data")

new_names <- c(human_pop_dens = "HumanPD" ,
               total_area_km2 = "total_a" ,
               border_control_posts = "BCP",
               crop_diversity_10km = "CropD") 
df <- df %>%
  rename(all_of(new_names)) %>% 
  filter(NUTS_ID!="MT00") %>% # remove Malta
  mutate(human_log = log(human_pop_dens)) # logarithm of the human population density

# Expected cases  
rj <- sum(df$Count)/sum(df$n_cells) #  rate (number of cases divided by population), see paper for the definition of "population"
df <- df %>% 
  mutate(E = n_cells*rj)

# Covariates scaled
covariates <- c("av_temp", "av_prec", "human_log", 
                "border_control_posts", "crop_diversity_10km")
df <- df %>% 
  mutate(across(all_of(covariates),
                ~c(scale(.x))))

#####################################################
# Neighbors
map_sp <- as(df, "Spatial")
## condition: at least 2 neighbors
map.nb2 <- poly2nb(map_sp)
summary(map.nb2)

# connect subgraphs
map.mod <- connect_subgraphs(carto = map_sp, ID.area = "NUTS_ID", nb = map.nb2, plot = FALSE)
map.nb2 <- map.mod$nb
summary(map.nb2)

# 16 regions with 1 link
IDs <- which(sapply(map.nb2, length) == 1)
nbs2 <- nearby(vect(df[IDs,]), vect(df), k=3)
for(i in seq_along(IDs)){
  v1 <- IDs[i]
  v2 <- nbs2[i, c("k2", "k3")]
  map.nb2[[v1]] <- unique(sort(c(map.nb2[[v1]], as.integer(v2))))
  map.nb2[[v1]] <- map.nb2[[v1]][map.nb2[[v1]]!=0]
  for(j in v2) {
    map.nb2[[j]] <- unique(sort(c(map.nb2[[j]], as.integer(v1))))
    map.nb2[[j]] <- map.nb2[[j]][map.nb2[[j]]!=0]
  }
}
summary(map.nb2)

# check if neighbors are in both directions
for(i in seq_along(map.nb2)){
  v2 <- map.nb2[[i]]
  for(j in v2){
    if(!i %in% map.nb2[[j]]){
      print(paste0(i, " is neighbor of ", j, "but not in the opposite direction!!"))
    }
  }
}

nb2INLA("./data/map_nb.adj", map.nb2) #to save


############################################################################

gr <- inla.read.graph(filename = "./data/map_nb.adj") #read the map_nb directly

# Column for the spatial effect
df$S <- c(1:nrow(df))

variables <- c(covariates, "f(S, model = 'bym', graph = gr, 
  scale.model = TRUE, constr = TRUE,
  hyper=list(prec.spatial=list(prior=sdunif),
                prec.unstruct=list(prior=sdunif)))")

# Prior Uniform
sdunif="expression:
 logdens=-log_precision/2;
 return(logdens)"

# model selection function
source("model_select.R")

models_list <- model_select(resp=df$Count, 
                            variables=variables,
                            E=df$E,
                            data=df,
                            family="poisson",
                            n=20,
                            control.predictor=list(compute=FALSE),
                            control.compute = list(
                              config=TRUE, dic=TRUE, cpo=TRUE, waic=TRUE),
                            control.inla = list(strategy = "laplace"),
                            verbose=FALSE)

models_list$sortby_WAIC[1:5,]
saveRDS(models_list, "./models/models_list.RDS")

# Selected the best model:

ff <- Count ~ av_temp + av_prec + human_log + 
  f(S, model = 'bym', graph = gr,  scale.model = TRUE, constr = TRUE,
    hyper=list(prec.spatial=list(prior=sdunif), prec.unstruct=list(prior=sdunif)))

res <- inla(ff,
            family = "poisson", data = df,
            E = df$E,
            control.compute = list(config=TRUE, dic=TRUE, waic=TRUE,
                                   cpo=TRUE),
            control.predictor = list(compute = TRUE),
            control.inla = list(strategy = "laplace"),
            verbose = TRUE)

summary(res)
saveRDS(res, "./models/model_bym.RDS")
