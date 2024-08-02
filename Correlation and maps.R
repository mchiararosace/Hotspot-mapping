#---
# title: "Dataframe and plots"
#---

load("./data/data.RData")
load("./data/model.RData")



# Load necessary libraries for data manipulation and visualization
library(sf)        
library(dplyr)     
library(ggplot2)  
library(scales)    
library(viridis) 
library(corrplot)
library(readxl)
library(corrplot)

# transform to sf for plot in ggplot


NUTS2 <- st_as_sf(df)
NUTS2$Count[NUTS2$Count==0] <- NA


#Remove non-EU countries
no_EU <- c("AL", "CH", "LI", "ME", "MK", "NO", "RS", "TR") #list of non-EU country codes
no_EU_NUTS2 <- unlist(sapply(no_EU, grep, x=EU$NUTS_ID, value=TRUE, fixed=TRUE)) # Find matching NUTS2 regions for non-EU countries
no_EU_NUTS2 <- c(no_EU_NUTS2, "ES63", "ES64") # remove also Spanish territories Ceuta and Melilla

no_EUmap <- EU %>% 
  subset(EU$NUTS_ID%in%no_EU_NUTS2)

# Calculate expected cases for each NUTS2 region
rj <- sum(df$Count) / sum(df$n_cells)  # Calculate the rate
EU_data <- df %>% 
  group_by(NUTS_ID) %>% 
  mutate(E = n_cells * rj)  # Calculate expected cases by NUTS2 region

# Calculate summary statistics for data and covariates
covariates <- c("av_temp", "av_prec", "human_log", "border_control_posts", "crop_diversity_10km")
df_stats <- sapply(c("Count", "E", covariates), function(v) summary(EU_data[[v]])) %>% 
  t() %>% 
  as.data.frame()  # Create a summary dataframe for statistics

# Prepare data for boxplot visualization
df_bpl <- EU_data %>% 
  st_drop_geometry() %>% 
  ungroup() %>% 
  select(NUTS_ID, !!covariates) %>% 
  mutate(
    across(all_of(covariates),
           ~ scale(.x))) %>% 
  mutate(outlier = case_when(NUTS_ID == "UKI7_UKI3_UKI4_UKI5_UKI6" ~ "UKI",
                             NUTS_ID == "UKG3" ~ "UKG3",
                             TRUE ~ NA)) %>% 
  tidyr::pivot_longer(all_of(covariates), names_to = "variable", values_to = "value")


# Create boxplot for standardized covariates
pl <- ggplot(df_bpl, aes(x = as.factor(variable), y = value, color = as.factor(variable))) +
  geom_boxplot() +
  theme(legend.position = "none") +
  labs(y = "Scaled values", x = "") +
  geom_text(data = df_bpl[df_bpl$variable == "human_pop_dens", ], 
            aes(label = outlier), nudge_x = 0.15)  # Add labels for outliers

# Prepare data for correlation plot
df_cov <- EU_data %>% 
  ungroup() %>% 
  st_drop_geometry() %>% 
  select(all_of(covariates))

# Create correlation plot
M <- cor(df_cov)
colnames(M) <- rownames(M) <- c("Temperature", "Precipitation", "HumanPopDens", "BCPs", "CropDiversity")
corrplot(M, 
         type = "lower",
         method = "color", 
         addCoef.col = "black",
         tl.cex = 0.85,
         tl.col = "black",
         diag = FALSE)



# Create a plot for the number of introductions
ggplot() +
  geom_sf(data = EU, fill = "grey", linewidth = 0.2, aes(color = "non-EU countries")) +
  geom_sf(data = df, aes(fill = Count), linewidth = 0.2) +
  scale_fill_gradientn(colours = c("white", "#FFDA7F", "#AB0000"), 
                       values = c(0, 0.1, 1),
                       name = "Number of first introductions") +
  scale_color_manual("", values = c("grey30"), 
                     guide = guide_legend(override.aes = list(fill = c("grey")))) +
  scale_y_continuous(position = 'right', limits = c(35, 71))+
  theme(legend.title = element_text(size = 12),    # Increase the size of legend title
        legend.text = element_text(size = 11))     # Increase the size of legend texts




# Create plots for covariates

# Temperature
p1 <- ggplot(df) + 
  geom_sf(data = EU, fill = "grey", linewidth = 0.2) +
  geom_sf(aes(fill = av_temp), linewidth = 0.2) +
  scale_fill_gradientn(colours = c("white", "#FFDA7F", "#AB0000"), 
                       values = c(0, 0.1, 1),
                       name = "°C") +
  geom_point(data=NUTS2, aes(size = Count, geometry = geometry), stat = "sf_coordinates", na.rm = TRUE,alpha=0.5, color="black")+
  scale_size(breaks = c(5,10,15,20), labels=c("1-5","6-10","11-15","16-20"))+
  guides(size=guide_legend(title="Pests introductions"))+
  labs(x="Longitude", y="Latitude") +
  geom_sf(data=NUTS2, colour = "black", fill = "transparent") + 
  geom_sf(data = no_EUmap, fill = "grey", color = "black")


# Precipitation
p2 <- ggplot(df) + 
  geom_sf(data = EU, fill = "grey", linewidth = 0.2) +
  geom_sf(aes(fill = av_prec), linewidth = 0.2) +
  scale_fill_viridis(option = "G", name = "mm", direction = -1) +
  geom_point(data=NUTS2, aes(size = Count, geometry = geometry), stat = "sf_coordinates", na.rm = TRUE,alpha=0.5, color="black")+
  scale_size(breaks = c(5,10,15,20), labels=c("1-5","6-10","11-15","16-20"))+
  guides(size=guide_legend(title="Pests introductions"))+
  labs(x="Longitude", y="Latitude") +
  geom_sf(data=NUTS2, colour = "black", fill = "transparent") + 
  geom_sf(data = no_EUmap, fill = "grey", color = "black")


library(cowplot)

plot_grid(p1, p2, 
          labels = c("a", "b"), 
          label_size = 14,
          ncol = 2,
          label_y = 0.9 
)


# Human population density

HP_breaks <- c(0, 10, 100, 1000, 4000)
ggplot(df)+
  geom_sf(aes(fill=human_pop_dens), linewidth=0.2)+
  scale_fill_viridis(option = "D", 
                     breaks = HP_breaks, labels = HP_breaks, 
                     direction= -1, name = "Persons per km?",
                     trans = scales::log_trans())+
  geom_point(data=NUTS2, aes(size = Count, geometry = geometry), stat = "sf_coordinates", na.rm = TRUE,alpha=0.5, color="black")+
  scale_size(breaks = c(5,10,15,20), labels=c("1-5","6-10","11-15","16-20"))+
  guides(size=guide_legend(title="Pests introductions"))+
  labs(x="Longitude", y="Latitude") +
  geom_sf(data=NUTS2, colour = "black", fill = "transparent") + 
  geom_sf(data = no_EUmap, fill = "grey", color = "black")

# Border control posts

##### with legend scaled:
BCP_breaks <- c(0, 0.0001, 0.001, 0.007)
# scale_fill_viridis_c(breaks = BCP_breaks, labels = BCP_breaks, 
#                        direction= -1, name = "",
#                        trans = scales::pseudo_log_trans(sigma = 0.00001))


ggplot(df)+
  geom_sf(aes(fill=border_control_posts), linewidth=0.2)+
  scale_fill_viridis_c(breaks = BCP_breaks, labels = BCP_breaks, 
                       direction= -1, name = "Density of BCPs and CPs",
                       trans = scales::pseudo_log_trans(sigma = 0.00001))+
  geom_point(data=NUTS2, aes(size = Count, geometry = geometry), stat = "sf_coordinates", na.rm = TRUE,alpha=0.5, color="black")+
  scale_size(breaks = c(5,10,15,20), labels=c("1-5","6-10","11-15","16-20"))+
  guides(size=guide_legend(title="Pests introductions"))+
  labs(x="Longitude", y="Latitude") +
  geom_sf(data=NUTS2, colour = "black", fill = "transparent") + 
  geom_sf(data = no_EUmap, fill = "grey", color = "black")




# Crop diversity
ggplot(df)+
  geom_sf(aes(fill=crop_diversity_10km), linewidth=0.2)+
  scale_fill_viridis(option = "D", name="Crop diversity index", direction=-1)+
  geom_point(data=NUTS2, aes(size = Count, geometry = geometry), stat = "sf_coordinates", na.rm = TRUE,alpha=0.5, color="black")+
  scale_size(breaks = c(5,10,15,20), labels=c("1-5","6-10","11-15","16-20"))+
  guides(size=guide_legend(title="Pests introductions"))+
  labs(x="Longitude", y="Latitude") +
  geom_sf(data=NUTS2, colour = "black", fill = "transparent") + 
  geom_sf(data = no_EUmap, fill = "grey", color = "black")
