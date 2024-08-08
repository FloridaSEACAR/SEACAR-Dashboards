# Text to declare funding
funding_text <- paste(
  tags$div("Funding Acknowledgement", style="text-align:center; font-weight:bold;"),
  
  "SEACAR is funded, in part, through a grant agreement from the 
  Florida Department of Environmental Protection, Florida Coastal Management 
  Program, by a grant provided by the Office for Coastal Management 
  under the <a href='https://coast.noaa.gov/czm/act/' target='_blank'>
  Coastal Zone Management Act of 1972</a>, as amended, 
  National Oceanic and Atmospheric Administration. 
  The views, statements, findings, conclusions, and recommendations expressed 
  herein are those of the author(s) and do not necessarily reflect the views 
  of the State of Florida, NOAA or any of their subagencies.",
  
  tags$div(
    tags$img(
      src = "www/dep-logos.png",
      alt = "Funding logos",
      height = "100",
      width = "290"
    ),style="text-align:center;"
  ),
  
  tags$div(paste0("Published: ", publish_date), style="text-align:center;"),
  
  sep="<br><br>"
)

funding <- function(){
  tags$div(wellPanel(htmlOutput("funding")), 
           style = 'width=80%;')
}

habitatText <- function(habitat){
  if(habitat=="Submerged Aquatic Vegetation"){
    t <- "Submerged aquatic vegetation (SAV) refers to plants and plant-like 
    macroalgae species that live entirely underwater. The most species-rich 
    categories of SAV inhabiting Florida estuaries are benthic macroalgae and 
    seagrasses which often grow together in dense beds or meadows that carpet the 
    seafloor. Macroalgae include multicellular species of green, red and brown 
    algae that often live attached to the substrate by a root-like structure 
    called a holdfast. They grow quickly and tolerate relatively high nutrient levels, 
    making them a threat to seagrasses and other benthic habitats in areas with 
    poor water quality. One of the most common types of macroalgae in Florida SAV 
    communities are species of the genus Caulerpa."      
  } else if(habitat=="Oyster Reef"){
    t <- "Oyster reefs are living, growing scaffolds of biodiversity that take 
    shape over many years as generations of oysters settle in the same location, 
    growing upward and outward with the accumulation of oyster shells and sediments. 
    Many oyster reefs have survived for centuries or longer while sheltering and 
    nourishing over 100 different species that offer benefits to the ecosystem as 
    well as local economies. The reef’s structure protects shorelines from erosion 
    caused by onshore wave action and alters estuary salinity by impounding 
    freshwater flowing down rivers. In some regions of Florida, mangrove forests 
    emerge on oyster reefs, further diversifying the ecosystem. As filter feeders, 
    oysters process enormous volumes of water and thereby reduce sediment load, 
    improve water clarity and remove potentially dangerous microorganisms and 
    chemical toxins. The oyster reef habitat is an irreplaceable coastal aquatic resource."
  } else if(habitat=="Coral Reef"){
    t <- "Coral reefs are formed by colonies of small invertebrate animals that make 
    rigid skeletons of calcium carbonate. Although relatively slow-growing, 
    coral reefs can grow quite large and form dynamic and diverse habitat structures 
    for other organisms. Most corals thrive in relatively warm, clear waters with 
    low nutrient concentrations, moderate wave action, and currents that bring oxygen 
    as well as plankton to the reef. Reef-building, or stony, corals depend for their 
    livelihood on a fragile relationship with an alga called zooxanthelle that trades 
    photosynthetic nutrients and amino acids for coral-produced carbon dioxide and water. 
    The critical symbiosis between corals and algae is easily upset by changes in water 
    chemistry and clarity, as well as the characteristics of other coral species on the reef."
  } else if(habitat=="Coastal Wetlands"){
    t <- "Coastal wetlands are critical transition zones between freshwater and 
    marine environments that provide essential habitat for fish, crustaceans and 
    coastal birds. There are two types of coastal wetlands in Florida: mangrove 
    swamps harboring one to four mangrove tree species, and salt marshes dominated 
    by grasses, rushes and sedges. Salt marshes and mangrove forests have extensive 
    root systems that enable them to withstand brief storm surges, buffering the 
    impact to upland areas. Wetlands also prevent erosion and sediments from washing 
    offshore while filtering nutrients, sediments and toxic contaminants."
  } else if(habitat=="Nekton"){
    t <- "Nekton refers to the free-swimming animals living in the water column. 
    Among many inhabitants are fish, macroinvertebrates like shrimp, octopus, and 
    jellyfish and megafauna like dolphins and manatees."
  }
  return(t)
}


### Saving for potential use later

## Habitat badges containing images from SEACAR Atlas in navbar form
# habitatBadges <- function(){
#   navset_card_tab(
#     nav_panel(div(img(src="sav_badge.png")),
#               "SAV Content"),
#     nav_panel(div(img(src="wc_badge.png")),
#               "WC Content",),
#     nav_panel(div(img(src="coral_badge.png")),
#               "Coral Content"),
#     nav_panel(div(img(src="oyster_badge.png")),
#               "Oyster Content"),
#     nav_panel(div(img(src="cw_badge.png")),
#               "CW Content")        
#   )
# }

## Generic Navbar
# habitatContent <- function(){
#   navset_card_tab(id="habInfo",
#     nav_panel("Submerged Aquatic Vegetation",
#               savContent(),
#               value = "Submerged Aquatic Vegetation"),
#     nav_panel("Nekton",
#               "Nekton Content",
#               value = "Nekton"),
#     nav_panel("Coral Reef",
#               "Coral Content",
#               value = "Coral Reef"),
#     nav_panel("Oyster Reef",
#               "Oyster Content",
#               value = "Oyster Reef"),
#     nav_panel("Coastal Wetlands",
#               "CW Content",
#               value = "Coastal Wetlands")
#   )
# }