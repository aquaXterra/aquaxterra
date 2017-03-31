# Bird categorization into functional groups by diet and foraging traits

btrait <- read.csv('C:/Users/Q/Dropbox/projects/aquaxterra/birdtraitmerged.csv', stringsAsFactors = FALSE)

# waterbirds versus not
btrait <- transform(btrait, waterbird = ForStrat.watbelowsurf > 0 | ForStrat.wataroundsurf > 0 | btrait$PelagicSpecialist == 1)

table(btrait$waterbird, btrait$Diet.5Cat)

btrait$English_Common_Name[btrait$waterbird]
btrait$English_Common_Name[btrait$Diet.5Cat == 'Omnivore']
btrait$English_Common_Name[btrait$Diet.5Cat == 'VertFishScav']
btrait$English_Common_Name[btrait$Diet.5Cat == 'FruiNect']

