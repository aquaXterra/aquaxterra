bbs_bystop <- read.csv('bbs_div_reduced2015.csv')
bbs_byroute <- read.csv('bbs_div_byroute.csv')
bbs_coord_bystop <- read.csv('bbs_wgs84_coords.csv')
bbs_coord_byroute <- read.csv('bbs_wgs84_coords_byroute.csv')

# Replace the wgs72 coordinates with the wgs84 coordinates.
bbs_bystop <- cbind(bbs_coord_bystop, bbs_bystop[,1:17])
bbs_byroute <- cbind(bbs_coord_byroute, bbs_byroute[,1:28])

write.csv(bbs_bystop, file = '/mnt/ls15/scratch/groups/plz-lab/BBS/bbs_bystop.csv', row.names = FALSE)
write.csv(bbs_byroute, file = '/mnt/ls15/scratch/groups/plz-lab/BBS/bbs_byroute.csv', row.names = FALSE)
