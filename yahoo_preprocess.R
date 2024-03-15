#' Accompanying code for "Estimating causal effects when treatments are entangled by network dynamics".
#' See README_entanglement.pdf for details.
#' 
#' INFO: This file should be placed in the root directory of the Yahoo Go data. This root directly is typically named 
#'   "Yahoo" and contains README files in .docx and .pdf format, and a folder named "dataset" containing all files.

#' To pre-process the data and prepare them for yahoo.R simply source this file.
rm(list=ls())

im1 = read.csv("dataset/netuser_im_step2_01_07.dat", header=F, sep="\t")
im2 = read.csv("dataset/netuser_im_step2_08_14.dat", header=F, sep="\t")
im3 = read.csv("dataset/netuser_im_step2_15_21.dat", header=F, sep="\t")
im4 = read.csv("dataset/netuser_im_step2_22_28.dat", header=F, sep="\t")

colnames(im3) = c("date", "egoid", "alterid", "activity_code", "count")
colnames(im4) = c("date", "egoid", "alterid", "activity_code", "count")
im3$date = NULL
im4$date = NULL
save(im3, file="dataset/simple/im_15_21.rda")
save(im4, file="dataset/simple/im_22_28.rda")
rm(im3)
rm(im4)
# colnames(im) = c("date", "egoid", "alterid", "activity_code", "count")
load("dataset/simple/im_08_14.rda")

user = read.csv("dataset/netuser_userdata.dat", header=F, sep="\t")
colnames(user) = c("userid", "sysid", "listid", "priorgo", "hasreg", "gender", "ageyr", "country")

head(user)
class(user)

user$sysid = NULL
user$listid = NULL

save(user, file="dataset/simple/user.rda")
rm(user)

go1 = read.csv("dataset/netuser_ygo_01_07.dat", header=F, sep="\t")
go2 = read.csv("dataset/netuser_ygo_08_14.dat", header=F, sep="\t")
go3 = read.csv("dataset/netuser_ygo_15_21.dat", header=F, sep="\t")
go4 = read.csv("dataset/netuser_ygo_22_28.dat", header=F, sep="\t")

colnames(go1) = c("date", "userid", "firstgo", "devid", "pctpv", "gopv", "fppv", "mailpv",
                  "srchpv", "wthrpv", "newspv", "finpv", "sportspv", "flickrpv")
colnames(go2) = c("date", "userid", "firstgo", "devid", "pctpv", "gopv", "fppv", "mailpv",
                  "srchpv", "wthrpv", "newspv", "finpv", "sportspv", "flickrpv")
colnames(go3) = c("date", "userid", "firstgo", "devid", "pctpv", "gopv", "fppv", "mailpv",
                  "srchpv", "wthrpv", "newspv", "finpv", "sportspv", "flickrpv")
colnames(go4) = c("date", "userid", "firstgo", "devid", "pctpv", "gopv", "fppv", "mailpv",
                  "srchpv", "wthrpv", "newspv", "finpv", "sportspv", "flickrpv")
go = rbind(go1, go2, go3, go4)
save(go, file="dataset/simple/go.rda")

load("dataset/simple/user.rda")


mob1 = read.csv("dataset/netuser_mobile_01_07.dat.gz", header=F, sep="\t")
colnames(mob1) =  c("date", "userid", "devid", "pctpv", "mwpv", "fppv", "mailpv",
                    "impv", "srchpv", "wthrpv", "newspv", "finpv", "sportspv", "flickrpv")
mob2 = read.csv("dataset/netuser_mobile_08_14.dat.gz", header=F, sep="\t")
colnames(mob2) =  c("date", "userid", "devid", "pctpv", "mwpv", "fppv", "mailpv",
                    "impv", "srchpv", "wthrpv", "newspv", "finpv", "sportspv", "flickrpv")
mob3 = read.csv("dataset/netuser_mobile_15_21.dat.gz", header=F, sep="\t")
colnames(mob3) =  c("date", "userid", "devid", "pctpv", "mwpv", "fppv", "mailpv",
                    "impv", "srchpv", "wthrpv", "newspv", "finpv", "sportspv", "flickrpv")
mob4 = read.csv("dataset/netuser_mobile_22_28.dat.gz", header=F, sep="\t")
colnames(mob4) =  c("date", "userid", "devid", "pctpv", "mwpv", "fppv", "mailpv",
                    "impv", "srchpv", "wthrpv", "newspv", "finpv", "sportspv", "flickrpv")

mob = rbind(mob1, mob2, mob3, mob4)
head(mob)


pc1 = read.csv("dataset/netuser_otherprop_01_07.dat", header=F, sep="\t")
colnames(pc1) = c("date", "userid", "wthrpv", "newspv" ,"finpv", "sportspv", "flickrpv")
head(pc1)

